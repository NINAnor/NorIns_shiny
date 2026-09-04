require(leaflet)
require(leaflet.minicharts)
require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(Norimon)
require(shinyvalidate)
require(shinyjs)

asvmap_ui <- function(id) {
  ns <- NS(id)

  useShinyjs()

  tabPanel(
    title = "Innenartsvariasjon",
    column(
      6,
      shinydashboardPlus::box(
        id = "taxabox",
        width = 12,
        title = "Genetisk variasjon innen arter",
        textOutput(ns("asvmap_text")),
        uiOutput(ns("choose_project")),
        height = "500px"
      ),
      shinydashboardPlus::box(
        width = 12,
        id = "speciesbox",
        title = "Artssøk",
        fluidRow(
          column(
            6,
            uiOutput(ns("choose_conf")),
            uiOutput(ns("choose_order")),
            uiOutput(ns("choose_fam"))
          ),
          column(
            6,
            uiOutput(ns("choose_spec")),
             selectizeInput(
               inputId = ns("species_filter"),
               label = "Fritekst",
               choices = NULL,
               selected = NULL
             ),
            actionButton(ns("filter_btn"),
              label = "Fritekssøk"
            ),
            actionButton(ns("filter_clear_btn"),
              label = "Rens fritext"
            )
          )
        ),
        height = "400px"
      )
    ),
    column(
      6,
      shinydashboardPlus::box(
        width = 12,
        id = "asv_leaflet_box",
        title = "Fordeling av genetiske varianter",
        shinycssloaders::withSpinner(
          {
            leaflet::leafletOutput(ns("asv_map"),
              width = "95%",
              height = 800
            )
          },
          type = 2,
          color = "#E57200",
          color.background = "#004F71"
        ),
        height = "800px"
      )
    )
  )
}




asvmap_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(a = 1)

    
    output$choose_project <- renderUI({
      #con <- login_import$con()

      selectInput(
        inputId = ns("project"),
        label = "Prosjekt",
        choices = c("Norsk insektovervåking" = "NorIns",
                    "Tidlig varsling av fremmede arter" = "TidVar"),
        # choices = c("", projects$project_name),
        selected = "Norsk insektovervåking",
        selectize = FALSE
      )
    })
    
    
    loc_species_list <- tbl(
      login_import$con, 
      Id(
        schema = "views",
        table = "loc_species_list"
      )
    )

    output$choose_conf <- renderUI({
      input$filter_btn

      conf_choices <- list(
        "Høy" = "HIGH",
        "Moderat" = "MODERATE",
        "Lav" = "LOW",
        "Dårlig" = "POOR",
        "Alle" = "ALL"
      )

      species_filter <- isolate(input$species_filter)
      if (species_filter == "" || is.null(species_filter) || species_filter == "Ingen") {
        selectInput(
          inputId = ns("sel_conf"),
          label = "Sikkerhet på artsbestemmelse",
          choices = conf_choices,
          selected = "HIGH"
        )
      } else {
        selectInput(
          inputId = ns("sel_conf"),
          label = "Sikkerhet på artsbestemmelse",
          choices = conf_choices,
          selected = species_filter_out()$identification_confidence
        )
      }
    })


    output$choose_order <- renderUI({
      req(input$project)
      input$filter_btn
      # Assign to higher environment, to not require again
      #con <<- login_import$con

      if (input$sel_conf != "ALL") {
      order_choices_q <- "
        SELECT sl.id_order, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
        FROM
        (SELECT distinct id_order as id_order
        from views.species_list
        WHERE id_order IS NOT NULL
        AND identification_confidence = ?id1
        AND project_short_name = ?id2) sl LEFT JOIN

        (SELECT orden,
         populaernavn_bokmaal
         FROM
        lookup.artsnavnebasen
        WHERE underorden IS NULL
        AND overfamilie IS NULL
        AND familie IS NULL
        ) names
        ON sl.id_order = names.orden
        ORDER BY id_order

      "

      order_choices_san <- sqlInterpolate(
        login_import$con,
        order_choices_q,
        id1 = input$sel_conf,
        id2 = input$project
      )

      order_choices_raw <- dbGetQuery(
        login_import$con,
        order_choices_san
      )
      } else {
        order_choices_q <- "
        SELECT sl.id_order, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
        FROM
        (SELECT distinct id_order as id_order
        from views.species_list
        WHERE id_order IS NOT NULL
        AND project_short_name = ?id2) sl LEFT JOIN

        (SELECT orden,
         populaernavn_bokmaal
         FROM
        lookup.artsnavnebasen
        WHERE underorden IS NULL
        AND overfamilie IS NULL
        AND familie IS NULL
        ) names
        ON sl.id_order = names.orden
        ORDER BY id_order
      "
        
        order_choices_san <- sqlInterpolate(
          login_import$con,
          order_choices_q,
          id2 = input$project
        )
        
        order_choices_raw <- dbGetQuery(
          login_import$con,
          order_choices_san
        )
        
      }

      order_choices_list <- as.list(order_choices_raw$id_order)

      if (length(order_choices_list) > 0) {
        names(order_choices_list) <- paste0(order_choices_raw$id_order, " - ", order_choices_raw$bokmal)
      }

      species_filter <- isolate(input$species_filter)
      if (species_filter == "" || is.null(species_filter) || species_filter == "Ingen") {
        selectInput(
          inputId = ns("sel_order"),
          label = "Velg orden",
          choices = order_choices_list,
          selected = "Blattodea"
        )
      } else {
        selectInput(
          inputId = ns("sel_order"),
          label = "Velg orden",
          choices = order_choices_list,
          selected = species_filter_out()$id_order
        )
      }
    })



    output$choose_fam <- renderUI({
      input$filter_btn
      #con <- login_import$con()

      req(input$sel_order)

      if (input$sel_conf != "ALL") {
        
      family_choices_q <- "
                SELECT sl.id_family, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
                FROM
                (SELECT distinct id_family as id_family
                from views.species_list
                WHERE id_family IS NOT NULL
        		    AND id_order = ?id1
        		    AND identification_confidence = ?id2
        		    AND project_short_name = ?id3) sl LEFT JOIN

                (SELECT familie,
                 populaernavn_bokmaal
                 FROM
                lookup.artsnavnebasen
                WHERE familie IS NOT NULL
        		    AND underfamilie IS NULL
                AND tribus IS NULL
        		    AND undertribus IS NULL
                AND slekt IS NULL
        		    AND orden = ?id1
                ) names
                ON sl.id_family = names.familie
                ORDER BY id_family
      "

      family_choice_san <- sqlInterpolate(login_import$con,
        family_choices_q,
        id1 = input$sel_order,
        id2 = input$sel_conf,
        id3 = input$project
      )

      family_choices_raw <- dbGetQuery(
        login_import$con,
        family_choice_san
      )
      } else {
       
        family_choices_q <- "
                SELECT sl.id_family, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
                FROM
                (SELECT distinct id_family as id_family
                from views.species_list
                WHERE id_family IS NOT NULL
        		    AND id_order = ?id1
        		    AND project_short_name = ?id2) sl LEFT JOIN

                (SELECT familie,
                 populaernavn_bokmaal
                 FROM
                lookup.artsnavnebasen
                WHERE familie IS NOT NULL
        		    AND underfamilie IS NULL
                AND tribus IS NULL
        		    AND undertribus IS NULL
                AND slekt IS NULL
        		    AND orden = ?id1
                ) names
                ON sl.id_family = names.familie
                ORDER BY id_family
      "
        
        family_choice_san <- sqlInterpolate(login_import$con,
                                            family_choices_q,
                                            id1 = input$sel_order,
                                            id2 = input$project
        )
        
        family_choices_raw <- dbGetQuery(
          login_import$con,
          family_choice_san
        ) 
        
      }
      
      family_choices_list <- as.list(family_choices_raw$id_family)

      if (length(family_choices_list) > 0) {
        names(family_choices_list) <- paste0(family_choices_raw$id_family, " - ", family_choices_raw$bokmal)
      }

      species_filter <- isolate(input$species_filter)
      if (species_filter == "" || is.null(species_filter) || species_filter == "Ingen") {
        selectInput(
          inputId = ns("sel_fam"),
          label = "Velg familie",
          choices = family_choices_list,
          selected = ""
        )
      } else {
        selectInput(
          inputId = ns("sel_fam"),
          label = "Velg familie",
          choices = family_choices_list,
          selected = species_filter_out()$id_family
        )
      }
    })




    output$choose_spec <- renderUI({
      req(input$sel_order)
      req(input$sel_fam)
      req(input$sel_conf)

      input$filter_btn
      #con <- login_import$con()


      if (input$sel_conf != "ALL") {
        species_choices_q <- "
          SELECT sl.species_latin_gbif, sl.id_genus, sl.id_species, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
        FROM
        (SELECT distinct on(species_latin_gbif)
    		 id_genus,
    		 id_species,
    		 species_latin_gbif
         from views.species_list
         WHERE id_genus IS NOT NULL
      	 AND id_species IS NOT NULL
      	 AND id_order = ?id1
         AND id_family = ?id2
         AND identification_confidence = ?id3
         AND project_short_name = ?id4) sl LEFT JOIN

        (SELECT *
         FROM
        lookup.artsnavnebasen
        WHERE slekt IS NOT NULL
    		AND art IS NOT NULL
    		AND orden = ?id1
    		AND familie = ?id2
        ) names
        ON sl.id_genus = names.slekt
		    AND sl.id_species = names.art
        ORDER BY species_latin_gbif
        "

        species_choice_san <- sqlInterpolate(login_import$con,
          species_choices_q,
          id1 = input$sel_order,
          id2 = input$sel_fam,
          id3 = input$sel_conf,
          id4 = input$project
        )

        species_choices_raw <- dbGetQuery(
          login_import$con,
          species_choice_san
        )
      } else {
        species_choices_q <- "
          SELECT sl.species_latin_gbif, sl.id_genus, sl.id_species, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
        FROM
        (SELECT distinct on(species_latin_gbif)
    		 id_genus,
    		 id_species,
    		 species_latin_gbif
         from views.species_list
         WHERE id_genus IS NOT NULL
      	 AND id_species IS NOT NULL
      	 AND id_order = ?id1
         AND id_family = ?id2
         AND project_short_name = ?id3) sl LEFT JOIN

        (SELECT *
         FROM
        lookup.artsnavnebasen
        WHERE slekt IS NOT NULL
    		AND art IS NOT NULL
    		AND orden = ?id1
    		AND familie = ?id2
        ) names
        ON sl.id_genus = names.slekt
		    AND sl.id_species = names.art
        ORDER BY species_latin_gbif
        "

        species_choice_san <- sqlInterpolate(login_import$con,
          species_choices_q,
          id1 = input$sel_order,
          id2 = input$sel_fam,
          id3 = input$project
        )

        species_choices_raw <- dbGetQuery(
          login_import$con,
          species_choice_san
        )
      }


      species_choices_list <- as.list(species_choices_raw$species_latin_gbif)

      if (length(species_choices_list) > 0) {
        names(species_choices_list) <- paste0(species_choices_raw$species_latin_gbif, " - ", species_choices_raw$bokmal)
      }

      species_filter <- isolate(input$species_filter)
      if (species_filter == "" || is.null(species_filter) || species_filter == "Ingen") {
        selectInput(ns("asv_species"),
          label = "Velg art fra familie",
          choices = c(species_choices_list, ""),
          # selected = "",
          selectize = TRUE
        )
      } else {
        selectInput(ns("asv_species"),
          label = "Velg art fra familie",
          choices = c(species_choices_list, ""),
          selected = species_filter_out()$species_latin_gbif,
          selectize = TRUE
        )
      }
    })



    basemap <- leaflet(
      width = "300px",
      height = "200px"
    ) |>
      addTiles(group = "OpenStreetMap")





    # species_choices <- function() {
    #   loc_species_list <- tbl({}
    #     con, ## Needs to be loaded into environment, here done by <<- earlier
    #     Id(
    #       schema = "views",
    #       table = "loc_species_list"
    #     )
    #   )
    # 
    # 
    #   species_choices <- loc_species_list  |> 
    #     filter(project_short_name == !!selected_project()) |> 
    #     select(species_latin_gbif)  |> 
    #     distinct() |> 
    #     arrange(species_latin_gbif) |>
    #     pull()
    # 
    #   return(species_choices)
    # }


  observeEvent(input$project,
               {
  
                 species_choices <- loc_species_list  |> 
                   filter(project_short_name == input$project) |> 
                   select(species_latin_gbif)  |> 
                   distinct() |> 
                   arrange(species_latin_gbif) |>
                   pull()
                 
                 updateSelectizeInput(
                   inputId = "species_filter",
                   choices = c("Ingen", species_choices),
                   selected = "Ingen",
                   server = TRUE,
                   options = list(maxOptions = 10)
                 )
               },
               ignoreNULL = TRUE,
               ignoreInit = FALSE
    
  )



    observeEvent(input$filter_clear_btn,
      {
        
        species_choices <- loc_species_list  |> 
          filter(project_short_name == input$project) |> 
          select(species_latin_gbif)  |> 
          distinct() |> 
          arrange(species_latin_gbif) |>
          pull()
        
        updateSelectizeInput(
          inputId = "species_filter",
          choices = c("Ingen", species_choices),
          selected = "Ingen",
          server = TRUE,
          options = list(maxOptions = 10)
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    species_filter_out <- reactive({
      req(input$project)
      if (input$species_filter != "Ingen") {
        #con <- login_import$con()

        taxa_reverse_q <- "
        SELECT *
        FROM views.species_list
        WHERE species_latin_gbif = ?id1
        AND project_short_name = ?id2
        "

        taxa_reverse_sql <- sqlInterpolate(login_import$con,
          taxa_reverse_q,
          id1 = input$species_filter,
          id2 = input$project
        )

        taxa_reverse_res <- dbGetQuery(
          login_import$con,
          taxa_reverse_sql
        )
      } else {
        taxa_reverse_res <- tibble("species_latin_gbif" = "Ingen")
      }

      return(taxa_reverse_res)
    })


    
    selected_project <- reactive({
      if (is.na(input$project)) {
        return(NULL)
      } else {
        project <- input$project
 
        return(project)
      }
    })
    
    
    selected_species <- reactive({
      if (is.na(input$asv_species)) {
        return(NULL)
      } else {
        # if(input$species_filter == "Ingen"){
        species <- input$asv_species
        # } else {

        # species <- input$species_filter
        # }

        return(species)
      }
    })
    
    sel_asv <- reactive({
    asv_perc_min_ind <- tbl(
      login_import$con,
      Id(schema = "views",
         table = "asv_perc_min_ind")
    )
    
    sel_asv <- asv_perc_min_ind |>
      filter(species_latin_gbif == !!selected_species(),
             project_short_name == !!selected_project()) |>
      collect() |>
      mutate(seq_short = stringr::str_sub(sequence_id,
                                                  start = 1,
                                                  end = 8),
             asv = as_factor(sequence_id),
             perc_min_no_ind = round(perc_min_no_ind, 3)
      ) |>
      arrange(sequence_id)
    
    return(sel_asv)
    })
    
    
    asv_to_leaflet <- reactive({

      to_plot <- sel_asv() |>
        select(
          locality,
          lat,
          lon,
          seq_short,
          min_no_ind,
          sum_min_no_ind,
          #perc_min_no_ind,
          max_possible_no_ind
        ) |>
        distinct() |> # This is a workaround for multiple records in genetics.asv_sequences for the same sequence_id. Should only be one species_latin_fixed per sequence_id!
        pivot_wider(
          names_from = "seq_short",
          values_from = "min_no_ind",
          names_prefix = "seq_",
          values_fill = 0
        )

      return(to_plot)
    })


    asv_colors <- function(x){
      ramp_fun <- colorRamp(c(ninaColors("dark blue"), ninaColors("green"),  ninaColors("yellow")),
                            bias = 5)   
      
      rgb(ramp_fun(x), maxColorValue = 255)
      
    }
    
    custom_colors <- reactive({ 
    
    custom_colors <- sel_asv() |> 
      mutate(seq_short = paste0("seq_", seq_short)) |> 
       select(seq_short,
              color_val) |> 
      distinct() |> 
      mutate(custom_col = asv_colors(color_val)) |> 
      select(seq_short,
             custom_col) 
    
    return(custom_colors)
    })
      
    custom_popups <- reactive({
      to_plot <-  asv_to_leaflet()
      
      res <- apply(to_plot, 1, function(row) {
        max_val <- row["max_possible_no_ind"]
        
        seq_mask <- grepl("^seq_", names(row))
        vals <- as.numeric(row[seq_mask])
        names(vals) <- names(row)[seq_mask]
        
        non_zero_filt <- vals[!is.na(vals) & vals > 0]
        non_zero_filt <- head(non_zero_filt[order(as.numeric(non_zero_filt), decreasing = TRUE)],
             10)
        
        if (length(non_zero_filt) == 0) {
          items <- "<i>No Data</i>"
        } else {
          # 1. Match names(non_zero_filt) to seq_colors$seq_short to get corresponding colors
          
          seq_colors <- custom_colors()
          col_matches <- seq_colors$custom_col[match(names(non_zero_filt), seq_colors$seq_short)]
          
          # Optional fallback (e.g., "#000000" or "black") if a sequence isn't found in your color table
          col_matches[is.na(col_matches)] <- "black"
          
          # 2. Wrap each sequence name in an HTML <span> with the matched inline color
          items <- paste0(
            "<span style='color: ", col_matches, "; font-weight: bold;'>", 
            names(non_zero_filt), 
            ":</span> ", 
            non_zero_filt, 
            collapse = "<br>"
          )
        }
        
        # 3. Assemble final popup
        popup <- paste0(
          "<b>Observed out of ", max_val, "<br> possible times:(top ten seq.)</b> ", "<br>",
          "<hr style='margin: 4px 0;'>",
          items
        )
      })
    })
    
    
    output$asv_map <- renderLeaflet({
      req(input$asv_species)
      # req(input$species_filter)

      to_plot <- asv_to_leaflet()
      if(nrow(to_plot) == 0) return(NULL)
      
      chart_data <- to_plot[, which(grepl("seq_", names(to_plot)))]
      
      basemap |>
        leaflet::addProviderTiles(providers$Esri.WorldImagery,
          group = "Ortophoto"
        ) |>
        leaflet::addProviderTiles(providers$OpenTopoMap,
          group = "Topo"
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::hideGroup(c("Topo", "Ortophoto")) |>
        addMinicharts(to_plot$lon,
          to_plot$lat,
          type = "pie",
          chartdata = chart_data,
          width = log(to_plot$sum_min_no_ind) * 10,
          legend = FALSE,
          #popup = list(noPopup = TRUE),
          colorPalette = custom_colors()$custom_col,
           popup = list(html = custom_popups(),
                        showValues = FALSE, # Disables default JS table generator
                        showTitle = FALSE   # Hides auto-generated layer ID title
                        )
          ) |> 
        addLegend(
          position = "bottomright",
          colors = asv_colors(seq(from = 0, to = 1, by = 0.25)),
          labels = round(seq(from = 0, to = 1, by = 0.25), 2),
          title = "Genetic variants 0-1",
          opacity = 1
        )
    })

    output$asvmap_text <- renderText("Kartet til høyre viser funnstedet for enkelte arter og kakediagrammene representerer komposisjonen av genetiske varianter innen hver art. Hver farge representerer en spesifikk genetisk variant. Størrelsen på sirklene er skalert etter hvor mange DNA-sekvenser vi totalt har funnet av arten i en lokalitet, og størrelsen på kakebitene viser hvor stor del av disse en gitt genetisk variant står for.

Nedenfor kan man søke på enkeltarter.
Per i dag har overvåkingsprogrammet et begrenset geografisk og tidsmessig omfang. Dataene for arter som er observert med få individer på få steder vil være mer tilfeldige enn arter med mange individer fanget på mange steder. Sikkerhet på artsbestemmelse angir usikkerheten knyttet til den automatiske artsidentifiseringen med DNA. De fleste funn er ikke gjennomgått manuelt og det kan være feil i artsnavn selv om vi angir sikkerheten som høy.")
  })
}
