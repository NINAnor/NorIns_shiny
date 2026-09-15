require(leaflet)
#require(leaflet.minicharts)
require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(Norimon)
require(shinyvalidate)
require(shinyjs)
require(sf)
require(leafgl)



asvmap_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Innenartsvariasjon",
    useShinyjs(),
    

    column(
      6,
      shinydashboardPlus::box(
        id = "taxabox",
        width = 12,
        title = "Genetisk variasjon innen arter",
        htmlOutput(ns("asvmap_text")),
        uiOutput(ns("choose_project")),
        height = "500px"
      ),
      shinydashboardPlus::box(
        width = 12,
        id = "map_choices_box",
        title = "Kartinstillinger",
        fluidRow(
          column(
            6,
            selectInput(
              ns("color_mode"),
              label = "Fargelegg basert på",
              choices = c("Genetisk avstand", "Tilfeldige farger", "Total diversitet per lok."),
              selected = "Genetisk avstand"
            )
          ),
          column(
            6,
            sliderTextInput(
              ns("pie_size"),
              label = "Kakestørrelse (0 = 500m)",
              choices = c(0, 1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 150, 200),
              #step = 5,
              selected = 10
            )
          )
        )
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

# asvmap_ui <- function(id) {
#   ns <- NS(id)
# 
#   useShinyjs()
#   
#   tabPanel(
#     title = "Innenartsvariasjon",
#     column(
#       6,
#       shinydashboardPlus::box(
#         id = "taxabox",
#         width = 12,
#         title = "Genetisk variasjon innen arter",
#         textOutput(ns("asvmap_text")),
#         uiOutput(ns("choose_project")),
#         height = "500px"
#       ),
#       shinydashboardPlus::box(
#         width = 12,
#         id = "map_choices_box",
#         title = "Kartinstillinger",
#         fluidRow(
#           column(6,
#                  uiOutput(ns("choose_color_mode"))
#                  ),
#           column(6,
#                  uiOutput(ns("choose_pie_size"))
#                  )
#         )
#       ),
#       shinydashboardPlus::box(
#         width = 12,
#         id = "speciesbox",
#         title = "Artssøk",
#         fluidRow(
#           column(
#             6,
#             uiOutput(ns("choose_conf")),
#             uiOutput(ns("choose_order")),
#             uiOutput(ns("choose_fam"))
#           ),
#           column(
#             6,
#             uiOutput(ns("choose_spec")),
#              selectizeInput(
#                inputId = ns("species_filter"),
#                label = "Fritekst",
#                choices = NULL,
#                selected = NULL
#              ),
#             actionButton(ns("filter_btn"),
#               label = "Fritekssøk"
#             ),
#             actionButton(ns("filter_clear_btn"),
#               label = "Rens fritext"
#             )
#           )
#         ),
#         height = "400px"
#       )
#     ),
#     column(
#       6,
#       shinydashboardPlus::box(
#         width = 12,
#         id = "asv_leaflet_box",
#         title = "Fordeling av genetiske varianter",
#         shinycssloaders::withSpinner(
#           {
#             leaflet::leafletOutput(ns("asv_map"),
#               width = "95%",
#               height = 800
#             )
#           },
#           type = 2,
#           color = "#E57200",
#           color.background = "#004F71"
#         ),
#         height = "800px"
#       )
#     )
#   )
# }




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


    # output$choose_color_mode <- renderUI({
    #   selectInput(ns("color_mode"),
    #               label = "Fargelegg basert på",
    #               choices = c("Genetisk avstand", 
    #                           "Tilfeldige farger"),
    #               selected = "Genetisk avstand")
    # })

    basemap <- leaflet(
      width = "300px",
      height = "200px"
    ) |>
      addTiles(group = "OpenStreetMap")

  # output$choose_pie_size <- renderUI({
  #   
  #   sliderInput(ns("pie_size"),
  #               label = "Kakestørrelse",
  #               min = 10,
  #               max = 80,
  #               step = 10,
  #               value = 30)
  # })

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
          most_common_min_no_ind,
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
      # ramp_fun <- colorRamp(c(ninaColors("dark blue"), ninaColors("green"),  ninaColors("purple")),
      #                       bias = 5)
      ramp_fun <- colorRamp(c(ninaColors("yellow"), ninaColors("blue"),  ninaColors("purple")),
                            bias = 1) 
      
      rgb(ramp_fun(x), maxColorValue = 255)
      
    }
    
    custom_colors <- function(df,
                              type){ 
      
      type <- match.arg(type,
                        choices = c("asv_color",
                                    "loc_color"))
      
      scale_01 <- function(x) {
        (x - min(x)) / (max(x) - min(x))
      }
    
    if(type == "asv_color"){  
    res <- df |> 
     # mutate(seq_short = paste0("seq_", seq_short)) |> 
       select(seq_short,
              color_val) |> 
      distinct() |> 
      mutate(custom_col = asv_colors(color_val)) |> 
      select(seq_short,
             custom_col) } else 
    if(type == "loc_color"){
        res <- df |> 
          group_by(species_latin_fixed,
                   locality) |> 
          mutate(loc_color_val = n_distinct(sequence_id)) |>
          group_by(species_latin_fixed) |> 
          mutate(loc_color_val = scale_01(loc_color_val),
                 .groups = "keep") |> 
          ungroup() |> 
          select(locality,
                 loc_color_val) |> 
          distinct() |> 
          mutate(custom_col = asv_colors(loc_color_val)) |> 
          select(locality,
                 custom_col)
      }
    
    return(res)
    }
      
    # custom_popups <- reactive({
    #   to_plot <-  asv_to_leaflet()
    #   
    #   res <- apply(to_plot, 1, function(row) {
    #     max_val <- row["max_possible_no_ind"]
    #     locality <- row["locality"]
    #     
    #     seq_mask <- grepl("^seq_", names(row))
    #     vals <- as.numeric(row[seq_mask])
    #     names(vals) <- names(row)[seq_mask]
    #     
    #     non_zero_filt <- vals[!is.na(vals) & vals > 0]
    #     non_zero_filt <- head(non_zero_filt[order(as.numeric(non_zero_filt), decreasing = TRUE)],
    #          10)
    #     
    #     if (length(non_zero_filt) == 0) {
    #       items <- "<i>No Data</i>"
    #     } else {
    #       # 1. Match names(non_zero_filt) to seq_colors$seq_short to get corresponding colors
    #       
    #       seq_colors <- custom_colors()
    #       col_matches <- seq_colors$custom_col[match(names(non_zero_filt), seq_colors$seq_short)]
    #       
    #       # Optional fallback (e.g., "#000000" or "black") if a sequence isn't found in your color table
    #       col_matches[is.na(col_matches)] <- "black"
    #       
    #       # 2. Wrap each sequence name in an HTML <span> with the matched inline color
    #       items <- paste0(
    #         "<span style='color: ", col_matches, "; font-weight: bold;'>", 
    #         names(non_zero_filt), 
    #         ":</span> ", 
    #         non_zero_filt, 
    #         collapse = "<br>"
    #       )
    #     }
    #     
    #     # 3. Assemble final popup
    #     popup <- paste0(
    #       "<b>Observed out of ", max_val, "<br> possible times in ", locality , "<br> (showing top ten seq.)</b> ", "<br>",
    #       "<hr style='margin: 4px 0;'>",
    #       items
    #     )
    #   })
    # })
    

    

    prepare_spatial_pie_slices <- function(df, 
                                           pie_scale_factor = 5, 
                                           base_radius_m = 250,
                                           aggregation_level = c("slices", "pie")) {
      
      aggregation_level <- match.arg(aggregation_level)
      
      # 1. Ensure data is filtered and grouped
      df_filtered <- df %>%
        filter(!is.na(lat), !is.na(lon), perc_min_no_ind > 0)
      
      # Return empty sf if no data matches criteria
      if (nrow(df_filtered) == 0) {
        return(st_sf(geometry = st_sfc(crs = 4326)))
      }
      
      if (aggregation_level == "pie") {
        # ----------------------------------------------------
        # AGGREGATION LEVEL: PIE (Single circular polygon per locality)
        # ----------------------------------------------------
        df_processed <- df_filtered %>%
          group_by(locality_id, locality, species_latin_fixed, lat, lon) %>%
          summarise(
            sum_min_no_ind = max(sum_min_no_ind, na.rm = TRUE),
            max_possible_no_ind = max(max_possible_no_ind, na.rm = TRUE),
            calc_ratio = max(sum_min_no_ind / max_possible_no_ind, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            radius_m = pmax(calc_ratio * pie_scale_factor * 250, base_radius_m)
          )
        
        polys_list <- vector("list", nrow(df_processed))
        
        for (i in seq_len(nrow(df_processed))) {
          row <- df_processed[i, ]
          
          # Full circle from 0 to 2*pi
          theta <- seq(0, 2 * pi, length.out = 60)
          
          lat_rad <- row$lat * pi / 180
          meters_per_deg_lat <- 111139
          meters_per_deg_lon <- 111139 * cos(lat_rad)
          
          dx <- row$radius_m * sin(theta) / meters_per_deg_lon
          dy <- row$radius_m * cos(theta) / meters_per_deg_lat
          
          # Circle boundary closed back to origin point
          coords <- cbind(row$lon + dx, row$lat + dy)
          coords <- rbind(coords, coords[1, ])
          
          polys_list[[i]] <- st_polygon(list(coords))
        }
        
        sf_out <- st_sf(
          df_processed %>% 
            mutate(seq_short = "alle",
                   perc_min_no_ind = 1,
                   min_no_ind = sum_min_no_ind) |> 
            select(locality_id, 
                   locality, 
                   species_latin_fixed, 
                   seq_short,
                   min_no_ind,
                   sum_min_no_ind,
                   perc_min_no_ind, 
                   max_possible_no_ind,
                   radius_m),
          geometry = st_sfc(polys_list, crs = 4326)
        )
        
      } else {
        # ----------------------------------------------------
        # AGGREGATION LEVEL: SLICES (Individual pie slice polygons)
        # ----------------------------------------------------
        df_processed <- df_filtered %>%
          group_by(locality_id, lat, lon) %>%
          mutate(
            # shares = perc_min_no_ind,
            # end_angle = 2 * pi * cumsum(perc_min_no_ind),
            # start_angle = lag(end_angle, default = 0),
            # calc_ratio = sum_min_no_ind / max_possible_no_ind,
            # radius_m = pmax(calc_ratio * pie_scale_factor * 250, base_radius_m
            shares = min_no_ind / sum(min_no_ind),
            end_angle = 2 * pi * cumsum(shares),
            start_angle = lag(end_angle, default = 0),
            calc_ratio = sum_min_no_ind / max_possible_no_ind,
            radius_m = pmax(calc_ratio * pie_scale_factor * 250, base_radius_m
            )
          ) %>%
          ungroup()
        
        polys_list <- vector("list", nrow(df_processed))
        
        for (i in seq_len(nrow(df_processed))) {
          row <- df_processed[i, ]
          
          theta <- seq(row$start_angle, row$end_angle, length.out = 20)
          
          lat_rad <- row$lat * pi / 180
          meters_per_deg_lat <- 111139
          meters_per_deg_lon <- 111139 * cos(lat_rad)
          
          dx <- row$radius_m * sin(theta) / meters_per_deg_lon
          dy <- row$radius_m * cos(theta) / meters_per_deg_lat
          
          coords <- rbind(
            c(row$lon, row$lat),
            cbind(row$lon + dx, row$lat + dy),
            c(row$lon, row$lat)
          )
          
          polys_list[[i]] <- st_polygon(list(coords))
        }
        
        sf_out <- st_sf(
          df_processed %>% 
            select(
              locality_id, 
              locality, 
              species_latin_fixed, 
              sequence_id, 
              seq_short, 
              perc_min_no_ind,
              min_no_ind,
              sum_min_no_ind, 
              max_possible_no_ind,
              pcoa_axis1,
              color_val, 
              radius_m
            ),
          geometry = st_sfc(polys_list, crs = 4326)
        )
      }
      
      return(sf_out)
    }
    
    
    output$asv_map <- renderLeaflet({
      req(input$asv_species)
      
      leaflet() |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Ortophoto") |>
        addProviderTiles(providers$OpenTopoMap, group = "Topo") |>
        addLayersControl(
          overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        hideGroup(c("Topo", "Ortophoto")) |>
        addLegend(
          position = "bottomright",
          colors = asv_colors(seq(from = 0, to = 1, by = 0.25)),
          labels = round(seq(from = 0, to = 1, by = 0.25), 2),
          title = "Color scale 0-1",
          opacity = 1
        )
    })
    
    
    # Track whether we've set the initial extent for the current dataset selection
    map_initialized <- reactiveVal(FALSE)
    
    # Reset initialization flag whenever the target species/dataset changes
    observeEvent(list(input$project, input$asv_species), {
      map_initialized(FALSE)
    })
    
    observeEvent(list(input$project, input$asv_species, input$pie_size, input$color_mode), {
      req(input$asv_species)
      req(input$pie_size)
      req(input$color_mode)
      
      df_current <- sel_asv()
      
      df_sorted <- df_current %>%
        arrange(across(any_of(c("locality_id", "pcoa_axis1")))) 
      req(nrow(df_current) > 0)
      
      # 1. Prepare spatial polygon geometries
      if(input$color_mode != "Total diversitet per lok."){
      pie_sf <- prepare_spatial_pie_slices(
        df = df_sorted,
        pie_scale_factor = input$pie_size,
        base_radius_m = 250,
        aggregation_level = "slices"
      )} else{
        pie_sf <- prepare_spatial_pie_slices(
          df = df_sorted,
          pie_scale_factor = input$pie_size,
          base_radius_m = 250,
          aggregation_level = "pie")
      }
      
      # 2. Map hex colors
      if (input$color_mode == "Genetisk avstand") {
        
        col_ref <- custom_colors(df = df_sorted, 
                                 type = "asv_color")
        color_map <- setNames(col_ref$custom_col, col_ref$seq_short)
        pie_sf$hex_color <- color_map[pie_sf$seq_short]
        pie_sf$hex_color[is.na(pie_sf$hex_color)] <- "#808080"
        
      } else if (input$color_mode == "Tilfeldige farger") { 
        
        # Get unique sequences
        unique_seqs <- unique(pie_sf$seq_short)
        n_seqs <- length(unique_seqs)
        
        # Generate N distinct hex colors evenly spaced around the HSV color wheel
        # (Golden ratio hue stepping creates visually distinct adjacent colors)
        distinct_colors <- hsv(
          h = (seq(0, n_seqs - 1) * 0.618033988749895) %% 1,
          s = 0.8,
          v = 0.95
        )
        
        # Map colors to unique sequence IDs
        color_map <- setNames(distinct_colors, unique_seqs)
        pie_sf$hex_color <- color_map[pie_sf$seq_short]
        
      } else if(input$color_mode == "Total diversitet per lok."){
        col_ref <- custom_colors(df_current,
                                 type = "loc_color")
        color_map <- setNames(col_ref$custom_col, col_ref$locality)
        pie_sf$hex_color <- color_map[pie_sf$locality]
        pie_sf$hex_color[is.na(pie_sf$hex_color)] <- "#808080"
      }
      
      proxy <- leafletProxy("asv_map")
      
      # 3. Fit bounds ONLY on species selection switch
      if (!map_initialized()) {
        min_lon <- min(df_current$lon, na.rm = TRUE)
        max_lon <- max(df_current$lon, na.rm = TRUE)
        min_lat <- min(df_current$lat, na.rm = TRUE)
        max_lat <- max(df_current$lat, na.rm = TRUE)
        
        proxy |> fitBounds(lng1 = min_lon, lat1 = min_lat, lng2 = max_lon, lat2 = max_lat)
        map_initialized(TRUE)
      }
      
      pie_sf_sorted <- pie_sf %>%
        arrange(desc(radius_m), locality)
      
      popup_text <- if (input$color_mode == "Total diversitet per lok.") {
        # Full pie aggregation level (no specific ASV context)
        paste0(
          "<strong>Lokalitet: </strong>", pie_sf_sorted$locality, "<br>",
          "<strong>Samplet antall ganger: </strong>", pie_sf_sorted$max_possible_no_ind, "<br>",
          "<strong>Antall individer alle ASV >=: </strong>", pie_sf_sorted$sum_min_no_ind
        )
      } else {
        # Slice level (includes specific ASV info)
        paste0(
          "<strong>Lokalitet: </strong>", pie_sf_sorted$locality, "<br>",
          "<strong>Samplet antall ganger: </strong>", pie_sf_sorted$max_possible_no_ind, "<br>",
          "<strong>Antall individer alle ASV >=: </strong>", pie_sf_sorted$sum_min_no_ind, "<br>",
          "<strong>ASV: </strong>", pie_sf_sorted$seq_short, "<br>",
          "<strong>Antall individer denne ASV >=: </strong>", pie_sf_sorted$min_no_ind
        )
      }
      
      
      # 4. Redraw WebGL polygons while maintaining existing viewport/zoom
      proxy |>
        clearGlLayers() |>
        addGlPolygons(
          data = pie_sf_sorted,
          fillColor = pie_sf_sorted$hex_color,
          fillOpacity = 1,
          color = NULL,
          weight = 0,
          bounds = FALSE, # Preserves user pan & zoom
          popup = popup_text,
          group = "pie_gl_layer"
        )
    }, ignoreInit = TRUE)

    output$asvmap_text <- renderUI({
      HTML(paste0(
    "<p>Kartet til høyre viser funnstedet for enkelte arter, og kakediagrammene representerer sammensetningen av genetiske varianter innen hver art. Hver farge representerer en spesifikk genetisk variant, og størrelsen på kakebitene viser hvor relativt vanlig de genetiske variantene var på hvert sted. Hvert enkelt funn av en genetisk variant representerer minst ét individ. Finner man en genetisk variant tre ganger, eller tre genetiske varianter én gang hver, har man derfor samlet inn i hvert fall tre individer. Størrelsen på sirklene gjenspeiler minimum totalt antall individer av arten i proporsjon til antall prøvetilfeller. Kakene, som kan skaleres etter ønske, er derfor større jo flere genetiske varianter som er funnet på et sted, og jo flere ganger de er funnet.</p>
Dataene kan vises på flere måter, enten som tilfeldige farger for hver unike genetiske variant, eller der fargene (så langt det er mulig) representerer den genetiske avstanden mellom ulike varianter langs en lineær skala fra 0 til 1. Kakene kan også fargelegges etter den totale genetiske variasjonen på tvers av alle varianter innen en lokalitet, standardisert mellom 0 og 1 (viser foreløpig antall unike sekvenser).</p>
<p>Nedenfor kan man søke på enkeltarter. Per i dag har overvåkingsprogrammet et begrenset geografisk og tidsmessig omfang. Dataene for arter som er observert med få individer på få steder vil være mer tilfeldige enn for arter med mange individer fanget på mange steder. Sikkerhet på artsbestemmelsen angir usikkerheten knyttet til den automatiske artsidentifiseringen med DNA. De fleste funn er ikke gjennomgått manuelt, og det kan være feil i artsnavn selv om vi angir sikkerheten som høy.</p>"
))
    })
    
    
  })
}
