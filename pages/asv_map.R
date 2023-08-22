
require(leaflet)
require(leaflet.minicharts)
require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(Norimon)

asvmap_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Innenartsvariasjon",
           fluidRow(
             box(title = "Fordeling av genetiske varianter",
                 leaflet::leafletOutput(ns("asv_map"),
                                        width = "95%",
                                        height = 600),
                 height = "800px"
                 
             ),
             box(title = "Vi registrerer innenartsvariasjon",
                 textOutput(ns("asvmap_text")),
                 height = "400px"),
             shinydashboardPlus::box(id = "speciesbox",
                                     title = "Artsutvalg",
                                     fluidRow(
                                       column(3,
                                     uiOutput(ns("choose_conf")),
                                     uiOutput(ns("choose_order")),
                                     uiOutput(ns("choose_fam"))),
                                     column(3,
                                     uiOutput(ns("choose_spec")),
                                     uiOutput(ns("choose_species_filter"))
                                     )
                                     ),
                                     height = "300px"
                                     
             )
           )
  )
  
}




asvmap_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
  
    output$choose_conf <- renderUI({
      
      conf_choices <- c("HIGH", "MODERATE", "LOW", "POOR", "ALL")
      
      
      selectInput(inputId = ns("sel_conf"),
                  label = "Velg konfidansenivå på navngiving",
                  choices = conf_choices,
                  selected = "")
    })
    
    
    output$choose_order <- renderUI({
      
      con <- login_import$con()
      
      order_choices_q <- "

        SELECT sl.id_order, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
        FROM
        (SELECT distinct id_order as id_order
        from views.species_list
        WHERE id_order IS NOT NULL) sl LEFT JOIN
        
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
      
      order_choices_san <- sqlInterpolate(con,
                                         order_choices_q)
      
      order_choices_raw <- dbGetQuery(con,
                                   order_choices_san)
      
      order_choices_list <- as.list(order_choices_raw$id_order)
      
      if(length(order_choices_list) >0){
      names(order_choices_list) <- paste0(order_choices_raw$id_order, ' - ', order_choices_raw$bokmal)
      }
      
      selectInput(inputId = ns("sel_order"),
                  label = "Velg orden",
                  choices = order_choices_list,
                  selected = "Blattodea")
    })
    
    
    
    output$choose_fam <- renderUI({
      
      con <- login_import$con()
      
      req(input$sel_order)
      
      family_choices_q <- "
                SELECT sl.id_family, INITCAP(COALESCE(names.populaernavn_bokmaal, '')) bokmal
                FROM
                (SELECT distinct id_family as id_family
                from views.species_list
                WHERE id_family IS NOT NULL
        		    AND id_order = ?id1) sl LEFT JOIN
                
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
      
      family_choice_san <- sqlInterpolate(con,
                                          family_choices_q,
                                          id1 = input$sel_order)
      
      family_choices_raw <- dbGetQuery(con,
                                   family_choice_san)
      
      family_choices_list <- as.list(family_choices_raw$id_family)
      
      if(length(family_choices_list) >0){
      names(family_choices_list) <- paste0(family_choices_raw$id_family, ' - ', family_choices_raw$bokmal)
      }
      
      selectInput(inputId = ns("sel_fam"),
                  label = "Velg familie",
                  choices = family_choices_list,
                  selected = "")
      
      
    })
    
    
    
    
    output$choose_spec <- renderUI({
      req(input$sel_order)
      req(input$sel_fam)
      req(input$sel_conf)
      
      con <- login_import$con()
      

      if(input$sel_conf != "ALL"){ 
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
         AND identification_confidence = ?id3) sl LEFT JOIN
        
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
        
        species_choice_san <- sqlInterpolate(con,
                                             species_choices_q,
                                             id1 = input$sel_order,
                                             id2 = input$sel_fam,
                                             id3 = input$sel_conf)
        
        species_choices_raw <- dbGetQuery(con,
                                      species_choice_san)
        
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
         AND id_family = ?id2) sl LEFT JOIN
        
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
        
        species_choice_san <- sqlInterpolate(con,
                                             species_choices_q,
                                             id1 = input$sel_order,
                                             id2 = input$sel_fam)
        
        species_choices_raw <- dbGetQuery(con,
                                      species_choice_san) 
        
      }
      
      
      species_choices_list <- as.list(species_choices_raw$species_latin_gbif)
      
      if(length(species_choices_list) >0){
        names(species_choices_list) <- paste0(species_choices_raw$species_latin_gbif, ' - ', species_choices_raw$bokmal)
      }
      
      selectInput(ns("asv_species"),
                  label = "Velg art fra familie",
                  choices = c(species_choices_list, ""),
                  #selected = "",
                  selectize = TRUE)
      
    })
    
    
    
    basemap <- leaflet(width = "300px",
                       height = "200px") %>% 
      addTiles(group = "OpenStreetMap")
    
    

  output$choose_species_filter <- renderUI({
    #req(input$asv_species)

    selectizeInput(inputId = ns("species_filter"),
                   label = "Overstyr via fritekst",
                   choices = NULL,
                   selected = NULL
    )
    

  })
  

    

  observeEvent(input$asv_species,{
    
    con <- login_import$con()
    
      loc_species_list <- tbl(con,
                              Id(schema = "views",
                                 table = "loc_species_list"))


      species_choices <- loc_species_list %>%
        select(species_latin_gbif) %>%
        distinct() %>%
        arrange(species_latin_gbif) %>%
        pull()

      #if(input$species_filter != "Ingen"){
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           inputId = "species_filter",
                           choices = c("Ingen", species_choices),
                           selected = "Ingen",
                           server = TRUE,
                           options = list(maxOptions = 10)
                           )
      #}
  }
)

  
    # observeEvent(input$species_filter,{
    # 
    # 
    #   if(input$species_filter != "Ingen") {
    #     
    #     con <- login_import$con()
    #     
    #     taxa_reverse_q <- "
    #     SELECT *
    #     FROM views.species_list
    #     WHERE species_latin_gbif = ?id1
    #     "
    #     
    #     taxa_reverse_sql <- sqlInterpolate(con,
    #                                        taxa_reverse_q,
    #                                        id1 = input$species_filter)
    #     
    #     taxa_reverse_res <- dbGetQuery(con,
    #                                    taxa_reverse_sql)
    #     
    #   updateSelectInput(inputId = "sel_conf",
    #                       selected = taxa_reverse_res$identification_confidence)
    #   
    #   updateSelectInput(inputId = "sel_order",
    #                     selected = taxa_reverse_res$id_order)
    #   
    #   updateSelectInput(inputId = "sel_fam",
    #                     selected = taxa_reverse_res$id_family)
    #   
    #   updateSelectInput(inputId = "asv_species",
    #                     selected = taxa_reverse_res$species_latin_gbif)
    #     
    #   }
    # 
    # },
    # ignoreNULL = TRUE,
    # ignoreInit = TRUE,
    # priority = 0)
    
    
    selected_species <- reactive({
      if(is.na(input$asv_species)) return(NULL)
      
      if(input$species_filter == "Ingen"){
        species <- input$asv_species
      } else {
        species <- input$species_filter
      }
      
      return(species) 
      
    })
    #species = "NULL"
    asv_to_leaflet <- function(){
      
      con <- login_import$con()
      
      asv_perc_reads <- tbl(con,
                            Id(schema = "views",
                               table = "asv_perc_reads"))
      
      sel_asv <- asv_perc_reads %>%
        filter(species_latin_gbif == !!selected_species())  %>%
        collect() %>% 
        #  filter(!!input$species_select_asv %in% (species_latin_gbif)) %>%
        mutate(asv = as_factor(sequence_id),
               perc_reads = round(perc_reads*100, 2)) %>% 
        arrange(sequence_id)
      
      
      to_plot  <- sel_asv %>% 
        # mutate(lat = st_coordinates(geometry)[,2],
        #        lon = st_coordinates(geometry)[,1]) %>%
        # st_drop_geometry() %>% 
        select(locality, 
               lat,
               lon, 
               sequence_id,
               perc_reads,
               sum_reads) %>% 
        pivot_wider(names_from = "sequence_id",
                    values_from = "perc_reads",
                    names_prefix = "seq_",
                    values_fill = 0)
      
      return(to_plot)
    }
    
    
    
    
    
    output$asv_map <- renderLeaflet({
      req(input$asv_species)
      req(input$species_filter)
      
      to_plot <- asv_to_leaflet()
      
      basemap  %>% 
        leaflet::addProviderTiles(providers$Esri.WorldImagery,
                                  group = "Ortophoto") %>% 
        leaflet::addProviderTiles(providers$OpenTopoMap, 
                                  group = "Topo")  %>% 
        leaflet::addLayersControl(overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto") 
                                  , options = layersControlOptions(collapsed = FALSE)) %>% 
        leaflet::hideGroup(c("Topo", "Ortophoto"))  %>% 
        addMinicharts(to_plot$lon,
                      to_plot$lat,
                      type = "pie",
                      chartdata = to_plot[, which(grepl("seq_", names(to_plot)))],
                      width = log(to_plot$sum_reads) * 3 ,
                      legend = FALSE,
                      popup = list(noPopup = TRUE)
                      # popupOptions = list(autoPan = FALSE,
                      #                     maxHeight = 400,
                      #                     maxWidth = 400,
                      #                     minWidth = 200
                      #                    )
        )
      
    })
    
    output$asvmap_text <- renderText("Som en del i metastrekkodingen registrerer vi også innenartsvariasjon. Dette betyr at vi noterer hver unik variant i den porsjon av genomet vi registrerer. Denne metodikk fanger ikke all genetisk variasjon, og ulike arter har også ulik mye variasjon i dette område. Likevel kan det være ett nyttig verktøy for å oppdage distinkte populasjoner, lokale anpassinger til klima og miljø, tilfeldig spredningshistorikk, og forandringer i populasjonsstørrelse.
       
Kakediagrammene til venstre viser komposisjonen av genetiske varianter der hver farge representerer en gitt genetisk variant. Størrelsen på kakene er skalert etter hvor mange DNA-sekvenser det totalt er blitt funnet av arten i hver lokalitet, og størrelsen på kakebitene viser hvor stor del av disse en gitt genetisk variant står for.

Bruk menyene nedenfor for å finne frem til en art av interesse. Notere at orervåkingsprogrammet fortsatt har en begrenset geografisk og tidsmessig utbredelse. Kart for arter som er observert bare ved et fåtall individer på et fåtall plasser vil være mer tilfeldige enn arter med mange individer fanget på mange plasser.

Konfidansenivå angir usikkerheten knyttet til den automatiske identifiseringen med DNA. De fleste av funnene er ikke gjennomgått manuelt og det kan være feil i artsnavn innenfor alle konfidansenivåer.

"
    )
    
    
  })
}