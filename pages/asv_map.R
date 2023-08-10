
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
                                        width = "70%",
                                        height = 600),
                 height = "800px"
                 
             ),
             box(title = "Vi registrerer innenartsvariasjon",
                 textOutput(ns("asvmap_text")),
                 height = "400px"),
             shinydashboardPlus::box(id = "speciesbox",
                                     title = "Artsutvalg",
                                     uiOutput(ns("choose_conf")),
                                     uiOutput(ns("choose_order")),
                                     uiOutput(ns("choose_fam")),
                                     uiOutput(ns("choose_spec")),
                                     height = "300px"
                                     
             )
           )
  )
  
}




asvmap_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    

    load("data/shinyPass.Rdata")
    
    connect_to_insect_db(user = my_username,
                         password = my_password)
   
    
    asv_perc_reads <- tbl(con,
                          Id(schema = "views",
                             table = "asv_perc_reads"))
    
    
    species_list <- tbl(con,
                        Id(schema = "views",
                           table = "species_list")) 
    
    
    
    output$choose_conf <- renderUI({
      
      conf_choices <- c("HIGH", "MODERATE", "LOW", "POOR", "ALL")
      
      
      selectInput(inputId = ns("sel_conf"),
                  label = "Velg konfidansenivå",
                  choices = conf_choices,
                  selected = "")
    })
    
    
    output$choose_order <- renderUI({
      
      order_choices <- species_list %>% 
        select(id_order) %>%
        filter(!is.na(id_order)) %>% 
        distinct() %>%
        arrange(id_order) %>% 
        pull()
      
      selectInput(inputId = ns("sel_order"),
                  label = "Velg orden",
                  choices = order_choices,
                  selected = "Blattodea")
    })
    
    
    
    output$choose_fam <- renderUI({
      
      req(input$sel_order)
      
      family_choices_q <- "
        SELECT distinct id_family
        from views.species_list
        WHERE id_order = ?id1
        AND id_family IS NOT NULL
        ORDER BY id_family
      "
      
      family_choice_san <- sqlInterpolate(con,
                                          family_choices_q,
                                          id1 = input$sel_order)
      
      family_choices <- dbGetQuery(con,
                                   family_choice_san)
      
      
      
      selectInput(inputId = ns("sel_fam"),
                  label = "Velg familie",
                  choices = family_choices$id_family,
                  selected = "")
      
      
    })
    
    
    
    
    output$choose_spec <- renderUI({
      req(input$sel_order)
      req(input$sel_fam)
      req(input$sel_conf)
      
      
      
      if(input$sel_conf != "ALL"){ 
        species_choices_q <- "
          SELECT distinct species_latin_gbif
          from views.species_list
          WHERE id_order = ?id1
          AND id_family = ?id2
          AND identification_confidence = ?id3
          AND species_latin_gbif IS NOT NULL
          ORDER BY species_latin_gbif
        "
        
        species_choice_san <- sqlInterpolate(con,
                                             species_choices_q,
                                             id1 = input$sel_order,
                                             id2 = input$sel_fam,
                                             id3 = input$sel_conf)
        
        species_choices <- dbGetQuery(con,
                                      species_choice_san)
        
      } else {
        species_choices_q <- "
          SELECT distinct species_latin_gbif
          from views.species_list
          WHERE id_order = ?id1
          AND id_family = ?id2
          AND species_latin_gbif IS NOT NULL
          ORDER BY species_latin_gbif
        "
        
        species_choice_san <- sqlInterpolate(con,
                                             species_choices_q,
                                             id1 = input$sel_order,
                                             id2 = input$sel_fam)
        
        species_choices <- dbGetQuery(con,
                                      species_choice_san) 
        
      }
      
      
      
      selectInput(ns("asv_species"),
                  label = "Velg art",
                  choices = species_choices$species_latin_gbif,
                  selected = "",
                  selectize = TRUE)
      
    })
    
    
    
    basemap <- leaflet(width = "100%",
                       height = "400px") %>% 
      addTiles(group = "OpenStreetMap")
    
    
    selected_species <- reactive({
      if(is.na(input$asv_species)) return(NULL)
      
      species <- input$asv_species
      return(species) 
      
    })
    
    asv_to_leaflet <- function(species = "NULL"){
      
      sel_asv <- asv_perc_reads %>%
        filter(species_latin_gbif == !!selected_species())  %>%
        collect() %>% 
        #  filter(!!input$species_select_asv %in% (species_latin_gbif)) %>%
        mutate(asv = as_factor(sequence_id),
               perc_reads = round(perc_reads*100, 2))
      
      
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
      
      to_plot <- asv_to_leaflet(input$asv_species)
      
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
    
    output$asvmap_text <- renderText("Som en del i metastrekkodingen, registreres også innenartsvariasjon. Det betyr at vi noterer hver unik genetisk variant av den del av den genetiske koden vi leser av. Noen arter har større variasjon i dette område enn andre arter, og vi fanger ikke all genetisk variasjon med denne metodikk. Likevel kan det være ett nyttig verktøy for å undersøke separerte populasjoner, anpassinger til lokalt klima, spredningshistorikk, og forandringer i populasjonsstørrelse.
       
Kakediagrammene til venstre viser komposisjonen av genetiske varianter for en gitt art på hver overvåkingslokalitet den er blitt funnet.  [Mer detaljer...]   

Bruk menyene nedenfor for å finne frem til en art av interesse. Notere at orervåkingsprogrammet fortsatt har en begrenset geografisk og tidsmessig utbredelse. Kart for arter som er observert bare ved et fåtall individer på et fåtall plasser vil være mer tilfeldige enn arter med mange individer fanget på mange plasser.
"
    )
    
    
  })
}