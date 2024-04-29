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

div_map_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Hotspots",
           column(6,
           box(width = 12,
               title = "Plasser med høy diversitet",
               textOutput(ns("div_map_text")),
               height = "500px"),
           shinydashboardPlus::box(width = 12,
                                   id = "hotspotbox",
                                   title = "Gruppeutvalg",
                                   fluidRow(column(12,
                                     uiOutput(ns("choose_group"))
                                     )),
                                   height = "400px"
                                   )
                                   
           ),
           column(6,
               shinydashboardPlus::box(width = 12,
                                       id = "div_map_box",
               title = "Fordeling av diversitet",
               shinycssloaders::withSpinner({
               leaflet::leafletOutput(ns("div_map"),
                                      width = "95%",
                                      height = 800)
                 },
               type = 2,
               color = "#E57200",
               color.background = "#004F71"),
               height = "800px"
               
           )
           )
  )
  
}



div_map_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    output$div_map_text <- renderText("En nasjonal insektovervåking gir ny og verdifull kunnskap om fordelingen av insektmangfoldet og hvor sjeldne arter finns. Vi kan for eksempel få ny kunnskap om utbredelsen til rødlistete arter, eller arter som ikke tidligere er registrert fra landet. 
    
Det er imidlertid viktig og være klar over at funnene ikke er manuelt verifisert, og at det kan være feil i DNA-bibliotekene. Disse feilene vil imidlertid bli færre over tid.

Her kan du se diversitetsmønstre for noen utvalg av artsgrupper. Størrelsen på punktene viser det (relative) antallet arter på hver plass for hver gruppe. For at punktene ikke skal overlappe er de spredt ut fra fangstlokalitetet med 5 km. Klikke på punktene for å få se artsantallet innen hver gruppe på en lokalitet.
"
    )
    
    output$choose_group <- renderUI({
      
      group_choices <- c("Rødlista arter", "Fremmede arter", "Pollinatorer", "Alle")
      
      selectInput(ns("chosen_group"),
                  label = "Velg gruppe",
                  choices = c(group_choices, ""),
                  #selected = "",
                  selectize = TRUE)
      
    })

    ##Get data from cache  
    redlisted_obs_2021_agg <- sf::read_sf(dsn = "data/redlisted_obs_2021_agg.shp")
    
    all_alien_obs_agg <- sf::read_sf(dsn = "data/all_alien_obs_agg.shp")
    
    poll_obs_agg <- sf::read_sf(dsn = "data/poll_obs_agg.shp")
    
    NorIns_richn_loc <- sf::read_sf(dsn = "data/NorIns_richn_loc.shp")
    
    ##End get pot alien species
    
    
    norge <- get_map()
    
    basemap <- leaflet(width = "300px",
                       height = "200px") %>% 
      addTiles(group = "OpenStreetMap")
    
    
    output$div_map <- renderLeaflet({
      req(input$chosen_group)
      #req(input$species_filter)
      
      if(input$chosen_group == "Rødlista arter"){
      to_plot <- redlisted_obs_2021_agg
      leaflet_colors <- c("NT" = "#ed6c26",
                          "VU" =  "#e94f33", 
                          "EN" = "#d80f27", 
                          "RE" = "#5a5b5d")
      } else { 
        if(input$chosen_group == "Fremmede arter"){
          to_plot <- all_alien_obs_agg
          leaflet_colors <- c("Fennoskandisk forek." = "#004F71",
                              "Potensielt fremmede arter" =  "#008C95", 
                              "På fremmedartslista" = "#E57200")
         } else {
        if(input$chosen_group == "Pollinatorer"){
          to_plot <- poll_obs_agg
          leaflet_colors <- c("Bier" = "#93328E",
                              "Blomsterfluer" =  "#FFB25B",
                              "Sommerfugler" = "#2DCCD3")
        } else {
          if(input$chosen_group == "Alle"){
          to_plot <- NorIns_richn_loc
          leaflet_colors <- c("Semi-nat" = "#E57200",
                              "Forest" = "#7A9A01")
          
        }
           else
             NULL 
           
         } 
           }}
      
      pal <- colorFactor(palette = leaflet_colors,
                         levels = names(leaflet_colors))
      
      basemap  %>% 
        leaflet::addProviderTiles(providers$Esri.WorldImagery,
                                  group = "Ortophoto") %>% 
        leaflet::addProviderTiles(providers$OpenTopoMap, 
                                  group = "Topo")  %>% 
        leaflet::addLayersControl(overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto") 
                                  , options = layersControlOptions(collapsed = FALSE)) %>% 
        leaflet::hideGroup(c("Topo", "Ortophoto"))  %>% 
      
        leaflet::addCircleMarkers(radius = datawizard::rescale(to_plot$no_spec, to = c(2, 12)),
                                  color = ~pal(kategori),
                                  data = to_plot,
                                  fillColor =  ~pal(kategori),
                                  fill = TRUE,
                                  fillOpacity = 100,
                                  stroke = FALSE,popup = ~paste0(locality, ": ", no_spec)) %>% 
        addLegend(labels = names(leaflet_colors),
                  colors = leaflet_colors,
                  opacity = 1)
              
    })
    
       
  })
}