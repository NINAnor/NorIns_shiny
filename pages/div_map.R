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
           
           box(title = "Fordeling av diversitet",
               leaflet::leafletOutput(ns("div_map"),
                                      width = "95%",
                                      height = 600),
               height = "800px"
               
           )
  )
  
}



div_map_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    output$div_map_text <- renderText("En nasjonal insektovervåking gir ny og verdifull kunnskap om fordelingen av insektmangfoldet og hvor sjeldne arter finns. Vi kan for eksempel få ny kunnskap om utbredelsen til rødlistete arter, eller arter som ikke tidligere er registrert fra landet. Det er imidlertid viktig og være klar over at funnene ikke er manuelt verifisert, og at det kan være feil i DNA-bibliotekene. Disse feilene vil imidlertid bli færre over tid.

Her kan du se utbredelsen av diversitetsmønstre for ulike utvalg av artsgrupper.
"
    )
    
    output$choose_group <- renderUI({
      
      group_choices <- c("Rødlista arter", "Fremmede arter", "Pollinatorer")
      
      selectInput(ns("chosen_group"),
                  label = "Velg gruppe",
                  choices = c(group_choices, ""),
                  #selected = "",
                  selectize = TRUE)
      
    })

    ##Get redlisted dataset, put this in separate script later  
    
    redlisted_obs_2021 <- read_sf(con,
                                  Id(schema = "views",
                                     table = "redlisted_obs")) %>% 
      mutate(kategori = kategori_2021)
    
    redlisted_obs_2021 <- redlisted_obs_2021 %>% 
      #mutate(expl_fact = gsub("([a-zA-Z]*)( >)(.*)", "\\1", expl_fact)) %>% 
      separate(expl_fact, 
               into = c("expl_1",
                        "expl_2",
                        "expl_3",
                        "expl_4",
                        "expl_5"),
               sep = ">") %>% 
      separate(expl_3,
               into = "expl_3_main",
               sep = "_") %>%
      mutate(expl_3_main = ifelse(is.na(expl_3_main), expl_1, expl_3_main)) %>% 
      mutate(expl_3_main = stringr::str_trim(expl_3_main)) %>% 
      mutate(expl_3_main = ifelse(expl_3_main == " ", NA, expl_3_main),
             expl_3_main = ifelse(expl_3_main == "", NA, expl_3_main),
             expl_3_main = ifelse(expl_3_main == "Ukjent ", NA, expl_3_main),
             expl_3_main = ifelse(expl_3_main == "Ukjent", NA, expl_3_main)) 
    
    redlisted_obs_2021 <- redlisted_obs_2021 %>%
      group_by(kategori) %>%
      mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>%
      ungroup() %>%
      filter(kategori != "DD")
    
 
    redlisted_obs_2021_agg <- redlisted_obs_2021 %>% 
      group_by(locality,
               kategori) %>% 
      summarise(no_spec = n_distinct(species_latin_fixed)) %>% 
      ungroup() %>% 
      st_jitter(redlisted_obs_2021_agg, amount = 7000)  %>% 
      st_transform(4326)
    
    ##End get redlisted species
    
    ##Get pot alien species
    
    fennoskand_obs_q <- "
      SELECT 
      obs.species_latin_fixed,
      yl.year,
      ls.sampling_name,
      l.locality,
      ST_Centroid(l.geom) as geom
      FROM occurrences.observations obs,
      lookup.fennoscand_species2 fennoscand,
      events.identifications,
      events.year_locality yl,
      events.locality_sampling ls,
      locations.localities l,
      events.sampling_trap st
      WHERE obs.identification_id = identifications.id
      AND obs.species_latin_fixed = fennoscand.species_latin_fixed
      AND identifications.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
      AND yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      AND obs.identification_confidence = 'HIGH'
      "
    
    fennoskand_obs <- read_sf(con,
                              query = fennoskand_obs_q) %>% 
      mutate(kategori = "Fennoskandisk forek.")
    
  
    pot_alien_obs_q <- "
      SELECT 
      obs.species_latin_fixed,
      yl.year,
      ls.sampling_name,
      l.locality,
      ST_Centroid(l.geom) as geom
      FROM occurrences.observations obs,
      lookup.pot_alien_species2 pot_alien,
      events.identifications,
      events.year_locality yl,
      events.locality_sampling ls,
      locations.localities l,
      events.sampling_trap st
      WHERE obs.identification_id = identifications.id
      AND obs.species_latin_fixed = pot_alien.species_latin_fixed
      AND identifications.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
      AND yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      AND obs.identification_confidence = 'HIGH'
      "
    
    pot_alien_obs <- read_sf(con,
                             query = pot_alien_obs_q) %>% 
      mutate(kategori = "Potensielt fremmede arter")
  
    
  
    alien_obs_q <- "
      SELECT 
      obs.species_latin_fixed,
      yl.year,
      ls.sampling_name,
      l.locality,
      ST_Centroid(l.geom) as geom
      FROM occurrences.observations obs,
      lookup.fremmedartslista_2018_artsdatabanken alien,
      events.identifications,
      events.year_locality yl,
      events.locality_sampling ls,
      locations.localities l,
      events.sampling_trap st
      WHERE obs.identification_id = identifications.id
      AND obs.species_latin_fixed = alien.\"scientificName\"
      AND identifications.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
      AND yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      AND alien.\"riskCategory\" IN ('NK', 'SE', 'HI', 'PH', 'LO')
      AND obs.identification_confidence = 'HIGH'
      "  
    
    alien_obs <- read_sf(con,
                         query = alien_obs_q) %>% 
      mutate(kategori = "På fremmedartslista")
   
  
    all_alien_obs <- fennoskand_obs %>% 
      rbind(pot_alien_obs)  %>% 
      rbind(alien_obs)
    
 
    kat_order <- tibble(kategori = c("Fennoskandisk forek.",
                                      "Potensielt fremmede arter",
                                      "På fremmedartslista"),
                        kat_order = 1:3)
    
    all_alien_obs_agg <- all_alien_obs %>% 
      group_by(kategori) %>% 
      mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>% 
      group_by(locality,
               kategori,
               no_spec_per_kat) %>% 
      summarise(no_spec = n_distinct(species_latin_fixed)) %>% 
      ungroup() %>% 
      mutate(kategori_append = factor(paste0(kategori, " (", no_spec_per_kat, " stk.)"))) %>% 
      left_join(kat_order,
                by = c("kategori" = "kategori")) %>% 
      mutate(kategori = factor(kategori, levels = c("Fennoskandisk forek.",
                                                    "Potensielt fremmede arter",
                                                    "På fremmedartslista"))
             )
    
    all_alien_obs_agg <- all_alien_obs_agg %>% 
      st_jitter(all_alien_obs_agg, amount = 7000)  %>% 
      st_transform(4326)
   
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
        NULL
         }}
      
      pal <- colorFactor(leaflet_colors, domain = names(leaflet_colors))
      
      basemap  %>% 
        leaflet::addProviderTiles(providers$Esri.WorldImagery,
                                  group = "Ortophoto") %>% 
        leaflet::addProviderTiles(providers$OpenTopoMap, 
                                  group = "Topo")  %>% 
        leaflet::addLayersControl(overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto") 
                                  , options = layersControlOptions(collapsed = FALSE)) %>% 
        leaflet::hideGroup(c("Topo", "Ortophoto"))  %>% 
      
        leaflet::addCircleMarkers(radius = datawizard::rescale(to_plot$no_spec, to = c(2, 10)),
                                  color = ~pal(kategori),
                                  data = to_plot,
                                  fillColor =  ~pal(kategori),
                                  fill = TRUE,
                                  fillOpacity = 100,
                                  stroke = FALSE) %>% 
        addLegend(labels = names(leaflet_colors),
                  colors = leaflet_colors,
                  opacity = 1)
              
    })
    
       
  })
}