
require(leaflet)
require(leaflet.minicharts)
require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(Norimon)
require(plotly)
require(NinaR)

locspec_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Artsantall per lokalitet",
           fluidRow(
             box(title = "Overvåkingslokaliteter",
                 leaflet::leafletOutput(ns("loc_map"),
                                        width = "95%",
                                        height = 400),
                 height = "500px"
                 
             ),
             shinydashboardPlus::box(id = "taxabox",
                                     title = "Taksonomisk utvalg",
                                     textOutput(ns("loc_spec_text")),
                                     uiOutput(ns("choose_project")),
                                     uiOutput(ns("choose_region")),
                                     uiOutput(ns("choose_habitattype")),
                                     height = "300px"
                                     
             )
           ),
           fluidRow(
             box(title = "Tidstrend",
               plotlyOutput(ns("loc_spec"),
                        height = "200px")
             )
             
           )
  )
  
}



locspec_server <- function(id, login_import) {
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session) {
    
    
    output$loc_spec_text <- renderText("Her vises det totale antallet arter som er observert på hvert lokalitet og en lokal polynomisk modell for trendene da det lar seg gjøre. For å kunne sammenligne resultatene vises kun resultater fra malaisefeller. Det går å filtrere utvalget basert på regioner og habitatstyper, samt gjennom å zoome i kartet. Antallet arter som fanges varierer over tid og mellom plasser, og det vil ta tid for å kunne dra konklusjoner om stabile trender. Resultatene på disse figurer må tolkes med omhu. 
"
    )
    
    
    output$choose_project <- renderUI({
      con <- login_import$con()
      
      projects_tab <- tbl(con,
                          in_schema("lookup", "projects"))
      
      projects <- projects_tab %>% 
        select(project_name) %>% 
        distinct() %>% 
        collect() %>% 
        arrange()
      
      
      
      selectInput(inputId = ns("project"),
                  label = "Prosjekt",
                  choices = c("", projects$project_name),
                  selected = c("Nasjonal insektovervåking"),
                  selectize = FALSE)
      
    })
    
    ## Choose region of available regions in database
    
    output$choose_region <- renderUI({
      req(input$project)
      con <- login_import$con()
      
      localities <- tbl(con,
                        in_schema("locations", "localities"))
      
      regions_q <- "
      SELECT distinct(l.region_name)
      FROM events.year_locality yl,
      locations.localities l,
      lookup.projects
      WHERE yl.locality_id = l.id
      AND yl.project_short_name = projects.project_short_name
      AND projects.project_name = ?id1
      ORDER BY region_name
      
      "
      
      regions_sql <- sqlInterpolate(con,
                                    regions_q,
                                    id1 = input$project)
      
      regions <- dbGetQuery(con,
                            regions_sql)
      
      
      selectInput(inputId = ns("region"),
                  label = "Region",
                  choices = c("All", regions$region_name),
                  selected = "All",
                  selectize = FALSE,
                  size = 1)
      
      
    })
    
    
    output$choose_habitattype <- renderUI({
      
      req(input$project)
      con <- login_import$con()
      
      habtypes_q <- "
      SELECT distinct l.habitat_type
      FROM locations.localities l,
      events.year_locality yl,
      lookup.projects
      WHERE yl.locality_id = l.id
      AND yl.project_short_name = projects.project_short_name
      AND projects.project_name = ?id1
      "
      
      habtypes_sql <- sqlInterpolate(con,
                                     habtypes_q,
                                     id1 = input$project)
      
      habtypes <- dbGetQuery(con,
                             habtypes_sql) 
      
      
      selectInput(inputId = ns("habitat_type"),
                  label = "Habitat type",
                  choices = c("All", habtypes$habitat_type),
                  selected = "All",
                  selectize = FALSE,
                  size = 1
    )
      
    })
    
      
    
    
    trap_points <- reactive({
      
      #con <- login_import$con()
      
      trap_sql <- "
        SELECT projects.project_name,
        trap.trap_name, 
        trap.locality,
        trap.year,
        trap_short_name,
        trap_model,
        liquid_name,
        coordinate_precision_m,
        elev_m,
        loc.habitat_type,
        st_transform(trap.geom, 4326) as point_geom,
        loc.region_name, 
        st_transform(loc.geom, 4326) as rute_geom
        FROM locations.traps trap,
        locations.localities loc,
        events.year_locality yl,
        lookup.projects
        WHERE trap.locality = loc.locality
        AND yl.locality_id = loc.id
        AND yl.project_short_name = projects.project_short_name
    
    "
      
      dat <- sf::read_sf(con, 
                         query = trap_sql)
      
      
      ##Add lon lat from geom
      
      if(!is.null(input$project)){
        if(input$project != ""){
          dat <- dat %>% 
            filter(project_name == input$project)
        }
      }
      
      
      if(!is.null(input$region)){
        if(input$region != "All"){
          dat <- dat %>% 
            filter(region_name == input$region)
        }
      }
      
   
      if(!is.null(input$habitat_type)){
        if(input$habitat_type != "All"){
          dat <- dat %>% 
            filter(habitat_type == input$habitat_type)
        }
      }
      
      return(dat)
      
    })
    
    
    ruter <- function() trap_points() %>% 
      sf::st_set_geometry("rute_geom") %>% 
      select(locality,
             year,
             habitat_type) %>% 
      distinct(locality,
               year,
               habitat_type,
               .keep_all = T) %>% 
      collect()  
    
    
    pal_cat <- isolate(ruter()$habitat_type) 
    
    pal <- leaflet::colorFactor(palette = NinaR::ninaPalette(),
                                domain = pal_cat)
   
    derived_pal <- pal(unique(pal_cat)) 
    names(derived_pal) <- unique(pal_cat)
    

    output$loc_map <- leaflet::renderLeaflet({
      req(input$project)
      req(input$region)
      req(input$habitat_type)
      
      leaflet::leaflet(width = "70%", height = 800) %>%
        
        leaflet::addTiles(group = "OpenStreetMap") %>%
        
        leaflet::addProviderTiles(providers$Esri.WorldImagery,
                                  group = "Ortophoto") %>% 
        
        leaflet::addProviderTiles(providers$OpenTopoMap, 
                                  group = "Topo") %>%
        
        leaflet::addLayersControl(overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto") 
                                  , options = layersControlOptions(collapsed = FALSE)) %>% 
        
        leaflet::hideGroup(c("Topo", "Ortophoto")) %>% 
        
        leaflet::addPolygons(data = ruter(),
                             color = ~pal(ruter()$habitat_type)) %>% 
        
        leaflet::addCircles(data = trap_points(),
                            popup = htmltools::htmlEscape(trap_points()$trap_name),
                            radius = ~trap_points()$coordinate_precision_m,
                            color = ~pal(trap_points()$habitat_type)
        ) %>% 
        
        leaflet::addLegend(pal = pal,
                           values  = ruter()$habitat_type,
                           position = "bottomright",
                           title = "Habitat type",
                           labFormat = leaflet::labelFormat(digits = 1),
                           opacity = 1)
    })
    
    
    ruterInBounds <- reactive({
      if (is.null(local(input$loc_map_bounds)))
        return(ruter()[FALSE,])
      
      bounds <- local(input$loc_map_bounds) %>% as.list()
      latRng <- range(bounds$south, bounds$north)
      lngRng <- range(bounds$west, bounds$east)
      
      suppressWarnings({ ruter <- ruter() %>% 
        dplyr::mutate(lon = sf::st_coordinates(st_centroid(.))[,1],
                      lat = sf::st_coordinates(st_centroid(.))[,2]) %>% 
        st_drop_geometry()}) 
      
      out <- ruter %>%
        filter(lat >= local(latRng[1]) & lat <= local(latRng[2]) &
                lon >= local(lngRng[1]) & lon <= local(lngRng[2])) %>%
        select(locality,
               Long = lon,
               Lat = lat) %>%
        as_tibble()
      
      return(out)
      
    })
    
    
    
    loc_species_data <- reactive({

      loc_species_q <- paste0("
      
      SELECT year, locality, habitat_type, no_spec as tot_spec
      FROM
      (SELECT locality,
      year, 
      habitat_type, 
      count(distinct(loc_spec.species_latin_gbif))::numeric no_spec
      FROM views.loc_traptype_species_list loc_spec
      WHERE trap_type = 'Malaise'
                              
      AND locality IN ('",
                              paste(ruterInBounds()$locality, collapse = "','"),
                              "') GROUP BY year, habitat_type, locality) foo
           
      
      " )
     
      
      loc_species_res <- dbGetQuery(con,
                                    loc_species_q)  %>% 
        mutate(year = as.factor(year)) 

      return(loc_species_res)
      
    })
    
    
    output$loc_spec <- renderPlotly({
      req(ns("loc_map"))

      p <- ggplot2::ggplot(aes(y = tot_spec, 
                               x = year,
                               group = habitat_type, 
                               col = habitat_type),
                           data =  loc_species_data()
                           ) +
       ggplot2::geom_point(position = position_dodge(width = 0.1)) +
       ggplot2::geom_smooth() +
        xlab("År") +
        ylab("Antall arter per lok.") +
        scale_color_manual(values = derived_pal,
                            name = "Habitatstype")
      
      suppressWarnings(suppressMessages(
        ggplotly(p) %>%
        style(hovertext = loc_species_data()[, "locality"])
      ))
      
   
    })
    
  })
}
