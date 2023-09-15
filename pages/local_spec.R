
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
           column(6,
             shinydashboardPlus::box(id = "taxabox",
                                     title = "Taksonomisk utvalg",
                                     textOutput(ns("loc_spec_text")),
                                     uiOutput(ns("choose_project")),
                                     #uiOutput(ns("choose_region")),
                                     checkboxInput(ns("hab_split"),
                                                   label = "Del etter habitat (filtrer på region i figur)",
                                                   value = TRUE),
                                     #uiOutput(ns("choose_habitattype")),
                                     radioButtons(ns("unit_to_plot"),
                                                  label = "Måleenhet",
                                                  choices = c("Artsantall",
                                                              "Biomasse")),
                                     height = "600px",
                                     width = 12
             ),
             br(),
             box(width = 12,
                 title = "Tidstrend",
               plotlyOutput(ns("loc_spec"),
                        height = "300px"),
               height = "400px"
             )),
           column(6,
           
           box(width = 12,
               title = "Overvåkingslokaliteter",
               leaflet::leafletOutput(ns("loc_map"),
                                      width = "95%",
                                      height = 700),
               height = "800px"
           )
           )
  )
  
}



locspec_server <- function(id, login_import) {
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session) {
    
    
    output$loc_spec_text <- renderText("Her vises det totale antallet arter som er observert på hver lokalitet og en lokal polynomisk modell for trendene der det lar seg gjøre. For å kunne sammenligne resultatene vises kun resultater fra malaisefeller. Det går an å filtrere utvalget basert på regioner og habitatstyper, samt gjennom å zoome i kartet. Antallet arter som fanges varierer over tid og mellom plasser, og det vil ta tid for å kunne dra konklusjoner om stabile trender. Resultatene på disse figurer må tolkes med omhu. 
"
    )
    
    
    output$choose_project <- renderUI({
      con <- login_import$con()
      
      projects_tab <- tbl(con,
                          in_schema("lookup", "projects"))
      
      projects <- projects_tab %>% 
        select(project_name) %>% 
        distinct() %>% 
        filter(project_name %in% c("Nasjonal insektovervåking",
                                   "Tidlig varsling av fremmede arter",
                                   "Overvåking av insekter i hule eiker")
               ) %>% 
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
      
      
      # if(!is.null(input$region)){
      #   if(input$region != "All"){
      #     dat <- dat %>% 
      #       filter(region_name == input$region)
      #   }
      # }
      
   
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
             habitat_type,
             region_name) %>% 
      distinct(locality,
               year,
               habitat_type,
               region_name,
               .keep_all = T) %>% 
      collect()  
    
    
    pal_hab_cat <- isolate(ruter()$habitat_type) 
    pal_hab <- leaflet::colorFactor(palette = NinaR::ninaPalette(),
                                domain = pal_hab_cat)
   
    derived_pal_hab <- pal_hab(unique(pal_hab_cat)) 
    names(derived_pal_hab) <- unique(pal_hab_cat)
    
    pal_reg_cat <- isolate(ruter()$region_name) 
    pal_reg <- leaflet::colorFactor(palette = NinaR::ninaPalette(),
                                    domain = pal_reg_cat)
    
    derived_pal_reg <- pal_reg(unique(pal_reg_cat)) 
    names(derived_pal_reg) <- unique(pal_reg_cat)
    

    output$loc_map <- leaflet::renderLeaflet({
      req(input$project)
      #req(input$region)
      #req(input$habitat_type)
      
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
                             color = ~pal_hab(ruter()$habitat_type)) %>% 
        
        leaflet::addCircles(data = trap_points(),
                            popup = htmltools::htmlEscape(trap_points()$trap_name),
                            radius = ~trap_points()$coordinate_precision_m,
                            color = ~pal_hab(trap_points()$habitat_type)
        ) %>% 
        
        leaflet::addLegend(pal = pal_hab,
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
        sf::st_drop_geometry()}) 
      
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
      
      SELECT year, foo.locality, l.region_name, foo.habitat_type, no_spec as tot_spec
      FROM
      (SELECT locality,
      year, 
      habitat_type, 
      count(distinct(loc_spec.species_latin_gbif))::numeric no_spec
      FROM views.loc_traptype_species_list loc_spec
      WHERE trap_type = 'Malaise'
                              
      AND locality IN ('",
      paste(ruterInBounds()$locality, collapse = "','"),
      "') GROUP BY year, habitat_type, locality) foo,
      locations.localities l
      WHERE foo.locality = l.locality
           
      
      " )
     
      
      loc_species_res <- dbGetQuery(con,
                                    loc_species_q)  %>% 
        mutate(year = as.factor(year)) 

      return(loc_species_res)
      
    })
    
    
    loc_biomass_data <- reactive({
      
      ##Something like this...
      loc_biomass_q <- paste0("
       SELECT l.locality,
    yl.year,
    l.habitat_type,
    sum(st.wet_weight) as sum_wet_weight,
	avg(st.wet_weight / (EXTRACT(epoch FROM (ls.end_date - ls.start_date)/86400)::integer)) as wet_weight_per_day
   FROM 
    events.sampling_trap st,
    events.locality_sampling ls,
    events.year_locality yl,
    locations.localities l,
    locations.traps,
    lookup.trap_types tt
  WHERE st.locality_sampling_id = ls.id AND 
  ls.year_locality_id = yl.id AND 
  yl.locality_id = l.id AND 
  st.trap_id = traps.id AND 
  traps.trap_model = tt.trap_model
  AND tt.trap_type = 'Malaise'
  AND ls.end_date IS NOT NULL
  AND ls.start_date IS NOT NULL
  AND st.wet_weight IS NOT NULL
  GROUP BY l.locality, yl.year, l.habitat_type
  ORDER BY wet_weight_per_day ASC
  --LIMIT 100
  
                              
      AND locality IN ('",
                              paste(ruterInBounds()$locality, collapse = "','"),
                              "') GROUP BY year, habitat_type, locality) foo,
      locations.localities l
      WHERE foo.locality = l.locality
           
      
      " )
      
      
      loc_species_res <- dbGetQuery(con,
                                    loc_species_q)  %>% 
        mutate(year = as.factor(year)) 
      
      return(loc_species_res)
      
    })
    
    
    output$loc_spec <- renderPlotly({
      req(ns("loc_map"))
      req(ns("hab_split"))

      if(input$unit_to_plot == "Artsantall"){
        to_plot <- loc_species_data()
      } else to_plot <- loc_biomass_data()
      
      if(is.null(to_plot) | nrow(to_plot) <1 ) return(NULL)
      
      p <- ggplot2::ggplot(aes(y = tot_spec, 
                               x = year,
                               group = region_name, 
                               col = region_name),
                           data =  to_plot
                           ) +
       ggplot2::geom_point(position = position_dodge(width = 0.2)) +
       ggplot2::geom_smooth() +
        xlab("År") +
        ylab("Antall arter per lok.") +
        scale_color_manual(values = derived_pal_reg,
                            name = "Region")
      
      if(input$hab_split){
        
        p <- p +
          facet_wrap(facets = vars(habitat_type),
                     nrow = 1)
      }
      
      suppressWarnings(suppressMessages(
        ggplotly(p) %>%
        style(hovertext = loc_species_data()[, "locality"])
      ))
      
   
    })
    
  })
}
