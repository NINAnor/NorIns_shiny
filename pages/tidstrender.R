
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
require(sf)

tidstrend_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Tidstrender",
           column(6,
             shinydashboardPlus::box(id = "taxabox",
                                     title = "Utvalg",
                                     textOutput(ns("loc_spec_text")),
                                     uiOutput(ns("choose_project")),
                                     #uiOutput(ns("choose_region")),
                                     fluidRow(
                                       column(5,
                                     checkboxInput(ns("only_summer"),
                                                   label = "Vis kun data mellom juni-august",
                                                   value = TRUE)
                                     ),
                                     column(5,
                                     checkboxInput(ns("hab_split"),
                                                   label = "Del etter habitat (filtrer på region i figur)",
                                                   value = FALSE)
                                     )
                                     ),
                                     #uiOutput(ns("choose_habitattype")),
                                     radioButtons(ns("unit_to_plot"),
                                                  label = "Måleenhet",
                                                  choices = c("Biomasse",
                                                              "Artsantall"),
                                                  selected = "Biomasse",
                                                  inline = TRUE),
                                     height = "600px",
                                     width = 12
             ),
             br(),
             shinydashboardPlus::box(width = 12,
                                        id = "locspecbox",
                                        title = "Tidstrend",
                                        shinycssloaders::withSpinner({
                                        plotOutput(ns("loc_spec2"),
                                                  height = "300px")
                 },
               type = 2,
               color = "#E57200",
               color.background = "#004F71"),
               height = "400px"
             )),
           column(6,
               shinydashboardPlus::box(width = 12,
                                       id = "loc_map_box",
               title = "Overvåkingslokaliteter",
               shinycssloaders::withSpinner({
               leaflet::leafletOutput(ns("loc_map"),
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



tidstrend_server <- function(id, login_import) {
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session) {
    
    
    output$loc_spec_text <- renderText("Her kan man se det totale artsantallet funnet per lokalitet på kart, og som en modell med snitt og variasjon per år nederst. Du bestemmer selv om du vil ha alle regioner og økosystemer. Det er bare data fra malaisefeller som er lagt inn. Det vil ta tid for å kunne dra konklusjoner om stabile trender, både mellom år og mellom steder. Resultatene på disse figurene må derfor tolkes med omhu. Ingen lokaliteter har per i dag blitt undersøkt mer enn en gang, og det vil i ta minst 10 år før alle lokaliteter har blitt undersøkt to ganger."
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
                  choices = c("Nasjonal insektovervåking"),
                  #choices = c("", projects$project_name),
                  selected = "Nasjonal insektovervåking",
                  selectize = FALSE)
      
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
                         query = trap_sql) %>% 
        mutate(habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type))
      
      
      ##Add lon lat from geom
      
      if(!is.null(input$project)){
        if(input$project != ""){
          dat <- dat %>% 
            filter(project_name == input$project)
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
      mutate(habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type)) %>% 
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
      #req(exists("input$only_summer"))
      #req(exists("input$hab_split"))
      
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
    
    
    load(file = "data/biomass_mf_locality_sampling_time.Rdata")
    load(file = "data/diversity_locality_sampling_time.Rdata")
    #load(file = "data/diversity_locality_sampling_time_id_high.Rdata")
    
  
    biomass_mf_locality_sampling_time <- biomass_mf_locality_sampling_time %>%   
      mutate(habitat_no = ifelse(habitat_type == "Semi-nat", "Gressmark", "Skog")) %>% 
      mutate(habitat_no = factor(habitat_no, levels = c("Gressmark", "Skog")),
             region_name = factor(region_name, levels = c("Østlandet", "Trøndelag", "Sørlandet", "Nord-Norge")),
             habitat_type = as.factor(habitat_type)) %>% 
      mutate(year = as.factor(year)) 
    
    diversity_locality_sampling_time <- diversity_locality_sampling_time %>% 
      mutate(habitat_no = ifelse(habitat_type == "Semi-nat", "Gressmark", "Skog")) %>% 
      mutate(habitat_no = factor(habitat_no, levels = c("Gressmark", "Skog")),
             region_name = factor(region_name, levels = c("Østlandet", "Trøndelag", "Sørlandet", "Nord-Norge")),
             habitat_type = as.factor(habitat_type)) %>% 
      mutate(year = as.factor(year)) 
    
    # diversity_locality_sampling_time_id_high <- diversity_locality_sampling_time_id_high %>%
    #   mutate(habitat_no = ifelse(habitat_type == "Semi-nat", "Gressmark", "Skog")) %>%
    #   mutate(habitat_no = factor(habitat_no, levels = c("Gressmark", "Skog")),
    #          region_name = factor(region_name, levels = c("Østlandet", "Trøndelag", "Sørlandet", "Nord-Norge")),
    #          habitat_type = as.factor(habitat_type)) %>%
    #   mutate(year = as.factor(year))
    
    
    to_plot_biomass <- reactive({
      temp <- biomass_mf_locality_sampling_time %>% 
      filter(locality %in% ruterInBounds()$locality)

      if(input$only_summer){
        temp <- temp %>%
          filter(start_month >=7,
                 start_month <=8)
      }
      
      temp <- temp %>% 
      group_by(year, 
               region_name,
               habitat_type) %>% 
      summarise(mean_biomass = mean(avg_wet_weight / no_trap_days, na.rm = TRUE),
                sd_biomass = sd(avg_wet_weight / no_trap_days, na.rm = TRUE),
                n = n(),
                se_biomass = sd_biomass / sqrt(n),
                .groups = "drop") %>% 
      mutate(year = as.factor(year),
             region_name = as.factor(region_name),
             habitat_type = as.factor(habitat_type))
      
      return(temp)
      
    })
    
    to_plot_species <- reactive({
        temp <- diversity_locality_sampling_time %>% 
        filter(locality %in% ruterInBounds()$locality)

        if(input$only_summer){
          temp <- temp %>%
            filter(start_month >=7,
                   start_month <=8)
        }

        temp <- temp %>% 
        group_by(year, 
                 region_name,
                 habitat_type) %>% 
        summarise(mean_richness = mean(no_species, na.rm = TRUE),
                  sd_richness = sd(no_species, na.rm = TRUE),
                  n = n(),
                  se_richness = sd_richness / sqrt(n),
                  sum_richness = sum(no_species, na.rm = TRUE),
                  .groups = "drop") %>% 
        mutate(year = as.factor(year),
               region_name = as.factor(region_name),
               habitat_type = as.factor(habitat_type))
      
        return(temp)
    })
    
    
    output$loc_spec2 <- renderPlot({
      req(ns("loc_map"))
      req(ns("hab_split"))
      req(ns("only_summer"))
      
      dodge = 0.2
      
      if(input$unit_to_plot == "Artsantall"){
        to_plot <- to_plot_species()
        yvar = data_sym("mean_richness")
        yse = data_sym("se_richness")
        ylab =  "Middelv. antall arter per lokalitet og år"
        ciMult <- qt(.975, to_plot$n - 1)
      } else 
      {
        to_plot <- to_plot_biomass()
        yvar = data_sym("mean_biomass")
        yse = data_sym("se_biomass")
        ylab = "Middelv. biomasse per dag (g/dag)"
        ciMult <- qt(.975, to_plot$n - 1)
      }
      
      p <- ggplot(to_plot,
                  aes(x = year,
                      y = !!yvar,
                      group = region_name:habitat_type)) +
      geom_point(aes(x = year,
                     y = !!yvar,
                     color = region_name,
                     pch = habitat_type),
                 cex = 3,
                 position = position_dodge(width = 0.2)) +
      geom_errorbar(aes(x = year,
                        ymin = !!yvar - ciMult * !!yse,
                        ymax = !!yvar + ciMult * !!yse,
                        color = region_name),
                    width = .2,
                    lwd = 1,
                    position=position_dodge(width = 0.2)
                    ) +
      geom_line(aes(x = year,
                    y = !!yvar,
                    color = region_name
      ),
      position=position_dodge(width = 0.2)
      ) +
      scale_color_nina(name = "Region",
                       palette = "darkblue-orange") +
      scale_shape_discrete(name = "Habitattype") +
      ylab(ylab) +
      xlab("År") +
      theme(legend.position = "right") 
      
      if(input$hab_split){
        
        p <- p +
          facet_wrap(facets = vars(habitat_type),
                     nrow = 1)
      }
      
      p
      # suppressWarnings(suppressMessages(
      #   ggplotly(p) #%>%
      #  #   style(hovertext = loc_species_data()[, "locality"])
      # ))
      
      
    
    })
    
  })
}
