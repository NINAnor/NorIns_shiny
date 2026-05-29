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

tidstrend_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Tidstrender",
    column(
      6,
      shinydashboardPlus::box(
        id = "taxabox",
        title = "Utvalg",
        textOutput(ns("loc_spec_text")),
        uiOutput(ns("choose_project")),
        # uiOutput(ns("choose_region")),
        fluidRow(
          column(
            5,
            checkboxInput(ns("only_summer"),
              label = "Vis kun data mellom juni-august",
              value = TRUE
            )
          ),
          column(
            5,
            checkboxInput(ns("hab_split"),
              label = "Del etter habitat (filtrer på region i figur)",
              value = FALSE
            )
          )
        ),
        # uiOutput(ns("choose_habitattype")),
        radioButtons(ns("unit_to_plot"),
          label = "Måleenhet",
          choices = c(
            "Biomasse",
            "Artsantall"
          ),
          selected = "Biomasse",
          inline = TRUE
        ),
        height = "600px",
        width = 12
      ),
      br(),
      shinydashboardPlus::box(
        width = 12,
        id = "locspecbox",
        title = "Tidstrend",
        shinycssloaders::withSpinner(
          {
            plotOutput(ns("loc_spec2"),
              height = "300px"
            )
          },
          type = 2,
          color = "#E57200",
          color.background = "#004F71"
        ),
        height = "400px"
      )
    ),
    column(
      6,
      shinydashboardPlus::box(
        width = 12,
        id = "loc_map_box",
        title = "Overvåkingslokaliteter",
        shinycssloaders::withSpinner(
          {
            leaflet::leafletOutput(ns("loc_map"),
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



tidstrend_server <- function(id, login_import) {
  ns <- NS(id)

  moduleServer(id, function(input, output, session) {
    output$loc_spec_text <- renderText("Her kan man se tidstrender for den totale mengden insekter eller det totale antallet insektarter som programmet har oppmålt. Zoom i kartet for å filtrere på geografisk region. Mengden insekter vises som gjennomsnittlig biomasse per felle og dag, og artsantallet vises som gjennomsnittligt artsantall per lokalitet og år. For å få en lik innsamlingsinnsats for alle habitater er kun data fra malaisefeller lagt inn. Den totale fangstperioden varierer noe mellom år, derfor bør man i første hånd sammenligne data kun for juni-august, der vi har er en sammenlignbar innsamlingsinnsats. I 2020 brukte vi en eldre sekvenseringsmaskin, og derfor er artsantallet lavere for dette året. Det vil ta tid for å kunne dra konklusjoner om stabile trender, både mellom år og mellom steder. Resultatene på disse figurene må derfor tolkes med omhu. Enn så lenge har ikke noen lokalitet blitt undersøkt mer enn en gang, og det vil i ta minst 10 år før alle lokaliteter har blitt undersøkt to ganger.")


    # Project selection dropdown UI
    output$choose_project <- renderUI({
      selectInput(
        inputId = ns("project"),
        label = "Prosjekt",
        choices = c("Norsk insektovervåking" = "NorIns",
                    "Tidlig varsling av fremmede arter" = "TidVar"),
        selected = "Norsk insektovervåking",
        selectize = FALSE
      )
    })
    
    # Reactive Spatial Point Data Fetcher
    trap_points <- reactive({
      req(input$project)
      
      trap_sql <- "
        SELECT yl.project_short_name,
        trap.trap_name,
        loc.locality,
        trap.year,
        trap_short_name,
        trap_model,
        liquid_name,
        coordinate_precision_m,
        elev_m,
        loc.habitat_type,
        ST_AsText(st_transform(trap.geom, 4326)) as point_geom,
        loc.region_name,
        ST_AsText(st_transform(loc.geom, 4326)) as rute_geom
        FROM locations.traps trap,
        locations.localities loc,
        events.year_locality yl,
        lookup.projects
        WHERE trap.locality_id = loc.id
        AND yl.locality_id = loc.id
        AND yl.project_short_name = projects.project_short_name
        AND projects.project_short_name = ?id1
        "
      
      trap_sql_san <- DBI::sqlInterpolate(login_import$con,
                                          trap_sql,
                                          id1 = input$project)
      
      res <- dbGetQuery(login_import$con, trap_sql_san)
      req(nrow(res) > 0)
      
     
      dat <- sf::st_as_sf(res,
                          wkt = "point_geom",
                          crs = 4326) %>%
        mutate(
          habitat_type = ifelse(habitat_type == "Forest", "Skogsmark", habitat_type),
          habitat_type = ifelse(habitat_type == "Semi-nat", "Jordbruksmark", habitat_type)
        )
      
      dat$rute_geom  <- sf::st_as_sfc(dat$rute_geom, crs = 4326)
      
      return(dat)
    })
    
    # Reactive Polygon Data Fetcher
    ruter <- reactive({
      req(trap_points())
      
      trap_points() %>%
        # 1. Switch active geometry to rute_geom
        sf::st_set_geometry("rute_geom") %>%
        # 2. FIX: Explicitly rename and keep the polygon geometry column!
        dplyr::select(locality, year, habitat_type, region_name, geometry = rute_geom) %>%
        # 3. Drop duplicate records
        dplyr::distinct(locality, year, habitat_type, region_name, .keep_all = TRUE) %>%
        dplyr::mutate(
          habitat_type = ifelse(habitat_type == "Forest", "Skogsmark", habitat_type),
          habitat_type = ifelse(habitat_type == "Semi-nat", "Jordbruksmark", habitat_type)
        ) %>%
        # 4. Enforce that it explicitly retains its spatial properties
        sf::st_as_sf(crs = 4326) %>%
        collect()
    })
    
    # Dynamic Map Color Palettes
    pal_hab <- reactive({
      req(ruter())
      leaflet::colorFactor(
        palette = NinaR::ninaPalette(),
        domain = ruter()$habitat_type
      )
    })
    
    pal_reg <- reactive({
      req(ruter())
      leaflet::colorFactor(
        palette = NinaR::ninaPalette(),
        domain = ruter()$region_name
      )
    })
    
    # Leaflet Output Rendering
    output$loc_map <- leaflet::renderLeaflet({
      req(input$project)
      map_ruter <- ruter()
      map_traps <- trap_points()
      color_provider <- pal_hab()
      
      leaflet::leaflet(width = "70%", height = 800) %>%
        leaflet::addTiles(group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Ortophoto") %>%
        leaflet::addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
        leaflet::addLayersControl(
          overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::hideGroup(c("Topo", "Ortophoto")) %>%
        leaflet::addPolygons(
          data = map_ruter,
          color = ~ color_provider(habitat_type)
        ) %>%
        leaflet::addCircles(
          data = map_traps,
          popup = htmltools::htmlEscape(map_traps$trap_name),
          radius = ~ coordinate_precision_m,
          color = ~ color_provider(habitat_type)
        ) %>%
        leaflet::addLegend(
          pal = color_provider,
          values = map_ruter$habitat_type,
          position = "bottomright",
          title = "Habitat type",
          labFormat = leaflet::labelFormat(digits = 1),
          opacity = 1
        )
    })
    
    # Map Boundary Tracking
    ruterInBounds <- reactive({
      req(ruter())
      
      if (is.null(input$loc_map_bounds)) {
        return(ruter() %>% sf::st_drop_geometry() %>% dplyr::select(locality))
      }
      
      bounds <- input$loc_map_bounds %>% as.list()
      latRng <- range(bounds$south, bounds$north)
      lngRng <- range(bounds$west, bounds$east)
      
      suppressWarnings({
        ruter_df <- ruter() %>%
          dplyr::mutate(
            lon = sf::st_coordinates(sf::st_centroid(geometry))[, 1],
            lat = sf::st_coordinates(sf::st_centroid(geometry))[, 2]
          ) %>%
          sf::st_drop_geometry()
      })
      
      out <- ruter_df %>%
        filter(lat >= latRng[1] & lat <= latRng[2] &
                 lon >= lngRng[1] & lon <= lngRng[2]) %>%
        dplyr::select(locality) %>%
        as_tibble()
      
      return(out)
    })
    
    # Data Loads
    load(file = "data/biomass_mf_locality_sampling_time.Rdata")
    load(file = "data/diversity_locality_sampling_time.Rdata")
    
    biomass_mf_locality_sampling_time <- biomass_mf_locality_sampling_time %>%
      mutate(habitat_no = ifelse(habitat_type == "Semi-nat", "Gressmark", habitat_type)) %>%
      mutate(
        habitat_no = factor(habitat_no, levels = c("Gressmark", "Skog", "Tid_aut", "Tid_man")),
        region_name = factor(region_name, levels = c("Østlandet", "Trøndelag", "Sørlandet", "Vestlandet", "Nord-Norge")),
        habitat_type = as.factor(habitat_type)
      ) %>%
      mutate(year = as.factor(year))
    
    diversity_locality_sampling_time <- diversity_locality_sampling_time %>%
      mutate(habitat_no = ifelse(habitat_type == "Semi-nat", "Gressmark", "Skog")) %>%
      mutate(
        habitat_no = factor(habitat_no, levels = c("Gressmark", "Skog")),
        region_name = factor(region_name, levels = c("Østlandet", "Trøndelag", "Sørlandet", "Vestlandet", "Nord-Norge")),
        habitat_type = as.factor(habitat_type)
      ) %>%
      mutate(year = as.factor(year))
    
    # Reactive Plot Computations
    to_plot_biomass <- reactive({
      req(ruterInBounds())
      temp <- biomass_mf_locality_sampling_time %>%
        filter(locality %in% ruterInBounds()$locality)
      
      if (input$only_summer) {
        temp <- temp %>% filter(start_month >= 7, start_month <= 8)
      }
      
      temp <- temp %>%
        group_by(year, region_name, habitat_type) %>%
        summarise(
          mean_biomass = mean(avg_wet_weight / no_trap_days, na.rm = TRUE),
          sd_biomass = sd(avg_wet_weight / no_trap_days, na.rm = TRUE),
          n = n(),
          se_biomass = sd_biomass / sqrt(n),
          .groups = "drop"
        ) %>%
        mutate(year = as.factor(year), region_name = as.factor(region_name), habitat_type = as.factor(habitat_type))
      
      return(temp)
    })
    
    to_plot_species <- reactive({
      req(ruterInBounds())
      temp <- diversity_locality_sampling_time %>%
        filter(locality %in% ruterInBounds()$locality)
      
      if (input$only_summer) {
        temp <- temp %>% filter(start_month >= 7, start_month <= 8)
      }
      
      temp <- temp %>%
        group_by(year, region_name, habitat_type) %>%
        summarise(
          mean_richness = mean(no_species, na.rm = TRUE),
          sd_richness = sd(no_species, na.rm = TRUE),
          n = n(),
          se_richness = sd_richness / sqrt(n),
          sum_richness = sum(no_species, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(year = as.factor(year), region_name = as.factor(region_name), habitat_type = as.factor(habitat_type))
      
      return(temp)
    })
    
    # Render Time Trend Plot
    output$loc_spec2 <- renderPlot({
      req(input$unit_to_plot)
      
      dodge <- 0.2
      
      if (input$unit_to_plot == "Artsantall") {
        to_plot <- to_plot_species()
        req(nrow(to_plot) > 0)
        yvar <- data_sym("mean_richness")
        yse <- data_sym("se_richness")
        ylab <- "Middelv. antall arter per lokalitet og år"
        ciMult <- qt(.975, to_plot$n - 1)
      } else {
        to_plot <- to_plot_biomass()
        req(nrow(to_plot) > 0)
        yvar <- data_sym("mean_biomass")
        yse <- data_sym("se_biomass")
        ylab <- "Middelv. biomasse per dag (g/dag)"
        ciMult <- qt(.975, to_plot$n - 1)
      }
      
      p <- ggplot(to_plot, aes(x = year, y = !!yvar, group = region_name:habitat_type)) +
        geom_point(aes(color = region_name, pch = habitat_type), cex = 3, position = position_dodge(width = dodge)) +
        geom_errorbar(aes(ymin = !!yvar - ciMult * !!yse, ymax = !!yvar + ciMult * !!yse, color = region_name), width = .2, lwd = 1, position = position_dodge(width = dodge)) +
        geom_line(aes(color = region_name), position = position_dodge(width = dodge)) +
        scale_color_nina(name = "Region", palette = "darkblue-orange") +
        scale_shape_discrete(name = "Habitattype") +
        ylab(ylab) + xlab("År") +
        theme(legend.position = "right")
      
      if (input$hab_split) {
        p <- p + facet_wrap(facets = vars(habitat_type), nrow = 1)
      }
      
      p
    })
  })
}