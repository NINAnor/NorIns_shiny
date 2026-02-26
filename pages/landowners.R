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


landowners_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Grunneiere",
    column(
      6,
      box(
        width = 12,
        title = "Funn per lokalitet",
        textOutput(ns("landowner_text")),
        height = "500px"
      ),
      shinydashboardPlus::box(
        width = 12,
        id = "hotspotbox",
        title = "Lokalitetsvalg",
        fluidRow(column(
          6,
          uiOutput(ns("choose_loc"))
        ),
        column(
          6,
          downloadButton(ns("download_pdf"), "Last ned funnsrapport")
        )),
        height = "400px"
      )
    ),
    column(
      6,
      shinydashboardPlus::box(
        width = 12,
        id = "div_map_box",
        title = "Fordeling av diversitet",
        shinycssloaders::withSpinner(
          {
            leaflet::leafletOutput(ns("landowner_map"),
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



landowners_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    output$landowner_text <- renderText("Vi har satt sammen en kort rapport over hovedfunnene for hver lokalitet. Her kan grunneiere og andre interesserte lese mer om de seneste funnene fra hver plass vi har besøkt så langt.
    Rapportene er så langt bare tilgjengelige for lokalitetene i  Norsk insektovervåking.")

  get_loc_choices <- function(){
    #con <- login_import$con()
    
    loc_q <- "
    SELECT l.locality, 
    l.id as locality_id, 
    ST_Transform(ST_Centroid(l.geom), 4326) as geom
    FROM (SELECT distinct locality_id
    FROM events.year_locality 
    WHERE project_short_name = 'NorIns') yl,
    locations.localities l,
    landowners.landowner_reports lr
    WHERE yl.locality_id = l.id
    AND lr.locality_id = yl.locality_id
    ORDER BY split_part(l.locality, '_', 1), split_part(l.locality, '_', 2)::numeric
    "
    out <- sf::st_read(login_import$con,
                      query = loc_q)
    return(out)
  }   
    
  loc_data <- reactive({
    get_loc_choices()
  })
  
    output$choose_loc <- renderUI({
      tt <- loc_data() |> 
        sf::st_drop_geometry() 
      
      loc_choices <- tt |> 
        select(locality_id) |> 
        pull()
      
      names(loc_choices) <- tt |> 
        select(locality) |> 
        pull()
      

      selectInput(ns("chosen_loc"),
        label = "Velg lokalitet",
        choices = c("Velg lokalitet..." = "", loc_choices),
        selected = "",
        selectize = TRUE
      )
    })


    norge <- get_map()

    basemap <- leaflet(
      width = "300px",
      height = "200px"
    ) %>%
      addTiles(group = "OpenStreetMap")


    output$landowner_map <- renderLeaflet({
      to_plot <- loc_data()
      
      basemap %>%
        leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Ortophoto") %>%
        leaflet::addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
        leaflet::addLayersControl(
          overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::hideGroup(c("Topo", "Ortophoto")) %>%
        # Set the initial view to show all of Norway
        # (Approximate bounds for the Norwegian mainland)
        leaflet::fitBounds(lng1 = 4.0, lat1 = 57.5, lng2 = 31.5, lat2 = 71.5) %>% 
        leaflet::addCircleMarkers(
          data = to_plot,
          layerId = ~locality_id,
          popup = ~paste0(locality),
          radius = 4,        # Slightly larger for visibility on a national map
          color = "#E57200",
          fillOpacity = 0.8
        )
    })
    
    
    get_pdf_from_db <- function(locality_id) {
      # Parameterized query to prevent SQL injection
      query <- "SELECT pdf_content 
                FROM landowners.landowner_reports 
                WHERE locality_id = $1
                ORDER BY report_year DESC
                LIMIT 1"
      
      res <- dbGetQuery(login_import$con, query, params = list(locality_id))
      
      if (nrow(res) == 0) return(NULL)
      
      # Return the raw binary vector
      return(res$pdf_content[[1]])
    }
    
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        # 1. Get the data from your reactive
        df <- loc_data() |> sf::st_drop_geometry()
        
        # 2. Find the row where locality_id matches the input
        chosen_name <- df |> 
          filter(locality_id == input$chosen_loc) |> 
          pull(locality) |> 
          head(1)
        
        # 3. Handle potential empty results and create the filename
        if(length(chosen_name) == 0) chosen_name <- "rapport"
        
        # Replace spaces or special chars to be safe for file systems
        clean_name <- gsub("[^[:alnum:]]", "_", chosen_name)
        paste0(clean_name, ".pdf")
      },
      content = function(file) {
        # Use req() to stop execution if no location is selected
        req(input$chosen_loc)
        
        # 1. Manually checkout a connection from the pool to be safe
        # This often solves the "Checked-out object deleted" error in streams
        #conn <- pool::poolCheckout(login_import$con)
        
        # Ensure it gets returned even if the code fails
        #on.exit(pool::poolReturn(conn))
        
        # 2. Fetch the data using the checked-out 'conn'
        query <- "SELECT pdf_content 
              FROM landowners.landowner_reports 
              WHERE locality_id = $1
              ORDER BY report_year DESC
              LIMIT 1"
        
        res <- dbGetQuery(login_import$con, query, params = list(input$chosen_loc))
        
        if (nrow(res) == 0) {
          showNotification("Ingen rapport funnet for denne lokaliteten.", type = "error")
          return(NULL)
        }
        
        # 3. Write the raw bytes
        writeBin(res$pdf_content[[1]], file)
      },
      contentType = "application/pdf"
    )
    
    
    # 1. Initialize the flag (Inside moduleServer)
    is_map_click <- reactiveVal(FALSE)
    
    # 2. Update the flag when the map is clicked
    observeEvent(input$landowner_map_marker_click, {
      # Set the flag to TRUE because we are about to trigger a change
      is_map_click(TRUE) 
      
      updateSelectInput(session, "chosen_loc", 
                        selected = input$landowner_map_marker_click$id)
    })
    
    # 3. The Zoom Observer (The "Gatekeeper")
    observeEvent(input$chosen_loc, {
      req(input$chosen_loc)
      req(loc_data())
      
      # ONLY zoom if this change was NOT triggered by a map click
      if (!is_map_click()) {
        
        selected_point <- loc_data() %>% 
          filter(locality_id == input$chosen_loc)
        
        if (nrow(selected_point) > 0) {
          coords <- sf::st_coordinates(selected_point)
          
          leafletProxy("landowner_map", session) %>%
            flyTo(
              lng = as.numeric(coords[1, 1]), 
              lat = as.numeric(coords[1, 2]), 
              zoom = 12
            )
        }
      }
      
      # ALWAYS reset the flag to FALSE at the end of the observer
      # so that the next manual dropdown change WILL trigger a zoom.
      is_map_click(FALSE)
    }, ignoreInit = TRUE)
    
  })
}
