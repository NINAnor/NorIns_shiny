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
    "
    out <- sf::st_read(con,
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
        choices = loc_choices,
        # selected = "",
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
      #req(input$chosen_group)
      # req(input$species_filter)

      to_plot <- loc_data()
      
      my_icon <- icons(
        iconUrl = "./figures/trap_icon.png",
        iconWidth = 32,
        iconHeight = 32
      )

      basemap %>%
        leaflet::addProviderTiles(providers$Esri.WorldImagery,
          group = "Ortophoto"
        ) %>%
        leaflet::addProviderTiles(providers$OpenTopoMap,
          group = "Topo"
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = c("OpenStreetMap", "Topo", "Ortophoto"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::hideGroup(c("Topo", "Ortophoto")) %>%
        leaflet::addMarkers(
          data = to_plot,
          popup = ~ paste0(locality),
          icon = my_icon
        )
      
    })
    
    
    get_pdf_from_db <- function(locality_id) {
      # Parameterized query to prevent SQL injection
      query <- "SELECT pdf_content 
                FROM landowners.landowner_reports 
                WHERE locality_id = $1
                ORDER BY report_year DESC
                LIMIT 1"
      
      res <- dbGetQuery(con, query, params = list(locality_id))
      
      if (nrow(res) == 0) return(NULL)
      
      # Return the raw binary vector
      return(res$pdf_content[[1]])
    }
    
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        # It's better to use a clean name
        paste0("Rapport_", input$chosen_loc, ".pdf")
      },
      content = function(file) {
        # Use req() to stop execution if no location is selected
        req(input$chosen_loc)
        
        # 1. Manually checkout a connection from the pool to be safe
        # This often solves the "Checked-out object deleted" error in streams
        #conn <- pool::poolCheckout(con)
        
        # Ensure it gets returned even if the code fails
        #on.exit(pool::poolReturn(conn))
        
        # 2. Fetch the data using the checked-out 'conn'
        query <- "SELECT pdf_content 
              FROM landowners.landowner_reports 
              WHERE locality_id = $1
              ORDER BY report_year DESC
              LIMIT 1"
        
        res <- dbGetQuery(con, query, params = list(input$chosen_loc))
        
        if (nrow(res) == 0) {
          showNotification("Ingen rapport funnet for denne lokaliteten.", type = "error")
          return(NULL)
        }
        
        # 3. Write the raw bytes
        writeBin(res$pdf_content[[1]], file)
      },
      contentType = "application/pdf"
    )
    
    
  })
}
