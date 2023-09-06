require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(shinydashboard)


dashboard_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Dashboard",
           fluidRow(
             valueBoxOutput(ns("no_loc_semi"),
                            width = 2),
             valueBoxOutput(ns("no_loc_forest"),
                            width = 2),
             valueBoxOutput(ns("no_sampl"),
                            width = 2)
           ),
           
           column(6,
                  box(width = 12,
                      title = "Prøvetaking",
                      height = "400px",
                     )
           
                  ,
             br(),
             box(width = 12,
                 title = "Taksonomisk fordeling",
                 # uiOutput(ns("tax_perc"),
                 #          height = "300px"),
                 height = "400px"
             )),
           column(6,
              box(width = 12,
                  title = "Funn i et nøtteskall",
                  # uiOutput(ns("agg_res"),
                  #          height = "300px"),
                  height = "400px"
                  )
           )
  )
  
}



dashboard_server <- function(id, login_import) {
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session) {
    
    
    
    
    
    
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
    
      
    no_loc <- reactive({
      con <- login_import$con()
      
      no_loc_q <- "
      SELECT habitat_type, count(distinct(yl.locality_id))::numeric no_loc
      FROM events.year_locality yl,
      locations.localities l
      WHERE yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      GROUP BY l.habitat_type
     
      "
      
      no_loc <- dbGetQuery(con,
                           no_loc_q)
      
      return(no_loc)
      
    })
    
    
    output$no_loc_semi <- renderValueBox({
    no_loc <- no_loc()
      
      valueBox(
        value = no_loc[no_loc$"habitat_type" == "Semi-nat",]$no_loc,
        subtitle = "Antall lokaliteter i semi-nat",
        color = "yellow",
        width = 2
      )
    })
   

    output$no_loc_forest <- renderValueBox({
      no_loc <- no_loc()
      
      valueBox(
        value = no_loc[no_loc$"habitat_type" == "Forest",]$no_loc,
        subtitle = "Antall lokaliteter i skog",
        color = "aqua"
      )
    })

    
    no_sampl <- reactive({
      
    con <- login_import$con()
    
    no_sampl_q <- "
      SELECT count(st.*)::numeric no_sampl
      FROM events.year_locality yl,
      events.locality_sampling ls,
      events.sampling_trap st
      WHERE yl.id = ls.year_locality_id
      AND ls.id = st.locality_sampling_id
      AND yl.project_short_name = 'NasIns'
      AND ls.end_date IS NOT NULL
      "
    
    no_sampl <- dbGetQuery(con,
                         no_sampl_q)
    return(no_sampl)
    })
    
    
    output$no_sampl <- renderValueBox({
      no_sampl <- no_sampl()
      
      valueBox(
        value = no_sampl$no_sampl,
        subtitle = "Antall felleprøver",
        color = "aqua"
      )
    })
    
    
})
}
