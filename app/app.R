require(shiny)
require(shinydashboard)

# load module functions
#source("./prep_data_for_shiny.R", local = TRUE)
source("./pages/feltarbeid.R", local = TRUE)
source("./pages/labarbeid.R", local = TRUE)
source("./pages/bioinformatikk.R", local = TRUE)
source("./pages/artsmangfold.R", local = TRUE)
source("./pages/div_map.R", local = TRUE)
source("./pages/asv_map.R", local = TRUE)
source("./pages/tidstrender.R", local = TRUE)
source("./pages/dashboard.R", local = TRUE)
source("./pages/landowners.R", local = TRUE)


# To make the app find the figures folder (and expose it to the web)
addResourcePath(prefix = "figures", directoryPath = "./figures")


conn_pool <- NULL
db_error_msg <- NULL

tryCatch({
  # 1. Create the pool with a short connection timeout
  conn_pool <- pool::dbPool(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    bigint   = "integer",
    connect_timeout = 5 # Timeout in seconds to prevent Docker freezes
  )
  
  # 2. Force an IMMEDIATE connection check (Checkout & Return)
  conn <- pool::poolCheckout(conn_pool)
  pool::poolReturn(conn)
  
  # Lock the reference directly into the onStop environment
  local({
    p <- conn_pool
    onStop(function() {
      if (!is.null(p) && pool::dbIsValid(p)) {
        pool::poolClose(p)
      }
    })
  })
  
}, error = function(e) {
  db_error_msg <<- e$message
  message("Database connection error: ", e$message)
})

login_export <- list(
  con = conn_pool
)

# Set up master ui function, fetching module ui-functions and defining ids
ui <- function(request){
  # If DB failed, replace navbar with a full-screen block error page
  if (!is.null(db_error_msg)) {
    return(
      fluidPage(
        style = "margin-top: 50px;",
        div(
          class = "alert alert-danger",
          role = "alert",
          h4(icon("exclamation-triangle"), " Severe Application Error: Database Unavailable"),
          p("The application failed to connect to the backend database on startup."),
          hr(),
          p(tags$strong("Details: "), tags$code(db_error_msg)),
          p(style = "margin-top: 15px; font-size: 0.9em;", 
            "If this container was just deployed, check Docker environment variables, network bridges, or firewall settings.")
        )
      )
    )
  }
  
  navbarPage(
  title = "Norsk insektovervåking - et innblikk",
  footer = NULL,
  header = NULL,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  felt_ui(id = "id_1"),
  labarbeid_ui(id = "id_2"),
  bioinformatikk_ui(id = "id_3"),
  biodiv_ui(id = "id_4"),
  dashboard_ui(id = "id_8"),
  div_map_ui(id = "id_5"),
  tidstrend_ui(id = "id_7"),
  asvmap_ui(id = "id_6"),
  landowners_ui(id = "id_9"),
  uiOutput("db_status_ui")
)
}

# Set up master server function, fetching module server-functions and defining ids. Database connection is made once, and shared though modules
server <- function(input, output, session) {
  
  output$db_status_ui <- renderUI({
    if (!is.null(db_error_msg)) {
      div(class = "db-err-banner",
          icon("exclamation-triangle"),
          " CRITICAL DATABASE ERROR: ",
          br(), br(),
          db_error_msg
      )
    }
  })
  
  # Also trigger a persistent toast notification on startup
  observe({
    if (!is.null(db_error_msg)) {
      showNotification(
        db_error_msg, 
        type = "error", 
        duration = NULL # Keep visible until dismissed
      )
    }
  })
  
  felt_server(id = "id_1")

  labarbeid_server(id = "id_2")

  bioinformatikk_server(id = "id_3")

  biodiv_server(id = "id_4")

  div_map_server(id = "id_5", login_import = login_export)

  asvmap_server(id = "id_6", login_import = login_export)

  tidstrend_server(id = "id_7", login_import = login_export)

  dashboard_server(id = "id_8", login_import = login_export)
  
  landowners_server(id = "id_9", login_import = login_export)
}

shinyApp(ui = ui, server = server)
