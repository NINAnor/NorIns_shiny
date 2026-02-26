require(shiny)
require(shinydashboard)

# load module functions
source("prep_data_for_shiny.R", local = TRUE)
source("pages/feltarbeid.R", local = TRUE)
source("pages/labarbeid.R", local = TRUE)
source("pages/bioinformatikk.R", local = TRUE)
source("pages/artsmangfold.R", local = TRUE)
source("pages/div_map.R", local = TRUE)
source("pages/asv_map.R", local = TRUE)
source("pages/tidstrender.R", local = TRUE)
source("pages/dashboard.R", local = TRUE)
source("pages/landowners.R", local = TRUE)



# To make the app find the figures folder (and expose it to the web)
addResourcePath(prefix = "figures", directoryPath = "figures")

load("data/shinyPass.Rdata")

conn_pool <- pool::dbPool(RPostgres::Postgres(),
                    host = "t2lippgsql03.nina.no",
                    dbname = "insect_monitoring",
                    user = my_username,
                    password = my_password)


#onStop(function() {
#   pool::poolClose(conn_pool)
#})

login_export <- list(
  con = conn_pool
)

# Set up master ui function, fetching module ui-functions and defining ids
ui <- navbarPage(
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
  landowners_ui(id = "id_9")
)


# Set up master server function, fetching module server-functions and defining ids. Database connection is made once, and shared though modules
server <- function(input, output, session) {
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
