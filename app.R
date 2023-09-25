require(shiny)
require(shinydashboard)
#require(sf)



# load module functions
source("pages/feltarbeid.R", local = TRUE)
source("pages/labarbeid.R", local = TRUE)
source("pages/bioinformatikk.R", local = TRUE)
source("pages/artsmangfold.R", local = TRUE)
source("pages/tidsserier.R", local = TRUE)
source("pages/asv_map.R", local = TRUE)
source("pages/local_spec.R", local = TRUE)
source("pages/dashboard.R", local = TRUE)



#To make the app find the figures folder (and expose it to the web)
addResourcePath(prefix = "figures", directoryPath = "figures")


#Set up master ui function, fetching module ui-functions and defining ids
ui <- navbarPage(title = "Norsk insektovervåking - et innblikk",
                 footer = NULL,
                 header = NULL,
                 tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                 ),
                 
                 felt_ui(id = "id_1"),
                 
                 dashboard_ui(id = "id_8"),
 
                 labarbeid_ui(id = "id_2"),
                 
                 bioinformatikk_ui(id = "id_3"),
                 
                 biodiv_ui(id = "id_4"),
                 
                 tidsserier_ui(id = "id_5"),
                 
                 locspec_ui(id = "id_7"),
                 
                 asvmap_ui(id = "id_6")
                 
                 
                 
                 
                 
)


#Set up master server function, fetching module server-functions and defining ids. Database connection is made once, and shared though modules
server <- function(input, output, session) {
  
  login_export <- felt_server(id = "id_1")
  
  labarbeid_server(id = "id_2")
  
  bioinformatikk_server(id = "id_3")
  
  biodiv_server(id = "id_4")
  
  tidsserier_server(id = "id_5")
  
  asvmap_server(id = "id_6", login_import = login_export)
  
  locspec_server(id = "id_7", login_import = login_export)
  
  dashboard_server(id = "id_8", login_import = login_export)
  
}



shinyApp(ui = ui, server = server)

