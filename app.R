library(shiny)
library(shinydashboard)



# load module functions
source("pages/feltarbeid.R", local = TRUE)
source("pages/labarbeid.R", local = TRUE)
source("pages/bioinformatikk.R", local = TRUE)
source("pages/biodiversitet.R", local = TRUE)
source("pages/tidsserier.R", local = TRUE)
source("pages/asv_map.R", local = TRUE)


#To make the app find the figures folder (and expose it to the web)
addResourcePath(prefix = "figures", directoryPath = "figures")


ui <- navbarPage(title = "Norsk insektovervåking - en innblikk",
                 footer = NULL,
                 header = NULL,
                 tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                 ),
 
                 felt_ui(id = "id_1"),
                 
                 labarbeid_ui(id = "id_2"),
                 
                 bioinformatikk_ui(id = "id_3"),
                 
                 biodiv_ui(id = "id_4"),
                 
                 tidsserier_ui(id = "id_5"),
                 
                 asvmap_ui(id = "id_6")
                 
)



server <- function(input, output, session) {
  
  felt_server(id = "id_1")
  
  labarbeid_server(id = "id_2")
  
  bioinformatikk_server(id = "id_3")
  
  biodiv_server(id = "id_4")
  
  tidsserier_server(id = "id_5")
  
  asvmap_server(id = "id_6")
  
}



shinyApp(ui = ui, server = server)

