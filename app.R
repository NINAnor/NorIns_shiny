require(shiny)
require(shinydashboard)
require(shiny.i18n)
require(shinyWidgets)



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


# File with translations
i18n <- Translator$new(translation_json_path = "./data/translation.json")
i18n$set_translation_language("no") # here you select the default translation to display

# To make the app find the figures folder (and expose it to the web)
addResourcePath(prefix = "figures", directoryPath = "figures")

# Set up master ui function, fetching module ui-functions and defining ids
ui <- navbarPage(
  title = uiOutput("main_title"),
  id = "banner",
  footer = NULL,
  header =  shiny.i18n::usei18n(i18n),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$div(
      style = "absolute: right;",
      radioGroupButtons(inputId = "selected_language",
                          label = i18n$t("Velg språk"),
                          choices = i18n$get_languages(),
                          selected = i18n$get_key_translation(),
      )
      )
    ),

  felt_ui(id = "id_1"),
  labarbeid_ui(id = "id_2"),
  bioinformatikk_ui(id = "id_3"),
  biodiv_ui(id = "id_4"),
  dashboard_ui(id = "id_8"),
  div_map_ui(id = "id_5"),
  tidstrend_ui(id = "id_7"),
  asvmap_ui(id = "id_6"),

)



# Set up master server function, fetching module server-functions and defining ids. Database connection is made once, and shared though modules
server <- function(input, output, session) {
  
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    #print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(input$selected_language)
  })
  
  choices_vals = c("no", "en")
  names(choices_vals) = c("Norsk insektovervåking", "Norwegian insectmonitoring")
  
  output$main_title <- renderUI(
    
    renderText(i18n$t("Norsk insektovervåking"))
  
  )

  login_export <- felt_server(id = "id_1")

  labarbeid_server(id = "id_2")

  bioinformatikk_server(id = "id_3")

  biodiv_server(id = "id_4")

  div_map_server(id = "id_5")

  asvmap_server(id = "id_6", login_import = login_export)

  tidstrend_server(id = "id_7", login_import = login_export)

  dashboard_server(id = "id_8", login_import = login_export)
  

}



shinyApp(ui = ui, server = server)
