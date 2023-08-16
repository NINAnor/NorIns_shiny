
tidsserier_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Insekter i tid og rom",
           fluidRow(
             box(
               uiOutput(ns("tidsserie")),
               title = "Biomasse og artsantall",
               solidHeader = TRUE,
               height = "400px"
             ),
             box(
               textOutput(ns("tidsserie_text")),
               title = "Starten på en tidsserie",
               solidHeader = TRUE,
               height = "400px"
             )
           ),
           br(),
           fluidRow(
             box(
               uiOutput(ns("redlist")),
               title = "Rødlistede og ikke tidligere registrerte funn",
               background = "lime",
               solidHeader = TRUE,
               height = "400px"
             ),
             
             box(
               uiOutput(ns("betacom")),
               title = "Artsamfunn varierer med økosystem, og over tid",
               solidHeader = TRUE,
               height = "400px"
             )
           ))
  
}



tidsserier_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    output$tidsserie <- renderUI({
      tags$img(src = "figures/biomass_div_concat.png", 
               height = "300px")
    })
    
    
    output$tidsserie_text <- renderText("Dette prosjekt vil danne en standardisert tidserie som kan brukes for å undersøke endringer i insektfaunaen over tid. Mengdene insekter varierer ofte kraftig innen en sesong, og mellom år og mellom lokaliteter. Man skal derfor ikke forvente seg å statistisk sikre forandringer etter bare noen år, da dette ville kreve svært store forandringer for å vise seg. 

Overvåkingen kartlegger ikke bara forandringer, uten gir også verdifull kunnskap om dagens artsforekomster. For eksempel kan det informere om forekomsten av rødlistete arter, eller arter som ikke tidligere er registrert fra landet. Merk at funnene ikke er manuelt verifisert, uten baseres på DNA-treffer, der man er avhengig kvaliteten på referansebasene. 

Artene danner også \"sammfunn\" av insekter som fordeler seg i regelmessig i tid og rom. Forandring av slike artssamfunn er også vert å følge med på, selv om enkelte arter ikke nødvendigvis forsvinner.
"
    )
    
    
    output$redlist <- renderUI({
      tags$img(src = "figures/redlist_nonnative_concat.png", 
               height = "300px"
      )
    })
    


    
    output$betacom <- renderUI({
      tags$img(src = "figures/beta-div-patterns-overall-1.svg", 
               height = "300px"
      )
    })
       
  })
}