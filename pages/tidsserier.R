
tidsserier_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Tidsserier",
           box(
             textOutput(ns("tidsserie_text")),
             title = "Tidsserie",
             solidHeader = TRUE,
             height = "800px"
           ),  
           box(
             uiOutput(ns("tidsserie")),
             title = "Biomasse og artsantall",
             solidHeader = TRUE,
             height = "400px"
           ),
           br(),
           box(
             uiOutput(ns("redlist")),
             title = "Rødlistede og ikke tidligere registrerte funn",
             background = "lime",
             solidHeader = TRUE,
             height = "400px"
           )

           )
  
}



tidsserier_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    output$tidsserie <- renderUI({
      tags$img(src = "figures/biomass_div_concat.png", 
               height = "300px")
    })
    
    
    output$tidsserie_text <- renderText("Målet med dette prosjekt er å få en tidsserie som kan brukes for å undersøke endringer i insektfaunaen over tid. Antall arter og mengde insekter varierer ofte kraftig i løpet av en sesong, mellom år og mellom lokaliteter. Med denne store variasjonen trenger vi en lang tidsserie, eller særlig store endringer, for å kunne si noe sikkert. 
    
Overvåkingen gir også verdifull kunnskap om artsforekomster. Vi kan få ny kunnskap om utbredelsen til rødlistete arter, eller arter som ikke tidligere er registrert fra landet. Det er imidlertid viktig og være klar over at  funnene ikke er manuelt verifisert, og at det kan være feil i DNA-bibliotekene. Disse feilene vil imidlertid bli færre over tid.

Artene danner også egne insektsamfunn, det vil si insekter som forekommer sammen i tid og rom. Forandring av slike artssamfunn er også viktig å kartlegge, selv om enkeltarter ikke nødvendigvis forsvinner.

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