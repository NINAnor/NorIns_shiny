
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
    
    output$tidsserie_text <- renderText("Overvåkingen innebærer starten på en sammenlignbar tidsserie over insektmengder og artskomposisjon for store insektsamfunn i landet. Insekter varierer ofte kraftig, både innen år, mellom år, og mellom lokaliteter, og man trenger derfor lange tidsserier før man kan observere statistisk sikre forandringer. Det er ikke mulig å forutse nøyaktig hvor lang tid, da det er avhengig hvor store forandringene blir, og hvor mye tilfeldig variasjon man har, men tidshorisonten er snarere 10-talls år enn enkeltår.

Overvåkingen kartlegger ikke bara forandringer, uten gir også verdifull kunnskap om dagens artsforekomster. For eksempel kan det brukes som en standardisert datakilde for forekomst av rødlistete arter, og arter som ikke tidligere er registrert fra landet. Det er da viktig å huske at funnen ikke er manuelt verifisert, uten baserer seg utelukkende på DNA-treffer, hvilke gjenspeiler kvaliteten på referansebasene. 

De fleste av de vanlige artene er allerede strekkodete, og vi kan dermed tildele et artsnavn for disse funn. Men et stort antall arter er ennå ikke strekkodet, og kan derfor ikke bestemmes kobles til et navn. Særskilt for tovinger of vepser har vi begrenset evne til å sette artsnavn, da mange av disse arter fortsatt mangler i referansebasene. Programmet strekkoder derfor hvert år arter som mangler i samarbeide med NTNU Vitenskapsmuseet, hittil mellom 300 og 500 nye arter per år.
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