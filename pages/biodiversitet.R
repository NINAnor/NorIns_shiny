

biodiv_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Artsmangfold",
           fluidRow(
             box(
               uiOutput(ns("rankabund")),
               title = "Mange arter fanges ofte, men de fleste artene er uvanlige",
               solidHeader = TRUE,
               height = "400px"
             ),
             
             box(
               textOutput(ns("rankabund_text")),
               title = "Overvåking av en hyperdivers organismegruppe",
               solidHeader = TRUE,
               height = "400px"
             )
           ),
           br(),
           fluidRow(
             box(
               uiOutput(ns("specacc")),
               title = "Mer og mer av insektmangfoldet kartlegges",
               solidHeader = TRUE,
               height = "400px"
             ),
             
             box(
               uiOutput(ns("artsniva")),
               title = "Mange arter kan ennå ikke tildeles et navn",
               solidHeader = TRUE,
               height = "400px"
             )
           )
  )
  
}


biodiv_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    

output$rankabund <- renderUI({
  tags$img(src = "figures/spec-occ-1.svg", 
           height = "300px",
           width = "95%"
           )
})

output$rankabund_text <- renderText("Insektsamfunn er ekstremt artsrike. Av de ca. 30 000 kjente artene i Norge fra dyreriket, er nesten 20 000 insekter, men  sannsynligvis finnes det ytterligere flere tusen insektarter i Norge som venter på å oppdages. Nesten alle artsrike samfunn domineres av et relativ fåtall antall arter og har en lang hale med skjeldne arter. Dette er spesielt tydelig med insektfunnen fra dette prosjekt, der flere tusen arter blir funnet svært sjeldent. Alt ettersom innsamlingen fortsetter kan man forvente seg at halen til høyre vil fortsette utvides, med nye sjeldne arter.

Slike skjeve abundanser innebærer at man ikke kan forvente seg å observere et helt insektsamfunn på enkelte lokaliteter, og at det vil ta lang tid før artskurvene flater ut. Likevel er det flere tusen av arter som observeres relativt ofte, og sett sammen er registreringen av insektsamfunnen konsistente nok til å undersøke forskjeller mellom naturtyper, og overvåke forandringer.
"
)




output$specacc <- renderUI({
  tags$img(src = "figures/div-est-1.svg", 
           height = "300px"
  )
})

output$artsniva <- renderUI({
  tags$img(src = "figures/artsniva-1.png", 
           height = "300px"
  )
}) 

})
}