

biodiv_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Artsmangfold",
           fluidRow(
             box(
               uiOutput(ns("rankabund")),
               title = "Tusenvis arter fanges ofte, men de fleste er sjeldne",
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

output$rankabund_text <- renderText("Insektsamfunn er ekstremt artsrike. Av de ca. 30 000 kjente artene i Norge fra dyreriket, er nesten 20 000 insekter, og sannsynligvis finnes det flere tusen insektarter i Norge som venter på å oppdages. De fleste artssamfunn domineres av et fåtall arter mens de andre er relativt skjeldne. Dette er spesielt tydelig i innsektovervåkingen, der halen med sjeldne arter er svært lang og mange tusen arter er blitt funnet bare et fåtall ganger. Alt ettersom innsamlingen fortsetter kan man forvente seg at halen til høyre vil fortsette utvides med nye sjeldne arter, og at akkumuleringskurvene fortsetter øke en tid fremover.

Tilfeldige forekomster av mange uvanlige arter innebærer at man ikke kan forvente seg å observere hele insektsamfunn på en enkelt lokalitet. Likevel er det flere tusen av arter som observeres relativt ofte, og sett sammen er registreringen av insektsamfunnen konsistente nok til å undersøke forskjeller mellom naturtyper og overvåke forandringer.

Fortsatt finner vi mye DNA som kan grupperes til distinkte arter, men som vi ennå ikke kan koble til et artsnavn fordi ingen har registrert deres artstilhørighet i offentlige kilder. Dette gjelder fremforalt de uvanlige artene, der spesielt tovinger og veps har mange \"arter\" foreløpig uten navn. For å forbedre denne situasjon fyller prosjektet i samarbeide med NTNU Vitenskapsmuseet årlig på offentlige datakilder med flere hundre tidligere uregistrerte arter.
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