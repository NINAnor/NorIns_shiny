

labarbeid_ui <- function(id){
  ns <- NS(id)
  
tabPanel(title = "Labarbeid",
         column(6,
           box(width = 12,
           textOutput(ns("proc_text")),
           #tags$style(type="text/css", "#proc_text {white-space: pre-wrap;}"),
           title = "Prosessering av prøver",
           solidHeader = TRUE,
           height = "800px"
         )),
         column(6,
          box(width = 12,
           uiOutput(ns("bottle")),
           title = "En malaisefelle samler tusenvis av insekter",
           solidHeader = TRUE,
           height = "400px"
         ),
        br(),
        box(width = 12,
           uiOutput(ns("lab")),
           title = "Håndtering av felleprøver på lab",
           solidHeader = TRUE,
           height = "400px"
        )
         )
         
)
}


labarbeid_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    output$bottle <- renderUI({
      tags$img(src = "figures/IMG_8093.jpg", 
               height = '300px')
    })
    
    output$proc_text <- renderText("Malaisefeller fanger store mengder insekter, og det er ikke praktisk mulig å sortere og artsbestemme alle insektene innenfor den kostnadsrammen vi har. Vi bruker derfor en metode som kalles metastrekkoding, der vi kan måle forekomsten av DNA fra mange arter samtidig.
    
Prøveflaskene blir lagret kjølig etter tømming, og sendes i batcher til NINA i Trondheim for sentral prosessering. Det første vi gjør på lab er å veie insektene. Vi lar etanolen dryppe av, og når det er mer enn 30 sekunder mellom hver dråpe, veier vi innholdet. Dette tallet kalles for avrunnen våtvekt og regnes som insektenes biomasse.
    
Deretter tilsetter vi en \"lyserings-væske\" som trekker ut DNA fra insektene. Det er denne væsken som vi etterpå analyserer for DNA. Deretter tilsettes ny etanol til insektprøven som så kan lagres. Insektene er dermed i stor grad intakte for eventuell manuell kontroll etterpå. For å kvalitetsikre metodikken har vi også foretatt manuell identifisering av biller og blomsterfluer.

Genetik-labbet på NINA i Trondheim isolerer en del av det mitokondrielle DNAet fra alle prøveflasker, tilsetter så en konstgjort markør som viser hvilken flaske DNAet kommer fra, og blander så alt sammen i et prøverør. Dette prøverør sendes så til Oslo universitetssykehus der det sekvenseres, det vil si at det blir registrert hvilke DNA-sekvenser man har i prøvet(ene).
"
    )
    
    
    output$syrphidae <- renderUI({
      tags$img(src = "figures/syrphidae_sortering.jpg", 
               height = '300px'
      )
    })
    
    output$lab <- renderUI({
      tags$img(src = "figures/20221026_095621.jpg", 
               height = '300px',
               width = "95%"
      )
    })
    
    
  })
}

