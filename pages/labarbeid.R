

labarbeid_ui <- function(id){
  ns <- NS(id)
  
tabPanel(title = "Labarbeid",
         
         fluidRow(
           box(
             uiOutput(ns("bottle")),
             title = "En malalisefelle samler tusenvis av insekter",
             solidHeader = TRUE,
             height = "400px"
           ),
           
           box(
             textOutput(ns("proc_text")),
             tags$style(type="text/css", "#proc_text {white-space: pre-wrap;}"),
             title = "Prosessering av prøver",
             solidHeader = TRUE,
             height = "400px"
           )
         ),
         br(),
         fluidRow(
           box(
             uiOutput(ns("syrphidae")),
             title = "Eksempel på blomfluer fra en felletømming",
             solidHeader = TRUE,
             height = "400px"
           ),
           
           box(
             uiOutput(ns("lab")),
             title = "Veing og identifisering med metastrekkoding",
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
    
    output$proc_text <- renderText("Særskilt malaisefeller fanger store mengder insekter, og en manuell sortering og identifisering av artene ville ta for lang tid i et så stort program som dette. Manuell identifisering stiller også store krav på taksonomisk kompetanse, men er mulig å gjennomføre for enkelte grupper for kvalitetssikring av metodikken, som for eksempel blomsterfluer. 
  
Felleprøvene prosesseres på lab i Trondheim, der vi tilsetter en \"lyserings-væske\" som trekker ut DNAet i væsken som deretter \"metastrekkodes\". Det vil si at man måler forekomst av DNA i væsken fra mange arter samtidig. Deretter tilsettes ny etanol til prøven for den lagres. Insektene er dermed i stor grad intakte for eventuell manuell kontroll etterpå.
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

