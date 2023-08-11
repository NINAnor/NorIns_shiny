

bioinformatikk_ui <- function(id){
  ns <- NS(id)
  
tabPanel(title = "Bioinformatikk",
         fluidRow(
           box(
             uiOutput(ns("flytskjema")),
             title = "Resultater fra metastrekkoding kvalitetssikres",
             solidHeader = TRUE,
             height = "400px"
           ),
           
           box(
             textOutput(ns("flytskjema_text")),
             title = "Dataprosessering og håndtering",
             solidHeader = TRUE,
             height = "400px"
           )
         ),
         br(),
         fluidRow(
           box(
             uiOutput(ns("data_storage")),
             title = "Prosjektet danner store datamengder",
             solidHeader = TRUE,
             height = "400px"
           ),
           
           box(
             uiOutput(ns("data_links")),
             title = "Linker til data",
             solidHeader = TRUE,
             height = "400px"
           )
         )
)
}



bioinformatikk_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    
    
    output$flytskjema <- renderUI({
      tags$img(src = "figures/flyt_concat.png", 
               height = "300px",
               width = "95%")
      
    })
    
    output$flytskjema_text <- renderText("Metastrekkoding gir tilbake en lang rekke av DNA-sekvenser, men gir ikke i seg selv informasjon om hvilke disse tilhører. Det er jobben for bioinformatikern, det vil si at sortere ut hvilke DNA-sekvenser som tilhører insekter, hvilke DNA-sekvenser som bør grupperes til samme art, og til sist å plassere inn disse i et taksnonomisk tre, forhåpentlig enda frem til et artsnavn. Dette er en komplisert prosesse, der vi har utviklet en egen pipeline med flere kvalitetssjekker. Resultatet er en artsliste for hver felleprøve, med tall på den relative mengde DNA for hver unik DNA-sekvense per prøve. Vi får også registrert innenarts-variasjonen i DNA for artene, uttrykkt i forskjellige DNA-sekvenser som grupperes til samme art.

Sluttresultatet fra bioinformatikken kobles siden til enkelte felleprøver med tilhørende data. Vi samler for eksmpel inn finskala værdata gjennom temperatur, fuktighet, og lysloggere ved hver felle. I tillegg gjennomfør vi en kartlegging av floraen rundt fellene, der vi følger metodikken for arealrepresentativ naturovervåking (ANO), samt en forenklet landsskogstaksering i skogslokalitetene. Til sist samler vi inn informasjon fra diverse offentlige datakilder knyttet til området runt fellene, for å hjelpe å forklare insektfunnen. Dette prosjekt genererer dermed store mengder data og binder sammen data fra en mange kilder. Alt dette lagres i en dedikert database, hvorifra informasjon kan sendes til forskjellige formål, som analyser, eller informasjonssider som denne.

Data formateres også til GBIF-format og eksporteres gjennom NINA sin GBIF-IPT."
    )
    
    
    output$data_storage <- renderUI({
      
    })
    
    output$data_links <- renderText("
    
    Datasettet på GBIF:
      https://www.gbif.org/dataset/19fe96b0-0cf3-4a2e-90a5-7c1c19ac94ee
    R-pakke med kod brukt i analyser:
      https://www.github.no/NINAnor/Norimon
    Analysekod for reporter:
      https://github.com/NINAnor/national_insect_monitoring
      ")
    
  })
}

