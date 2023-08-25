

bioinformatikk_ui <- function(id){
  ns <- NS(id)
  
tabPanel(title = "Bioinformatikk",
         
         box(
           textOutput(ns("flytskjema_text")),
           title = "Dataprosessering og håndtering",
           solidHeader = TRUE,
           height = "800px"
         ),
         box(
           uiOutput(ns("flytskjema")),
           title = "DNA-resultat kvalitetssikres og kobles til navn",
           solidHeader = TRUE,
           height = "400px"
         ),
         br(),
         box(
           uiOutput(ns("data_links")),
           title = "Linker til data",
           solidHeader = TRUE,
           height = "400px"
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
    
    output$flytskjema_text <- renderText("Metastrekkoding gir tilbake en lang rekke av DNA-sekvenser, men gir ikke i seg selv informasjon om hvilke arter disse tilhører. Neste skritt er bioinformatikk, der man identifiserer hvilke DNA-sekvenser som tilhører samme art, og hvor disse sekvenser kan plasseres inn i et taksnonomisk tre, forhåpentlig enda frem til et artsnavn. For dette arbeid har vi utviklet en egen pipeline med flere kvalitetssjekker. Resultatet er en artsliste for hver felleprøve, med tall på den relative mengde DNA for hver unik DNA-sekvense per prøve. Vi får også registrert innenarts-variasjonen i DNA for artene, uttrykkt i forskjellige DNA-sekvenser som grupperes til samme art.

Vi samler også inn inn værdata fra temperatur-, fuktighets-, og lysloggere på hver lokalitet. I tillegg gjennomfør vi en kartlegging av floraen rundt fellene, der vi følger metodikken for arealrepresentativ naturovervåking (ANO), samt en forenklet landsskogstaksering i skogslokalitetene. Til sist samler vi inn informasjon fra diverse offentlige datakilder knyttet til området runt fellene, for å hjelpe å forklare insektfunnen. Dette prosjekt genererer dermed store mengder data som kobles sammen med data fra en mange kilder. Alt dette lagres i en dedikert database, hvorifra informasjon kan sendes til forskjellige formål, som analyser, eller informasjonssider som denne."
 )
    
    
    output$data_storage <- renderUI({
      
    })
    
    output$data_links <- renderUI({
    
    dataset <- a("Datasettet på GBIF", href="https://www.gbif.org/dataset/19fe96b0-0cf3-4a2e-90a5-7c1c19ac94ee")
    norimon <- a("R-pakke med kod brukt i analyser", href="https://www.github.com/NINAnor/Norimon")
    report_code <- a("Analysekod for rapporter:", href="https://github.com/NINAnor/national_insect_monitoring")

    tagList(
      
      dataset,
      norimon,
      report_code,
    )
    
      })
    
  })
}

