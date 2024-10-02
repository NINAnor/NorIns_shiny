bioinformatikk_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Databehandling",
    column(
      6,
      box(
        width = 12,
        htmlOutput(ns("flytskjema_text")),
        title = "Bioinformatikk og miljøvariabler",
        solidHeader = TRUE,
        height = "800px"
      )
    ),
    column(
      6,
      box(
        width = 12,
        uiOutput(ns("flytskjema")),
        title = "DNA-resultat kvalitetssikres og kobles til artsnavn",
        solidHeader = TRUE,
        height = "400px"
      ),
      br(),
      box(
        width = 12,
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
      tags$img(
        src = "figures/flyt_concat.png",
        height = "300px",
        width = "95%"
      )
    })

    output$flytskjema_text <- renderUI({
      str_1 <- paste("Resultatet fra sekvenseringen fra Oslo universitetssykehus er en lang rekke med DNA-sekvenser. Det vi nå må gjøre er å koble disse sekvensene tilbake til de enkelte prøvene de kom fra, med hjelp av de konstgjorde markørene, og dele in sekvensene i enkelte arter basert på hvor forskjellige de er. \"Artene\" plasseres så inn i et taksonomisk tre, helst ned til artsnavn og blir dermed \"identifisert\". Dette kalles bioinformatikk. For dette arbeid har vi utviklet en egen fremgangsmåte med mange sjekkpunkter underveis for å sikre kvaliteten på arbeidet. Resultatet er en artsliste for hver felleprøve, inkludert relative mengder med DNA for hver unik DNA-sekvens. I disse analysene får vi også registrert genetisk variasjon innad i hver art.

I tillegg til insektene, samler vi også inn værdata – vi har både temperatur-, fuktighets-, og lysloggere på hver lokalitet. Vegetasjonen rundt fellene blir også kartlagt med samme type metodikk som brukes for arealrepresentativ naturovervåking ")

      ano_link <- a("(ANO)",
        href = "https://www.nina.no/%C3%98kosystemer/Arealrepresentativ-naturoverv%C3%A5king-ANO",
        target = "_blank"
      )

      str_2 <- paste(". I skogen gjøres i tillegg en forenklet kartlegging av skogtilstand, slik det gjøres på ")

      landskog_link <- a("landsskogstakseringen",
        href = "https://www.nibio.no/tema/skog/skog-og-miljoinformasjon-fra-landsskogtakseringen/landsskog-takseringen",
        target = "_blank"
      )

      str_3 <- paste(". Til sist samler vi inn informasjon fra offentlige datakilder knyttet til området rundt fellene. Dette kan hjelpe oss med å forklare endringer i insekter over tid, og forskjeller mellom områder som vi finner nå. Alle dataene lagres i en egen database.")

      HTML(paste(str_1, ano_link, str_2, landskog_link, str_3, sep = ""))
    })


    output$data_storage <- renderUI({

    })

    output$data_links <- renderUI({
      dataset <- a("Datasettet på GBIF",
        href = "https://www.gbif.org/dataset/19fe96b0-0cf3-4a2e-90a5-7c1c19ac94ee",
        target = "_blank"
      )
      norimon <- a("R-pakke med kod brukt i analyser",
        href = "https://www.github.com/NINAnor/Norimon",
        target = "_blank"
      )
      report_code <- a("Analysekode for rapporter:",
        href = "https://github.com/NINAnor/national_insect_monitoring",
        target = "_blank"
      )

      tagList(
        dataset,
        norimon,
        report_code,
      )
    })
  })
}
