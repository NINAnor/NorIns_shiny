library(shiny)
library(shinydashboard)
library(knitr)
require(Norimon)
#require(knitr)
require(leaflet)
require(leaflet.minicharts)
require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)


ui <- navbarPage(title = "Norsk insektovervåking - en innblikk",
                 footer = NULL,
                 header = NULL,

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabPanel(title = "Feltarbeid",
               fluidRow(
                 box(
                   uiOutput('felletyper'),
                   title = "Innsamling med malaise- og vindusfeller",
                   solidHeader = TRUE
                 ),

                 box(
                   textOutput("felletyper_text"),
                   tags$style(type="text/css", "#felletyper_text {white-space: pre-wrap;}"),
                   title = "Overvåkingsdesign",
                   solidHeader = TRUE

                 )
               ),
               br(),
               fluidRow(
                 box(
                   uiOutput('stagger'),
                   title = "Lokaliteter gjenbesøkes hvert 5:e år",
                   solidHeader = TRUE
                   
                 ),
                 
                 box(
                   uiOutput('localities'),
                   title = "Overvåkingslokaliteter i perioden 2020-2022",
                   solidHeader = TRUE
                 )
               )
               ),
      tabPanel(title = "Labarbeid",
             
               fluidRow(
                 box(
                   uiOutput('bottle'),
                   title = "En malalisefelle samler tusenvis av insekter",
                   solidHeader = TRUE
                 ),
                 
                 box(
                   textOutput("proc_text"),
                   tags$style(type="text/css", "#proc_text {white-space: pre-wrap;}"),
                   title = "Overvåkingsdesign",
                   solidHeader = TRUE
                 )
               ),
               br(),
               fluidRow(
                 box(
                   uiOutput('syrphidae'),
                   title = "Eksempel på blomfluer fra en felletømming",
                   solidHeader = TRUE
                 ),
                 
                 box(
                   uiOutput('lab'),
                   title = "Prøvene veies og DNA-et identifiseres med metastrekkoding",
                   solidHeader = TRUE
                 )
               )
               ),
    tabPanel(title = "Bioinformatikk"),
    tabPanel(title = "Artsmangfold",
             fluidRow(
               box(
                 uiOutput('rankabund'),
                 title = "Flere tusen arter observeres ofte, men de aller fleste arter er uvanlige",
                 solidHeader = TRUE
               ),
               
               box(
                 textOutput("rankabund_text"),
                 tags$style(type="text/css", "#rankabund_text {white-space: pre-wrap;}"),
                 title = "Overvåking av en hyperdivers organismegruppe",
                 solidHeader = TRUE
               )
             ),
             br(),
             fluidRow(
               box(
                 uiOutput('specacc'),
                 title = "Mer og mer av insektmangfoldet kartlegges",
                 solidHeader = TRUE
               ),
               
               box(
                 uiOutput('artsniva'),
                 title = "Mange observerte arter har ennå ikke blitt tildelt et navn",
                 solidHeader = TRUE
               )
             )
             ),
    tabPanel(title = "Insekter i tid og rom",
             
             fluidRow(
               box(
                 uiOutput('tidsserie'),
                 title = "Biomasse og artsantall",
                 solidHeader = TRUE
               ),
               
               box(
                 textOutput("tidsserie_text"),
                 tags$style(type="text/css", "#tidsserie_text {white-space: pre-wrap;}"),
                 title = "Starten på en tidsserie",
                 solidHeader = TRUE
               )
             ),
             br(),
             fluidRow(
               box(
                 uiOutput('redlist'),
                 title = "Rødlistede og ikke tidligere registrerte funn",
                 solidHeader = TRUE
               ),
               
               box(
                 uiOutput('betacom'),
                 title = "Artsamfunn varierer med økosystem, og over tid",
                 solidHeader = TRUE
               )
             )),
    tabPanel(title = "Innenartsvariasjon",
             fluidRow(
               box(
                 leaflet::leafletOutput("asv_map",
                                        width = "70%",
                                        height = 600)
                 
               ),
               box(
                 uiOutput("choose_asv_species")
               )
             )
             )

)

server <- shinyServer(function(input, output, session) {
  

  load("data/shinyPass.Rdata")
  
  connect_to_insect_db(user = my_username,
                       password = my_password)


  addResourcePath(prefix = "figures", directoryPath = "figures")
  
  ##Start - Feltaribeid-page
  
  output$felletyper <- renderUI({
    tags$img(src = "figures/felletyper.jpg", width = '90%')
  })
  
  output$felletyper_text <- renderText("Overvåkingen ble startet i 2020 i skog og semi-naturlig mark på Østlandet. I 2021, og 2022 ble semi-naturlig mark lagt til i Trøndelag respektive Sørlandet, og i 2023 skal programmet utvides også til semi-naturlig mark i Nord-Norge. På sikt er ambisjonen å dekke hele landet i begge disse økosystemtyper.
  
Vi besøker hver lokalitet i et rullerende skjema der man gjenbesøker fellene etter 5 år. Hvert år besøkes 10 lokaliteter innen hvert økosystem og hver av de 5 landsdelene. Dermed vil programmet på sikt inkludere 250 lokaliteter per økosystem spredt over hele landet.

Overvåkingen skjer hovedsakelig med malaisefeller, et slags telt der insektene flyver inn og ender opp i en flaske fylt med etanol. På lokalitetene i skog bruker vi også 4 st vindusfeller, der fremst biller flyger inn i en plastskive og dropper ned i en flaske med etanol. Fellene står ute i den hovedsakelige flyveperioden, fra april til og med oktober, og tømmes annenhver uke."  
    )

  
  output$stagger <- renderUI({
    tags$img(src = "figures/forskj-1.svg", width = '90%')
  })
  
  
  output$localities <- renderUI({
    tags$img(src = "figures/lokaler20202022-1.svg", width = '90%')
  })


  ##End - Feltaribeid-page
  
  ##Start - Labarbeid-page
  
  output$bottle <- renderUI({
    tags$img(src = "figures/IMG_8093.jpg", width = '90%')
  })
  
  output$proc_text <- renderText("Særskilt malaisefeller fanger store mengder insekter, og en manuell sortering og identifisering av artene ville ta for lang tid i et så stort program som dette. Manuell identifisering stiller også store krav på taksonomisk kompetanse, men er mulig å gjennomføre for enkelte grupper for kvalitetssikring av metodikken, som for eksempel blomsterfluer. 
  
Felleprøvene prosesseres på lab i Trondheim, der vi tilsetter en \"lyserings-væske\" som trekker ut DNAet i væsken som deretter \"metastrekkodes\". Det vil si at man måler forekomst av DNA i væsken fra mange arter samtidig. Deretter tilsettes ny etanol til prøven for den lagres. Insektene er dermed i stor grad intakte for eventuell manuell kontroll etterpå.
"
)
  
  
  output$syrphidae <- renderUI({
    tags$img(src = "figures/syrphidae_sortering.jpg", 
         width = '90%'
         )
  })
  
  output$lab <- renderUI({
    tags$img(src = "figures/20221026_095621.jpg", 
         width = '90%'
    )
  })
  
  ##End - Labarbeid-page
  
  ##Start - Bioinformatikk-page
  ##End - Bioinformatikk-page
  
  ##Start - Artsmangfold-page
  output$rankabund <- renderUI({
    tags$img(src = "figures/spec-occ-1.svg", width = '90%')
  })
  
  output$rankabund_text <- renderText("Insektsamfunn er ekstremt artsrike. Av de ca. 30 000 kjente artene i Norge fra dyreriket, er nesten 20 000 insekter, men  sannsynligvis finnes det ytterligere flere tusen insektarter i Norge som venter på å oppdages. Nesten alle artsrike samfunn domineres av et relativ fåtall antall arter og har en lang hale med skjeldne arter. Dette er spesielt tydelig med insektfunnen fra dette prosjekt, der flere tusen arter blir funnet svært sjeldent. Alt ettersom innsamlingen fortsetter kan man forvente seg at halen til høyre vil fortsette utvides, med nye sjeldne arter.

Slike skjeve abundanser innebærer at man ikke kan forvente seg å observere et helt insektsamfunn på enkelte lokaliteter, og at det vil ta lang tid før artskurvene flater ut. Likevel er det flere tusen av arter som observeres relativt ofte, og sett sammen er registreringen av insektsamfunnen konsistente nok til å undersøke forskjeller mellom naturtyper, og overvåke forandringer.
"
  )
  
  
  output$specacc <- renderUI({
    tags$img(src = "figures/div-est-1.svg", 
             width = '90%'
    )
  })
  
  output$artsniva <- renderUI({
    tags$img(src = "figures/artsniva-1.png", 
             width = '90%'
    )
  })
  ##End - Artsmangfold-page
  
  ##Start - Insekter-tidrom-page
  output$tidsserie <- renderUI({
    tags$img(src = "figures/biomass_div_concat.png", width = '90%')
  })
  
  output$tidsserie_text <- renderText("Overvåkingen innebærer starten på en sammenlignbar tidsserie over insektmengder og artskomposisjon for store insektsamfunn i landet. Insekter varierer ofte kraftig, både innen år, mellom år, og mellom lokaliteter, og man trenger derfor lange tidsserier før man kan observere statistisk sikre forandringer. Det er ikke mulig å forutse nøyaktig hvor lang tid, da det er avhengig hvor store forandringene blir, og hvor mye tilfeldig variasjon man har, men tidshorisonten er snarere 10-talls år enn enkeltår.

Overvåkingen kartlegger ikke bara forandringer, uten gir også verdifull kunnskap om dagens artsforekomster. For eksempel kan det brukes som en standardisert datakilde for forekomst av rødlistete arter, og arter som ikke tidligere er registrert fra landet. Det er da viktig å huske at funnen ikke er manuelt verifisert, uten baserer seg utelukkende på DNA-treffer, hvilke gjenspeiler kvaliteten på referansebasene. 

Referansebasene blir stendig bedre, hvilket leder til at altfler arter vill kunne kobles til ett artsnavn. I tillegg vil noen artsnavn endres, altettersom feil i basene (f.eks. i BOLD), oppdages og blir utbedret. Typisk sett er de fleste av de vanlige artene (som utgjører merparten av den innsamlete DNAen) strekkodete, og vi kan sette et artsnavn på disse. Likevel gjenstår det et stort antall arter som ennå ikke er strekkodet, og derfor ikke kan bestemmes til art. Særskilt for store og vanskelige artsgrupper har vi begrenset evne å sette artsnavn til alle innsamlete (antatte) arter, da mange av dem fortsatt mangler i referansebasene. Programmet strekkoder derfor hvert år arter som mangler i samarbeide med NTNU Vitenskapsmuseet, hittil mellom 300 og 500 nye arter per år.
"
  )
  
  
  output$redlist <- renderUI({
    tags$img(src = "figures/redlist_nonnative_concat.png", 
             width = '90%'
    )
  })
  
  output$betacom <- renderUI({
    tags$img(src = "figures/beta-div-patterns-overall-1.svg", 
             width = '90%'
    )
  })
  
  ##END - Insekter-tidrom-page
  
  ##Start - Innenartsvariasjon-page
  
 
  asv_perc_reads <- tbl(con,
                        Id(schema = "views",
                           table = "asv_perc_reads"))

  

 basemap <- leaflet(width = "100%",
                     height = "400px") %>% 
    addTiles(group = "OpenStreetMap")
  
  
  asv_to_leaflet <- function(species = "NULL"){
    
    sel_asv <- asv_perc_reads %>%
      filter(species_latin_gbif == species)  %>%
      collect() %>% 
      #  filter(!!input$species_select_asv %in% (species_latin_gbif)) %>%
      mutate(asv = as_factor(sequence_id),
             perc_reads = round(perc_reads*100, 2))
    
    
    to_plot  <- sel_asv %>% 
      # mutate(lat = st_coordinates(geometry)[,2],
      #        lon = st_coordinates(geometry)[,1]) %>%
      # st_drop_geometry() %>% 
      select(locality, 
             lat,
             lon, 
             sequence_id,
             perc_reads,
             sum_reads) %>% 
      pivot_wider(names_from = "sequence_id",
                  values_from = "perc_reads",
                  names_prefix = "seq_",
                  values_fill = 0)
    
    return(to_plot)
  }
  
  

  

  output$asv_map <- renderLeaflet({
    
    to_plot <- asv_to_leaflet(input$asv_species)
    
    basemap  %>% 
      addMinicharts(to_plot$lon,
                    to_plot$lat,
                    type = "pie",
                    chartdata = to_plot[, which(grepl("seq_", names(to_plot)))],
                    width = log(to_plot$sum_reads) * 3 ,
                    legend = FALSE,
      )
    
  })
  
  
  
  
  


species_list <- tbl(con,
           Id(schema = "views",
              table = "species_list")) 

output$choose_fam <- renderUI({
  
  family_choices <- species_list %>% 
    filter(id_order == input$sel_order)
  select(id_order) %>%
    distinct() %>%
    arrange(id_order) %>% 
    pull()
  
  
  selectInput("sel_fam",
              label = "Velg familie",
              choices = family_choices,
              selected = "")
  
  
})




output$choose_conf <- renderUI({
  
  conf_choices <- c("HIGH", "MODERATE", "LOW", "POOR")
  
  
  selectInput("sel_conf",
            label = "Velg konfidansenivå",
            choices = conf_choices,
            selected = "")
})


output$choose_order <- renderUI({
  
  order_choices <- species_list %>% 
    select(id_order) %>%
    distinct() %>%
    arrange(id_order) %>% 
    pull()
  
  selectInput("sel_order",
            label = "Velg orden",
            choices = order_choices,
            selected = "")
})

output$choose_asv_species <- renderUI({
  
  
  species_choices <- species_list %>% 
    select(species_latin_gbif) %>%
    distinct() %>%
    arrange(species_latin_gbif) %>% 
    pull()
  
  selectInput("asv_species",
            label = "Velg art",
            choices = species_choices,
            selected = "")
})


spec_conf <- function() reactive({
  species_list %>%
    filter(species_latin_gbif == input$asv_species) %>%
    select(identification_confidence) %>%
    distinct() %>%
    pull()

})




  ##End - Innenartsvariasjon-page
  
  
  
})

shinyApp(ui, server)
