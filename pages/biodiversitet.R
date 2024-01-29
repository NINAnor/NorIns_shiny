

biodiv_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Artsmangfold",
           column(6,
           box(width = 12,
             textOutput(ns("rankabund_text")),
             title = "Overvåking av en hyperdivers organismegruppe",
             solidHeader = TRUE,
             height = "800px"
           )),
           column(6,
             box(width = 12,
               uiOutput(ns("rankabund")),
               title = "De fleste arter er sjeldne",
               solidHeader = TRUE,
               height = "400px"
             ),
           br(),
            box(width = 12,
               uiOutput(ns("specacc")),
               title = "Kunnskapen om insektmangfoldet øker",
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
  tags$img(src = "figures/spec-occ-1.png", 
           height = "300px",
           width = "95%"
           )
})

output$rankabund_text <- renderText("Insektsamfunn er ekstremt artsrike. Av de ca. 30 000 kjente dyreartene i Norge, er nesten 20 000 insekter, og sannsynligvis finnes det flere tusen insektarter i Norge som vi ikke har funnet ennå. De fleste arter er faktisk veldig sjeldne – og bare noe få er veldig vanlige. Dette er spesielt tydelig i tallene fra innsektovervåkingen, der halen med sjeldne arter er svært lang(se figur oppe til høyre). Over tid kan vi forvente seg at antall sjeldne arter øker (halen til høyre vil fortsette utvides), og at antall nye arter funnet (fig nede til høyre) fortsetter å øke en god tid fremover. 

Selv om det er mange sjeldne arter, er det flere tusen arter som blir funnet relativt ofte. Dette gjør at vi er i stand til  å undersøke forskjeller mellom naturtyper og overvåke endringer over tid. Hvilke arter vi har funnet kan også endres tilbake i tid. Det er mye DNA som vi ennå ikke kan koble til et artsnavn fordi ingen har registrert nettopp deres DNA i offentlige kilder. Dette gjelder fremfor alt de sjeldne artene, og spesielt tovinger og veps. For å forbedre denne situasjon samarbeider vi med NTNU Vitenskapsmuseet og har sørget for atflere hundre tidligere uregistrerte arter nå er tilgjengelig i DNA-bibliotekene."
)


## "Antall arter" på x-akseln. "Antall observasjoner av hver art (1114 mulige)
## større tall på aksene
## Mulig å dele opp i fig per år?

output$specacc <- renderUI({
  tags$img(src = "figures/div-est-1.svg", 
           height = "300px"
  )
})

## "faktiske tall", "ekstrapolerte tall" isteden for engelsk. Men vanskelig i iNEXT plot?
## Semi-naturlig mark i Østfold etc i legend. 
## Mulig å dele opp i fig per år?

output$artsniva <- renderUI({
  tags$img(src = "figures/artsniva-1.png", 
           height = "300px"
  )
}) 

})
}