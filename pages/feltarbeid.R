

felt_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Feltarbeid",
           fluidRow(
             box(
               uiOutput(ns("felletyper")),
               title = "Innsamling med malaise- og vindusfeller",
               solidHeader = TRUE,
               height = "400px"
             ),
             
             box(
               textOutput(ns("felletyper_text")),
               # tags$style(type="text/css", "#felletyper_text {white-space: pre-wrap;}"),
               title = "Overvåkingsdesign",
               solidHeader = TRUE,
               height = "400px"
               
             )
           ),
           br(),
           fluidRow(
             box(
               uiOutput(ns("stagger")),
               title = "Lokaliteter gjenbesøkes hvert 5:e år",
               solidHeader = TRUE,
               height = "400px"
             ),
             
             box(
               uiOutput(ns("localities")),
               title = "Overvåkingslokaliteter i perioden 2020-2022",
               solidHeader = TRUE,
               height = "400px"
             )
           )
  )
}


felt_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    


output$felletyper <- renderUI({
  tags$img(src = "figures/felletyper.jpg", 
           height = "300px",
           width = "95%")
  
})

output$felletyper_text <- renderText("Overvåkingen ble startet i 2020 i skog og semi-naturlig mark på Østlandet. I 2021, og 2022 ble semi-naturlig mark lagt til i Trøndelag respektive Sørlandet, og i 2023 skal programmet utvides også til semi-naturlig mark i Nord-Norge. På sikt er ambisjonen å dekke hele landet i begge disse økosystemtyper.
  
Vi besøker hver lokalitet i et rullerende skjema der man gjenbesøker fellene etter 5 år. Hvert år besøkes 10 lokaliteter innen hvert økosystem og hver av de 5 landsdelene. Dermed vil programmet på sikt inkludere 250 lokaliteter per økosystem spredt over hele landet.

Overvåkingen skjer hovedsakelig med malaisefeller, et slags telt der insektene flyver inn og ender opp i en flaske fylt med etanol. På lokalitetene i skog bruker vi også 4 st vindusfeller, der fremst biller flyger inn i en plastskive og dropper ned i en flaske med etanol. Fellene står ute i den hovedsakelige flyveperioden, fra april til og med oktober, og tømmes annenhver uke."  
)


output$stagger <- renderUI({
  tags$img(src = "figures/forskj-1.svg", 
           height = '300px')
})


output$localities <- renderUI({
  tags$img(src = "figures/lokaler20202022-1.svg",
           height = '300px')
})

  })
}


