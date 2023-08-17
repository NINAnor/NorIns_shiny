require(shinyjs)


jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"


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
               title = "Overvåkingslokaliteter i perioden 2020-2023",
               solidHeader = TRUE,
               height = "400px"
             )
           )
  )
}


felt_server <- function(id, login_import) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
   
    
    login_export <- reactiveValues()
    
    load("data/shinyPass.Rdata")
    
    connect_to_insect_db(user = my_username,
                         password = my_password)
    
    login_export$con <- con
    
    
    output$felletyper <- renderUI({
      tags$img(src = "figures/felletyper.jpg", 
               height = "300px",
               width = "95%",
               #onclick="enlargeImg()",
               #id ="img1"
               )
      
    })

output$felletyper_text <- renderText("Overvåkingen ble startet på Østlandet i 2020 og utvides suksesivt over landet. Fra og med 2023 dekker programmet semi-naturlig mark i alle landsdeler bortsett fra Vestlandet, samt skogsmark på Østlandet. På sikt er ambisjonen å dekke hele landet i begge disse økosystemtyper.
  
Vi bruker et rullerende skjema der man gjenbesøker fellene etter 5 år. Hvert år besøkes 10 lokaliteter innen hvert økosystem i hvert av de 5 landsdelene. Dermed vil programmet på sikt inkludere 250 lokaliteter per økosystem spredt over hele landet.

Overvåkingen skjer hovedsakelig med malaisefeller, et slags telt der insekter flyger oppover duken og ender opp i en flaske fylt med etanol. På lokalitetene i skog bruker vi også 4 st vindusfeller, der fremst biller som flyger inn i en plastskive dropper ned i en flaske med etanol. Fellene står ute i den hovedsakelige flyveperioden, fra april til og med oktober, og tømmes annenhver uke."  
)


output$stagger <- renderUI({
  tags$img(src = "figures/forskj-1.svg", 
           height = '300px')
})


output$localities <- renderUI({
  tags$img(src = "figures/lokaler_20202023-1.svg",
           height = '300px')
})

  return(
    list(con = reactive(login_export$con))
  )


  })
}


