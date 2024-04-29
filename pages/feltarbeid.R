require(shinyjs)

jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"


felt_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Innsamling",
           column(6,
           box(width = 12,
             textOutput(ns("felletyper_text")),
             # tags$style(type="text/css", "#felletyper_text {white-space: pre-wrap;}"),
             title = "Overvåkingsdesign",
             solidHeader = TRUE,
             height = "800px"
           )),
           column(6,
          box(width = 12,
            uiOutput(ns("localities")),
            title = "Overvåkingslokaliteter i perioden 2020-2023",
            solidHeader = TRUE,
            height = "400px"
          ),
          br(),
           box(width = 12,
            uiOutput(ns("felletyper")),
            title = "Malaise- og vindusfeller",
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
      tags$img(src = "figures/felletyper_medium.jpg", 
               #src = "figures/felletyper.jpg", 
               height = "300px",
               width = "95%",
               #onclick="enlargeImg()",
               #id ="img1"
               )
      
    })

output$felletyper_text <- renderText("Overvåkingen skal representere ulike økosystemer innenfor geografiske regioner (Norges 5 landsdeler). Vi startet med skog og jordbruksmark (semi-naturlig mark) på Østlandet i 2020. Fra og med 2024 dekker programmet jordbruksmark i alle regioner, men skog er fortsatt kun overvåket på Østlandet. På sikt er ambisjonen å dekke hele landet i begge disse to økosystemene med 250 lokaliteter hver. Hvert år besøkes 10 lokaliteter innen hvert økosystem i hvert av de 5 regionene (se dashboard). Hver lokalitet gjenbesøkes dermed hvert 5 år. 

Innsamling av insekter skjer hovedsakelig med malaisefeller, et slags telt der insekter flyr oppover en duk og ender opp i en flaske fylt med etanol på det høyeste punktet i teltet. På lokalitetene i skog bruker vi også 4 vindusfeller. Disse fanger store biller som faller ned i en flaske med etanol etter at de har krasjet med et gjennomsiktig plastplate. Fellene står ute i den hovedsakelige flygeperioden, fra april til og med oktober, og tømmes annenhver uke av feltarbeidere som ansvarer for en eller flere lokaliteter."  
)


# output$stagger <- renderUI({
#   tags$img(src = "figures/forskj-1.svg", 
#            height = '300px')
# })


  # output$localities <- renderUI({
  #   tags$img(src = "figures/lokaler_20202023-1.svg",
  #            height = '300px')
  # })

  output$localities <- renderUI({
    tags$img(src = "figures/fig-lokaler20202023-1.png",
             height = '300px')
  })

  return(
    list(con = reactive(login_export$con))
  )


  })
}


