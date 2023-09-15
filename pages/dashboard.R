require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(shinydashboard)


dashboard_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Dashboard",
           fluidRow(column(1),
                    column(3, {
             valueBoxOutput(ns("no_loc_semi"),
                            width = 12)
                      }),
             column(3, {
             valueBoxOutput(ns("no_loc_forest"),
                            width = 12)
               }),
             column(3, {
             valueBoxOutput(ns("no_sampl"),
                            width = 12)
             })
           ),
           
           column(6,
                  box(width = 12,
                      title = "Prøvetaking",
                      height = "400px",

                      plotOutput(ns("project_sum_map"),
                                 height = "300px")                  )
           
                  ,
             br(),
             box(width = 12,
                 title = "Taksonomisk fordeling",
                 # uiOutput(ns("tax_perc"),
                 #          height = "300px"),
                 height = "400px"
             )),
           column(6,
              box(width = 12,
                  title = "Funn i et nøtteskall",
                  # uiOutput(ns("agg_res"),
                  #          height = "300px"),
                  height = "400px"
                  )
           )
  )
  
}



dashboard_server <- function(id, login_import) {
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session) {
    
    
    
    
    
    
    output$choose_habitattype <- renderUI({
      
      req(input$project)
      con <- login_import$con()
      
      habtypes_q <- "
      SELECT distinct l.habitat_type
      FROM locations.localities l,
      events.year_locality yl,
      lookup.projects
      WHERE yl.locality_id = l.id
      AND yl.project_short_name = projects.project_short_name
      AND projects.project_name = ?id1
      "
      
      habtypes_sql <- sqlInterpolate(con,
                                     habtypes_q,
                                     id1 = input$project)
      
      habtypes <- dbGetQuery(con,
                             habtypes_sql) 
      
      
      selectInput(inputId = ns("habitat_type"),
                  label = "Habitat type",
                  choices = c("All", habtypes$habitat_type),
                  selected = "All",
                  selectize = FALSE,
                  size = 1
    )
      
    })
    
      
    no_loc <- reactive({
      con <- login_import$con()
      
      no_loc_q <- "
      SELECT habitat_type, count(distinct(yl.locality_id))::numeric no_loc
      FROM events.year_locality yl,
      locations.localities l
      WHERE yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      GROUP BY l.habitat_type
     
      "
      
      no_loc <- dbGetQuery(con,
                           no_loc_q)
      
      return(no_loc)
      
    })
    
    
    output$no_loc_semi <- renderValueBox({
    no_loc <- no_loc()
      
      valueBox(
        value = no_loc[no_loc$"habitat_type" == "Semi-nat",]$no_loc,
        subtitle = "Antall lokaliteter i semi-nat",
        color = "yellow",
        width = 2
      )
    })
   

    output$no_loc_forest <- renderValueBox({
      no_loc <- no_loc()
      
      valueBox(
        value = no_loc[no_loc$"habitat_type" == "Forest",]$no_loc,
        subtitle = "Antall lokaliteter i skog",
        color = "aqua"
      )
    })

    
    no_sampl <- reactive({
      
    con <- login_import$con()
    
    no_sampl_q <- "
      SELECT count(st.*)::numeric no_sampl
      FROM events.year_locality yl,
      events.locality_sampling ls,
      events.sampling_trap st
      WHERE yl.id = ls.year_locality_id
      AND ls.id = st.locality_sampling_id
      AND yl.project_short_name = 'NasIns'
      AND ls.end_date IS NOT NULL
      "
    
    no_sampl <- dbGetQuery(con,
                         no_sampl_q)
    return(no_sampl)
    })
    
    
    output$no_sampl <- renderValueBox({
      no_sampl <- no_sampl()
      
      valueBox(
        value = no_sampl$no_sampl,
        subtitle = "Antall felleprøver",
        color = "aqua"
      )
    })
    
    
    get_year_locality_stats <- function(){
      #con <- login_import$con()
      
      project_year_localities <-tbl(con,
                                    Id(schema = "views",
                                       table = "project_year_localities")
                                    ) %>% 
        mutate(habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type))
      
      proj_sum <- project_year_localities %>% 
                  filter(project_short_name == "NasIns") %>%
                  collect() %>% 
                  mutate(region_name = factor(region_name, 
                                              levels = c("Sørlandet", 
                                                         "Østlandet", 
                                                         "Vestlandet", 
                                                         "Trøndelag", 
                                                         "Nord-Norge")
                                              )
                         ) %>% 
                  mutate(habitat_type = factor(habitat_type)) %>% 
                  mutate(year = factor(year, levels = 2024:2020)) %>% 
                  group_by(region_name,
                           habitat_type,
                           year,
                           .drop = FALSE) %>% 
                  summarise(visits = as.integer(n()),
                            .groups = "drop") %>% 
                 # mutate(year = as.numeric(as.character(year))) %>% 
                  mutate(habitat_type = as.character(habitat_type)) %>% 
                  arrange(region_name,
                          year, 
                          habitat_type) 

      
      return(proj_sum)
      
    }
    
    
    plot_project_sum <- function(){
      
      raw_data <- get_year_locality_stats()
      
      fill_cols <- c("Semi-nat" = "#E57200", 
                     "Skog" = "#7A9A01",
                     "Ikke besøkt" = "white")
      
      reg_cols <- tibble(region_name = c("Sørlandet",
                                         "Østlandet",
                                         "Vestlandet",
                                         "Trøndelag",
                                         "Nord-Norge"),
                         color = c("#E57200",
                                   "#008C95",
                                   "#7A9A01",
                                   "#93328E",
                                   "#004F71")
                    
                      )
      
      plot_data <- raw_data %>% 
           group_by(region_name, year) %>% 
        mutate(custom_y = cur_group_id()) %>% 
        mutate(year = as.integer(as.character(year))) %>% 
        mutate(custom_x = ifelse(habitat_type == "Semi-nat", 
                                 as.integer(as.character(year)) - 0.2, 
                                 as.integer(as.character(year)) + 0.2),
               visited = ifelse(visits > 0, "Ja", "Nei")) 

     
      yintercepts <- tibble(y = c(0, 5, 10, 15, 20, 25) + 0.5) 
      
      p <-  ggplot(plot_data,
             aes(x = custom_x,
                 y = custom_y)
             ) +

         geom_hline(aes(yintercept = y),
                    lty = 3,
                    data = yintercepts) +
        
          geom_tile(aes(fill = visited,
                        color = habitat_type),
                    width = .3, 
                    height = .9,
                    lwd = 1) + 
        
         scale_fill_manual(name = "Registrert", 
                              values=c("black", "white")) +
        
         scale_color_manual(name = "Habitattype",
                            values = fill_cols,
                            aesthetics = "colour") +
        ylab("") +
        #xlab("År") +
        scale_x_continuous(name = "År",
                           breaks = unique(plot_data$year)) +
        scale_y_continuous(breaks = c(0, 5, 10, 15, 20) + 2.5,
                           labels = c("<b style='color:#E57200'>Sørlandet</b>",
                                      "<b style='color:#008C95'>Østlandet</b>",
                                      "<b style='color:#7A9A01'>Vestlandet</b>",
                                      "<b style='color:#93328E'>Trøndelag</b>",
                                      "<b style='color:#004F71'>Nord-Norge</b>")) +
        theme(panel.background = element_blank(),
             axis.text.y = ggtext::element_markdown()) 
      
      p
      # 
      # loc_text_1 <- grid::textGrob("Lok 01-10", gp = grid::gpar(fontsize = 8))
      # loc_text_2 <- grid::textGrob("Lok 11-20", gp = grid::gpar(fontsize = 8))
      # loc_text_3 <- grid::textGrob("Lok 21-30", gp = grid::gpar(fontsize = 8))
      # loc_text_4 <- grid::textGrob("Lok 31-40", gp = grid::gpar(fontsize = 8))
      # loc_text_5 <- grid::textGrob("Lok 41-50", gp = grid::gpar(fontsize = 8))
      # 
      # p +
      #   annotation_custom(loc_text_1, xmin = 2019, xmax = 2019,  ymin = 5, ymax = 5) +
      #   annotation_custom(loc_text_1, xmin = 2019, xmax = 2019,  ymin = 10, ymax = 10) +
      #   annotation_custom(loc_text_1, xmin = 2019, xmax = 2019,  ymin = 15, ymax = 15) +
      #   annotation_custom(loc_text_1, xmin = 2019, xmax = 2019,  ymin = 20, ymax = 20) +
      #   annotation_custom(loc_text_1, xmin = 2019, xmax = 2019,  ymin = 25, ymax = 25) +
      #   
      #   annotation_custom(loc_text_2, xmin = 2019, xmax = 2019,  ymin = 5 - 1, ymax = 5 - 1) +
      #   annotation_custom(loc_text_2, xmin = 2019, xmax = 2019,  ymin = 10 - 1, ymax = 10 - 1) +
      #   annotation_custom(loc_text_2, xmin = 2019, xmax = 2019,  ymin = 15 - 1, ymax = 15 - 1) +
      #   annotation_custom(loc_text_2, xmin = 2019, xmax = 2019,  ymin = 20 - 1, ymax = 20 - 1) +
      #   annotation_custom(loc_text_2, xmin = 2019, xmax = 2019,  ymin = 25 - 1, ymax = 25 - 1) +
      #   
      #   annotation_custom(loc_text_3, xmin = 2019, xmax = 2019,  ymin = 5 - 2, ymax = 5 - 2) +
      #   annotation_custom(loc_text_3, xmin = 2019, xmax = 2019,  ymin = 10 - 2, ymax = 10 - 2) +
      #   annotation_custom(loc_text_3, xmin = 2019, xmax = 2019,  ymin = 15 - 2, ymax = 15 - 2) +
      #   annotation_custom(loc_text_3, xmin = 2019, xmax = 2019,  ymin = 20 - 2, ymax = 20 - 2) +
      #   annotation_custom(loc_text_3, xmin = 2019, xmax = 2019,  ymin = 25 - 2, ymax = 25 - 2) +
      #   coord_cartesian(clip = 'off')
      #   
      #   
      # p  
      
      
    }
    
    nor <- Norimon::get_map()
    
    plot_region_map <- function(){
      
      p <- ggplot(nor) +
           geom_sf(aes(fill = region)) +
           scale_fill_nina(name = "") +
        guides(fill = "none") +
        ggthemes::theme_map()
      
      p
      
    }
    
    
    output$project_sum_map <- renderPlot({
      
      plot1 <- plot_project_sum()
      plot2 <- plot_region_map()
      
      gridExtra::grid.arrange(plot2, 
                              plot1, 
                              ncol = 2,
                              widths = c(unit(6, "cm"), unit(10, "cm"))
      )
      
    })

  

})
}
