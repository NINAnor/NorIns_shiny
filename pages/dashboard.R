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
                      title = "Etablering og omdrev",
                      height = "400px",

                      plotOutput(ns("project_sum_map"),
                                 height = "300px"))
           
                  ,
             br(),
             box(width = 12,
                 title = "Taksonomisk fordeling",
                 plotOutput(ns("taxa_share"),
                            height = "300px"),
                 height = "400px"
             )),
           column(6,
            shinydashboardPlus::box(id = "notteskallbox",
                                    width = 12,
                  title = "Fangstmengde",
                  div(style = "display:inline-block; padding-left: 20px", 
                      radioButtons(ns("data_type"),
                                   label = "Datatype",
                                   choiceNames = c("Antall arter",
                                               "Biomasse"),
                                   choiceValues = c("species",
                                              "biomass"),
                                   width = "100px")),
                  div(style = "display:inline-block; padding-left: 20px", 
                  radioButtons(ns("agg_level"),
                               label = "Funn per",
                               choices = c("Sampling",
                                           "Sesong"),
                               width = "100px")),
                  div(style = "display:inline-block; padding-left: 20px", 
                  radioButtons(ns("rank_dens"),
                               label = "Plot-type",
                               choices = c("Ranking",
                                           "Fordeling"),
                               width = "100px")),
                 
                  plotlyOutput(ns("catch_sum_biomass"),
                             height = "300px"),
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
        subtitle = "registrerte lokaliteter i semi-nat",
        color = "yellow",
        width = 2
      )
    })
   

    output$no_loc_forest <- renderValueBox({
      no_loc <- no_loc()
      
      valueBox(
        value = no_loc[no_loc$"habitat_type" == "Forest",]$no_loc,
        subtitle = "registrerte lokaliteter i skog",
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
        subtitle = "innsamlete felleprøver",
        color = "aqua"
      )
    })
    
    
    get_year_locality_stats <- function(){
      con <- login_import$con()
      
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
                  mutate(year = factor(year, levels = max(year):min(year))) %>% 
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

     
      yline_pos <- tibble(hline = seq(0, 
                                    length(levels(plot_data$region_name)) * n_distinct(plot_data$year), 
                                    by =  n_distinct(plot_data$year)) + 0.5
      )
      
      
      ytext_pos <- tibble(ytext = seq(0, 
                                        (length(levels(plot_data$region_name))-1) * n_distinct(plot_data$year), 
                                        by =  n_distinct(plot_data$year)) + (n_distinct(plot_data$year) + 1) /2) 
      
      p <-  ggplot(plot_data,
             aes(x = custom_x,
                 y = custom_y)
             ) +

         geom_hline(aes(yintercept = hline),
                    lty = 3,
                    data = yline_pos) +
        
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
        scale_y_continuous(breaks = ytext_pos$ytext,
                           labels = c("<b style='color:#E57200'>Sørlandet</b>",
                                      "<b style='color:#008C95'>Østlandet</b>",
                                      "<b style='color:#7A9A01'>Vestlandet</b>",
                                      "<b style='color:#93328E'>Trøndelag</b>",
                                      "<b style='color:#004F71'>Nord-Norge</b>")) +
        theme(panel.background = element_blank(),
             axis.text.y = ggtext::element_markdown()) 
      
      p
      
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
    
    
    catch_per_locality_sampling <- function(){
      con <- login_import$con()
      
      biomass_per_ls_q <- "
          SELECT (row_number() OVER(ORDER BY(round(sum(st.wet_weight)::numeric, 2)) DESC))::integer,
          ls.id,
	ls.sampling_name,
	l.locality,
    yl.year,
	l.region_name,
    l.habitat_type,
	tt.trap_type,
    round(sum(st.wet_weight)::numeric, 2) as value	
   	FROM 
    events.sampling_trap st,
    events.locality_sampling ls,
    events.year_locality yl,
    locations.localities l,
    locations.traps,
    lookup.trap_types tt
	WHERE st.locality_sampling_id = ls.id AND 
	ls.year_locality_id = yl.id AND 
	yl.locality_id = l.id AND 
	st.trap_id = traps.id AND 
	traps.trap_model = tt.trap_model
	AND yl.project_short_name = 'NasIns'	
	AND ls.end_date IS NOT NULL
	AND ls.start_date IS NOT NULL
	AND st.wet_weight IS NOT NULL
	GROUP BY ls.id, yl.year, l.region_name, l.habitat_type, l.locality, tt.trap_type

  "
      
      biomass_per_ls <- dbGetQuery(con,
                                     biomass_per_ls_q) %>% 
        filter(value > 0)
      
      
      tot_spec_per_ls_q <- "
      SELECT *
      FROM views.no_spec_locality_sampling
      "
      
      tot_spec_per_ls <- dbGetQuery(con,
                                      tot_spec_per_ls_q) %>% 
        arrange(desc(tot_no_spec)) %>% 
        mutate(row_number = as.integer(row_number()),
               value = as.integer(tot_no_spec)) %>% 
        select(- tot_no_spec)
      
      out <- list("biomass" = biomass_per_ls,
                  "species" = tot_spec_per_ls)
      
      return(out)
      
    }
    
    
    catch_per_year_locality <- function(){
      
      con <- login_import$con()
      
      biomass_per_yl_q <- "
       SELECT (row_number() OVER(ORDER BY(round(sum(st.wet_weight)::numeric, 2)) DESC))::integer ,
       yl.id,
	l.locality,
    yl.year,
	l.region_name,
    l.habitat_type,
	tt.trap_type,
    round(sum(st.wet_weight)::numeric, 2) as value	
   	FROM 
    events.sampling_trap st,
    events.locality_sampling ls,
    events.year_locality yl,
    locations.localities l,
    locations.traps,
    lookup.trap_types tt
	WHERE st.locality_sampling_id = ls.id AND 
	ls.year_locality_id = yl.id AND 
	yl.locality_id = l.id AND 
	st.trap_id = traps.id AND 
	traps.trap_model = tt.trap_model
	AND yl.project_short_name = 'NasIns'	
	AND ls.end_date IS NOT NULL
	AND ls.start_date IS NOT NULL
	AND st.wet_weight IS NOT NULL
	GROUP BY yl.id, yl.year, l.region_name, l.habitat_type, l.locality, tt.trap_type

      "
      
      biomass_per_yl <- dbGetQuery(con,
                                   biomass_per_yl_q)
      
      
      
      tot_spec_per_yl_q <- "
      SELECT *
      FROM views.no_spec_year_locality
      "
      
      tot_spec_per_yl <- dbGetQuery(con,
                                    tot_spec_per_yl_q) %>% 
        arrange(desc(tot_no_spec)) %>% 
        mutate(row_number = as.integer(row_number()),
               value = as.integer(tot_no_spec)) %>% 
        select(- tot_no_spec)
        
      
      out <- list("biomass" = biomass_per_yl,
                  "species" = tot_spec_per_yl)
      
      return(out)
      
    }
    
    
    rank_plot <- function(x,
                          dataset){
      
      df <- x
      p <-  ggplot(data = df[[dataset]])
      
      p <- p +
        geom_bar(aes(y = value,
                   x = row_number),
               stat = "identity") +
        xlab("Rangert rekkefølge") +
        geom_segment(aes(y = median(value), 
                         yend = median(value),
                         x = min(row_number),
                         xend = max(row_number),
                         linetype = "Median"),
                     color = nina_colors[2]) +
        geom_segment(aes(y = mean(value), 
                         yend = mean(value),
                         x = min(row_number),
                         xend = max(row_number),
                         linetype = "Middelv."),
                     color = nina_colors[3]) +
        scale_linetype_discrete(name = "",
                                guide = guide_legend(override.aes = list(color = c(nina_colors[2], nina_colors[3])))) 
      
      if(dataset == "biomass"){
        p <- p +
          ylab("Våtvekt (g.)")
      } 
      
      if(dataset == "species"){
        p <- p +
          ylab("Antall arter")
      } 
      
      
      
      return(p)
      
    }
    
    
    dens_plot <- function(x,
                          dataset){
      df <- x
      p <- ggplot(data = df[[dataset]])
      
      p <- p +
        geom_density(aes(x = value)) +
        ylab("Densitet")
        
      
      if(dataset == "biomass"){
        p <- p +
          xlab("Våtvekt (g.)")
      } 
      
      if(dataset == "species"){
        p <- p +
          xlab("Antall arter")
      }
        
      
      return(p)
    }
    
    output$catch_sum_biomass <- renderPlotly({
      
     
      
      if(input$agg_level == "Sampling"){
          if(input$rank_dens == "Ranking"){
          p <- rank_plot(catch_per_locality_sampling(),
                         dataset = input$data_type)
        } else {
          p <- dens_plot(catch_per_locality_sampling(),
                         dataset = input$data_type)
           }
        } else {
        if(input$rank_dens == "Ranking"){
          p <- rank_plot(catch_per_year_locality(),
                         dataset = input$data_type)
        } else {
          p <- dens_plot(catch_per_year_locality(),
                         dataset = input$data_type)
        }
      }
      
      
      
      p <- p +
        theme(panel.background = element_blank())
        
      return(p)
      
    })

  
    
    taxonomic_perc <- function(){
      #con <- login_import$con()
      
      loc_traptype_species_list <- tbl(con,
                                       Id(schema = "views",
                                       table = "loc_traptype_species_list")) %>% 
        filter(id_class == "Insecta")
      
      order_level_data_mf <- loc_traptype_species_list %>% 
        filter(trap_type == "Malaise") %>% 
        collect() %>% 
        group_by(id_order,
                 trap_type) %>% 
        summarise(share = n()/nrow(.),
                  .groups = "drop")
      
      family_level_data_mf <- loc_traptype_species_list %>% 
        filter(trap_type == "Malaise")  %>% 
        collect() %>% 
        group_by(id_order, 
                 id_family,
                 trap_type) %>% 
        summarise(share = n()/nrow(.),
                  .groups = "drop")
      
      order_level_data_vf <- loc_traptype_species_list %>% 
        filter(trap_type == "Window") %>% 
        collect() %>% 
        group_by(id_order,
                 trap_type) %>% 
        summarise(share = n()/nrow(.),
                  .groups = "drop")
      
      family_level_data_vf <- loc_traptype_species_list %>% 
        filter(trap_type == "Window")  %>% 
        collect() %>% 
        group_by(id_order, 
                 id_family,
                 trap_type) %>% 
        summarise(share = n()/nrow(.),
                  .groups = "drop")
      
      list("order_level_data_mf" = order_level_data_mf,
           "family_level_data_mf" = family_level_data_mf,
           "order_level_data_vf" = order_level_data_vf,
           "family_level_data_vf" = family_level_data_vf
           )
    }
    
    
      my_donut_plot <- function(trap_type = c("Malaise", 
                                              "Window"),
                                legend.position = "bottom",
                                ggtitle = "none"){
        
        data_list <- taxonomic_perc()
      
        if(trap_type == "Malaise"){
        
         p <- ggplot() +
             geom_col(aes(x = 2, y = share, fill = id_order), 
                      data = data_list$order_level_data_mf, color = "black") + 
             geom_col(aes(x = 3, y = share, fill = id_order), 
                      data = data_list$family_level_data_mf, color = "black") +
             #geom_text(aes(label = Group, x= 3, y = Pos), data = metadata2, size = 3)+
             xlim(0, 3.5) + labs(x = NULL, y = NULL) + 
             theme(axis.ticks=element_blank(),
                   axis.text=element_blank(),
                   axis.title=element_blank()
                   ) 
        } else {
          p <- ggplot() +
            geom_col(aes(x = 2, y = share, fill = id_order), 
                     data = data_list$order_level_data_vf, color = "black") + 
            geom_col(aes(x = 3, y = share, fill = id_order), 
                     data = data_list$family_level_data_vf, color = "black") +
            #geom_text(aes(label = Group, x= 3, y = Pos), data = metadata2, size = 3)+
            xlim(0, 3.5) + labs(x = NULL, y = NULL) + 
            theme(axis.ticks=element_blank(),
                  axis.text=element_blank(),
                  axis.title=element_blank()
                  )
        }
         
         p <- p + coord_polar(theta = "y") +
           theme(legend.position = legend.position) +
           ggtitle(ggtitle)
        
        return(p)
     
      }
    
    output$taxa_share <- renderPlot({
      plot1 <- my_donut_plot("Malaise",
                             ggtitle = "Malaisefelle",
                             legend.position ="none")
      plot2 <- my_donut_plot("Window",
                             ggtitle = "Vindusfelle",
                             legend.position ="none")
    
    gridExtra::grid.arrange(plot1, 
                            plot2, 
                            ncol = 2,
                            widths = c(unit(10, "cm"), unit(10, "cm"))
    )
    
    })

})
}
