require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(shinydashboard)
require(shinyWidgets)


get_map <- function (region_subset = NULL,
                     con = con) 
{
  norway_terr <- sf::read_sf(con, layer = DBI::Id(schema = "backgrounds", 
                                                  table = "norway_terrestrial")) %>% select(fylke = navn)
  region_def <- tibble(region = c("Trøndelag", "Østlandet", 
                                  "Østlandet", "Østlandet", "Østlandet", "Sørlandet", 
                                  "Sørlandet", "Vestlandet", "Vestlandet", "Nord-Norge", 
                                  "Nord-Norge"), fylke = c("Trøndelag", "Innlandet", "Oslo", 
                                                           "Vestfold og Telemark", "Viken", "Rogaland", "Agder", 
                                                           "Vestland", "Møre og Romsdal", "Troms og Finnmark", 
                                                           "Nordland"))
  norway_terr <- norway_terr %>% left_join(region_def, by = c(fylke = "fylke"))
  if (!is.null(region_subset)) {
    norway_terr <- norway_terr %>% filter(region %in% region_subset)
  }
  return(norway_terr)
}


dashboard_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Dashboard",
    fluidRow(
      column(1),
      column(3, {
        valueBoxOutput(ns("no_loc_semi"), width = 12)
      }),
      column(3, {
        valueBoxOutput(ns("no_loc_forest"), width = 12)
      }),
      column(3, {
        valueBoxOutput(ns("no_sampl"), width = 12)
      })
    ),
    column(
      12,
      shinydashboardPlus::box(
        width = 12,
        id = ns("omdrevbox"),
        title = "Etablering og omdrev (10 lok. per region-habitat-år)",
        height = "200px",
        shinycssloaders::withSpinner(
          {
            plotOutput(ns("project_sum_map"),
              height = "270px",
              width = "1000px"
            )
          },
          type = 2,
          color = "#E57200",
          color.background = "#004F71"
        )
      )
    ),
    br(),
    column(
      6,
      shinydashboardPlus::box(
        id = ns("taxasharebox"),
        width = 12,
        height = "400px",
        title = "Taksonomisk fordeling",
        div(
        style = "display:inline-block; padding-left: 20px",
        radioButtons(ns("taxa_plot_type"),
          label = "Plot-type",
          choiceNames = c(
            "Stabel",
            "Smultring (fam. i ytre ring)"
          ),
          choiceValues = c(
            "barplot",
            "donut"
          ),
          inline = TRUE
        )
        ),
        shinycssloaders::withSpinner(
          {
            plotOutput(ns("taxa_share_plot"),
              height = "300px"
            )
          },
          type = 2,
          color = "#E57200",
          color.background = "#004F71"
        )
      ),
    ),
    column(
      6,
      shinydashboardPlus::box(
        id = ns("notteskallbox"),
        width = 12,
        height = "400px",
        title = "Fangstmengde",
        div(
          style = "display:inline-block; padding-left: 20px",
          radioButtons(ns("data_type"),
                       label = "Datatype",
                       choiceNames = c(
                         "Antall arter",
                         "Biomasse"
                       ),
                       choiceValues = c(
                         "species",
                         "biomass"
                       ),
                       width = "100px"
          )
        ),
        div(
          style = "display:inline-block; padding-left: 20px",
          radioButtons(ns("agg_level"),
                       label = "Funn per",
                       choiceNames = c(
                         "Lokalitet-sampling",
                         "Lokalitet-sesong"
                       ),
                       choiceValues = c(
                         "Sampling",
                         "Sesong"
                       ),
                       width = "150px"
          )
        ),
        div(
          style = "display:inline-block; padding-left: 20px",
          radioButtons(ns("rank_dens"),
                       label = "Plot-type",
                       choices = c(
                         "Ranking",
                         "Fordeling"
                       ),
                       width = "100px"
          )
        ),
        shinycssloaders::withSpinner(
          {
            plotOutput(ns("catch_sum_biomass"),
              height = "300px"
            )
          },
          type = 2,
          color = "#E57200",
          color.background = "#004F71"
        )
      )
    )
  )
}


dashboard_server <- function(id, login_import) {
  ns <- NS(id)

  moduleServer(id, function(input, output, session) {
    # no_loc <- reactive({
    # con <- login_import$con()

    no_loc_q <- "
      SELECT habitat_type, count(distinct(yl.locality_id))::numeric no_loc
      FROM events.year_locality yl,
      locations.localities l
      WHERE yl.locality_id = l.id
      AND yl.project_short_name = 'NorIns'
      GROUP BY l.habitat_type
      "

    no_loc <- reactive({dbGetQuery(
      login_import$con,
      no_loc_q
    )
    })

    # return(no_loc)

    # })


    output$no_loc_semi <- renderValueBox({
      res <- no_loc()
      
      valueBox(
        value = res[res$"habitat_type" == "Semi-nat", ]$no_loc,
        subtitle = "lokaliteter nå i semi-naturlig mark",
        color = "yellow",
        width = 2
      )
    })


    output$no_loc_forest <- renderValueBox({
       no_loc <- no_loc()

      valueBox(
        value = no_loc[no_loc$"habitat_type" == "Forest", ]$no_loc,
        subtitle = "lokaliteter nå i skog",
        color = "aqua"
      )
    })


    # no_sampl <- reactive({

    # con <- login_import$con()

    no_sampl_q <- "
      SELECT count(st.*)::numeric no_sampl
      FROM events.year_locality yl,
      events.locality_sampling ls,
      events.sampling_trap st
      WHERE yl.id = ls.year_locality_id
      AND ls.id = st.locality_sampling_id
      AND yl.project_short_name = 'NorIns'
      AND ls.end_date IS NOT NULL
      "

    no_sampl <- dbGetQuery(
      login_import$con,
      no_sampl_q
    )



    output$no_sampl <- renderValueBox({
      valueBox(
        value = no_sampl$no_sampl,
        subtitle = "innsamlete felleprøver",
        color = "aqua"
      )
    })


    year_locality_stats <- readr::read_rds("data/year_locality_stats.Rdata")
    nor <- get_map(con = login_import$con)
    
    
    output$project_sum_map <- renderPlot(expr = {
      plot1 <- Norimon::plot_country_map(nor)
      plot2 <- Norimon::plot_year_region_stats(year_locality_stats)


      cowplot::plot_grid(plot1,
        plot2,
        ncol = 2,
        rel_widths = c(1 / 5, 4 / 5)
      )
    })


    rank_plot <- function(x,
                          dataset) {
      df <- x
      p <- ggplot(data = df[[dataset]])

      p <- p +
        geom_bar(
          aes(
            y = value,
            x = row_number
          ),
          stat = "identity"
        ) +
        xlab("Rangert rekkefølge") +
        geom_segment(
          aes(
            y = median(value),
            yend = median(value),
            x = min(row_number),
            xend = max(row_number),
            linetype = "Median"
          ),
          color = nina_colors[2]
        ) +
        geom_segment(
          aes(
            y = mean(value),
            yend = mean(value),
            x = min(row_number),
            xend = max(row_number),
            linetype = "Middelv."
          ),
          color = nina_colors[3]
        ) +
        scale_linetype_discrete(
          name = "",
          guide = guide_legend(override.aes = list(color = c(nina_colors[2], nina_colors[3])))
        )

      if (dataset == "biomass") {
        p <- p +
          ylab("Våtvekt (g.)")
      }

      if (dataset == "species") {
        p <- p +
          ylab("Antall arter")
      }



      return(p)
    }


    dens_plot <- function(x,
                          dataset) {
      df <- x
      p <- ggplot(data = df[[dataset]])

      p <- p +
        geom_density(aes(x = value)) +
        ylab("Densitet")


      if (dataset == "biomass") {
        p <- p +
          xlab("Våtvekt (g.)")
      }

      if (dataset == "species") {
        p <- p +
          xlab("Antall arter")
      }


      return(p)
    }

    catch_per_locality_sampling <- rlist::list.load("data/catch_per_locality_sampling_data.Rdata")
    catch_per_year_locality <- rlist::list.load("data/catch_per_year_locality_data.Rdata")


    output$catch_sum_biomass <- renderCachedPlot(
      expr = {
        if (input$agg_level == "Sampling") {
          if (input$rank_dens == "Ranking") {
            p <- rank_plot(catch_per_locality_sampling,
              dataset = input$data_type
            )
          } else {
            p <- dens_plot(catch_per_locality_sampling,
              dataset = input$data_type
            )
          }
        } else {
          if (input$rank_dens == "Ranking") {
            p <- rank_plot(catch_per_year_locality,
              dataset = input$data_type
            )
          } else {
            p <- dens_plot(catch_per_year_locality,
              dataset = input$data_type
            )
          }
        }

        p <- p +
          theme(panel.background = element_blank())

        return(p)
      },
      cacheKeyExpr = list(input$agg_level, input$rank_dens, input$data_type)
    )


    taxonomic_perc <- rlist::list.load("data/taxonomic_perc_data.Rdata")

    taxa_donut_plot <- function(trap_type = c(
                                  "Malaise",
                                  "Window"
                                ),
                                legend.position = "bottom",
                                ggtitle = "none") {
      data_list <- taxonomic_perc

      if (trap_type == "Malaise") {
        p <- ggplot() +
          geom_col(aes(x = 2, y = share, fill = id_order),
            data = data_list$order_level_data_mf,
            color = "black"
          ) +
          geom_col(aes(x = 3, y = share, fill = id_order),
            data = data_list$family_level_data_mf,
            color = "black"
          ) +
          # geom_text(aes(label = Group, x= 3, y = Pos), data = metadata2, size = 3)+
          xlim(0, 3.5) +
          labs(x = NULL, y = NULL) +
          theme(
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank()
          )
      } else {
        p <- ggplot() +
          geom_col(aes(x = 2, y = share, fill = id_order),
            data = data_list$order_level_data_vf, color = "black"
          ) +
          geom_col(aes(x = 3, y = share, fill = id_order),
            data = data_list$family_level_data_vf, color = "black"
          ) +
          # geom_text(aes(label = Group, x= 3, y = Pos), data = metadata2, size = 3)+
          xlim(0, 3.5) +
          labs(x = NULL, y = NULL) +
          theme(
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank()
          )
      }

      p <- p + coord_polar(theta = "y") +
        theme(legend.position = legend.position) +
        ggtitle(ggtitle)

      return(p)
    }


    taxa_barplot <- function() {
      data_list <- taxonomic_perc

      data_comb_order <- data_list$order_level_data_mf %>%
        rbind(data_list$order_level_data_vf) %>%
        mutate(id_order = factor(id_order, levels = data_list$order_level_data_mf$id_order))

      data_comb_family <- data_list$family_level_data_mf %>%
        rbind(data_list$family_level_data_vf)


      ggplot() +
        geom_bar(
          aes(
            y = share,
            x = id_order,
            group = trap_type,
            fill = trap_type
          ),
          data = data_comb_order,
          stat = "identity",
          position = "dodge"
        ) +
        scale_fill_nina(
          name = "Felletype",
          palette = "darkblue-orange"
        ) +
        ylab("Andel arter") +
        xlab("Orden") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }


    output$taxa_share_plot <- renderCachedPlot(
      expr = {
        if (input$taxa_plot_type == "donut") {
          plot1 <- taxa_donut_plot("Malaise",
            ggtitle = "Malaisefelle",
            legend.position = "bottom"
          ) +
            scale_fill_nina(
              name = "Orden",
              breaks = c(
                "Diptera",
                "Hymenoptera",
                "Lepidoptera",
                "Hemiptera",
                "Coleoptera",
                "Psocoptera",
                "Trichoptera",
                "Neuroptera"
              )
            ) +
            guides(fill = guide_legend(nrow = 3))
          # theme(legend.text = element_text(color = "white"),
          #       legend.title = element_text(color = "white"),
          #       legend.key = element_rect(fill = "white")) +
          # guides(fill = guide_legend(nrow = 3,
          #                            override.aes= list(alpha = 0, color = "white"))) +
          # theme(legend.key=element_rect(colour="white"))


          plot2 <- taxa_donut_plot("Window",
            ggtitle = "Vindusfelle",
            legend.position = "bottom"
          ) +
            scale_fill_nina(
              name = "Orden",
              breaks = c(
                "Diptera",
                "Hymenoptera",
                "Lepidoptera",
                "Hemiptera",
                "Coleoptera",
                "Psocoptera",
                "Trichoptera",
                "Neuroptera"
              )
            ) +
            guides(fill = guide_legend(nrow = 3))


          gridExtra::grid.arrange(plot1,
            plot2,
            ncol = 2,
            widths = c(
              unit(5, "cm"),
              unit(5, "cm")
            )
          )
        }

        if (input$taxa_plot_type == "barplot") {
          taxa_barplot()
        }
      },
      cacheKeyExpr = input$taxa_plot_type
    )
  })
}
