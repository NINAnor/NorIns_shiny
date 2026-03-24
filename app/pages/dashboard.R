require(DBI)
require(dbplyr)
require(dplyr)
require(forcats)
require(tidyr)
require(shinydashboard)


dashboard_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Dashboard",
    fluidRow(
      column(1),
      column(3, {
        valueBoxOutput(ns("no_loc_semi"),
          width = 12
        )
      }),
      column(3, {
        valueBoxOutput(ns("no_loc_forest"),
          width = 12
        )
      }),
      column(3, {
        valueBoxOutput(ns("no_sampl"),
          width = 12
        )
      })
    ),
    column(
      12,
      shinydashboardPlus::box(
        width = 12,
        id = "omdrevbox",
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
        id = "taxa_share",
        width = 12,
        title = "Taksonomisk fordeling",
        height = "400px",
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
          # selected = "donut",
          width = "100px"
        ),
        shinycssloaders::withSpinner(
          {
            plotOutput(ns("taxa_share"),
              height = "300px"
            )
          },
          type = 2,
          color = "#E57200",
          color.background = "#004F71"
        )
      ),

      # "#004F71"  "#008C95"  "#E57200"  "#93328E"  "#7A9A01"  "#A2AAAD"  "#2DCCD3"  "#FFB25B
    ),
    column(
      6,
      shinydashboardPlus::box(
        id = "notteskallbox",
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

    no_loc <- dbGetQuery(
      login_import$con,
      no_loc_q
    )

    # return(no_loc)

    # })


    output$no_loc_semi <- renderValueBox({
      # no_loc <- no_loc()

      valueBox(
        value = no_loc[no_loc$"habitat_type" == "Semi-nat", ]$no_loc,
        subtitle = "lokaliteter nå i semi-naturlig mark",
        color = "yellow",
        width = 2
      )
    })


    output$no_loc_forest <- renderValueBox({
      # no_loc <- no_loc()

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
    # return(no_sampl)
    # })


    output$no_sampl <- renderValueBox({
      # no_sampl <- no_sampl()

      valueBox(
        value = no_sampl$no_sampl,
        subtitle = "innsamlete felleprøver",
        color = "aqua"
      )
    })


    year_locality_stats <- readr::read_rds("data/year_locality_stats.Rdata")


    plot_project_sum <- function() {
      # par(mar = rep(0, 4))

      raw_data <- year_locality_stats

      fill_cols <- c(
        "Semi-nat" = "#E57200",
        "Skog" = "#7A9A01",
        "Ikke besøkt" = "white"
      )

      reg_cols <- tibble(
        region_name = c(
          "Sørlandet",
          "Østlandet",
          "Vestlandet",
          "Trøndelag",
          "Nord-Norge"
        ),
        color = c(
          "#E57200",
          "#008C95",
          "#7A9A01",
          "#93328E",
          "#004F71"
        )
      )

      plot_data <- raw_data %>%
        group_by(region_name, year) %>%
        mutate(custom_y = cur_group_id()) %>%
        mutate(year = as.integer(as.character(year))) %>%
        mutate(
          custom_x = ifelse(habitat_type == "Semi-nat",
            as.integer(as.character(year)) - 0.2,
            as.integer(as.character(year)) + 0.2
          ),
          visited = ifelse(visits > 0, "Ja", "Nei")
        )


      yline_pos <- tibble(hline = seq(0,
        length(levels(plot_data$region_name)) * n_distinct(plot_data$year),
        by = n_distinct(plot_data$year)
      ) + 0.5)


      ytext_pos <- tibble(ytext = seq(0,
        (length(levels(plot_data$region_name)) - 1) * n_distinct(plot_data$year),
        by = n_distinct(plot_data$year)
      ) + (n_distinct(plot_data$year) + 1) / 2)

      p <- ggplot(
        plot_data,
        aes(
          x = custom_x,
          y = custom_y
        )
      ) +
        geom_hline(aes(yintercept = hline),
          lty = 3,
          data = yline_pos
        ) +
        geom_tile(
          aes(
            fill = visited,
            color = habitat_type
          ),
          width = .3,
          height = .9,
          lwd = 1
        ) +
        scale_fill_manual(
          name = "Registrert",
          values = c("black", "white")
        ) +
        scale_color_manual(
          name = "Habitattype",
          values = fill_cols,
          aesthetics = "colour"
        ) +
        ylab("") +
        # xlab("År") +
        scale_x_continuous(
          name = "År",
          breaks = unique(plot_data$year)
        ) +
        scale_y_continuous(
          breaks = ytext_pos$ytext,
          labels = c(
            "<b style='color:#E57200'>Sørlandet</b>",
            "<b style='color:#008C95'>Østlandet</b>",
            "<b style='color:#7A9A01'>Vestlandet</b>",
            "<b style='color:#93328E'>Trøndelag</b>",
            "<b style='color:#004F71'>Nord-Norge</b>"
          )
        ) +
        theme(
          panel.background = element_blank(),
          axis.text.y = ggtext::element_markdown(),
          plot.margin = margin(0, 0, 0, 0, "cm")
        )

      p
    }

    nor <- get_map(con = login_import$con)

    plot_region_map <- function() {
      # par(mar = rep(0, 4))

      p <- ggplot(nor) +
        geom_sf(aes(fill = region)) +
        scale_fill_nina(name = "") +
        guides(fill = "none") +
        theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
        ggthemes::theme_map()


      p
    }


    output$project_sum_map <- renderPlot(expr = {
      plot1 <- plot_region_map()
      plot2 <- plot_project_sum()

      # gridExtra::grid.arrange(plot1,
      #                         plot2,
      #                         ncol = 2,
      #                         widths = c(unit(6, "cm"),
      #                                    unit(10, "cm")),
      #                         heights = c(unit(6, "cm"),
      #                                     unit(6, "cm")),
      #                         padding = 0)

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


    output$taxa_share <- renderCachedPlot(
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
