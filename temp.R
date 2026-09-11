asv_perc_min_ind <- tbl(
  login_import$con,
  Id(schema = "views",
     table = "asv_perc_min_ind")
)

sel_asv <- asv_perc_min_ind |>
      filter(species_latin_gbif == "Actia lamia",
             project_short_name == "NorIns") |>
      collect() |>
      mutate(seq_short = stringr::str_sub(sequence_id,
                                                  start = 1,
                                                  end = 8),
             asv = as_factor(sequence_id),
             perc_min_no_ind = round(perc_min_no_ind, 3)
      ) |>
      arrange(sequence_id)
      
      
      to_plot <- sel_asv |>
        select(
          locality,
          lat,
          lon,
          seq_short,
          min_no_ind,
          most_common_min_no_ind,
          sum_min_no_ind,
          #perc_min_no_ind,
          max_possible_no_ind
        ) |>
        distinct() |> # This is a workaround for multiple records in genetics.asv_sequences for the same sequence_id. Should only be one species_latin_fixed per sequence_id!
        pivot_wider(
          names_from = "seq_short",
          values_from = "min_no_ind",
          names_prefix = "seq_",
          values_fill = 0
        )
      
to_plot |> 
  filter(locality == 'Semi-nat_33') |> 
  print(width = Inf)



custom_colors <- function(){ 
  
  custom_colors <- sel_asv |> 
    #mutate(seq_short = paste0("seq_", seq_short)) |> 
    select(seq_short,
           color_val) |> 
    distinct() |> 
    mutate(custom_col = asv_colors(color_val)) |> 
    select(seq_short,
           custom_col) 
  
  return(custom_colors)
}


#seq_3e1aa6b0 shows as yellow on size 40 but as green on size 90.
c('seq_8ffdc1c6', 'seq_338532b8')
'#7B9309'  '#7D8A15', '#8B5163'


'#903B81','#807F24','#7B9309','#903A82','#7B9309','#7B9309','#807F24','#7B9309','#7D8917','#807F24','#7B9309','#93328E','#7D8A15','#7B9408','#7B9309','#8C4D68','#8B5163','#7C900D'

'#808080'

# Inside your Shiny server observer / render function:

observe({
  req(sel_asv)
  if (nrow(sel_asv) == 0) return()
  
  # 1. Convert long data frame into GPU-ready sf polygons
  pie_sf <- prepare_spatial_pie_slices(
    df = sel_asv,
    pie_scale_factor = input$pie_size, # Driven by your Shiny slider
    base_radius_m = 50                 # Minimum slice radius in meters
  )
  
  # 2. Assign palette hex codes directly matching seq_short
  if (input$color_mode == "Genetisk avstand") {
    col_ref <- custom_colors()
    color_map <- setNames(col_ref$custom_col, col_ref$seq_short)
    pie_sf$hex_color <- color_map[pie_sf$seq_short]
    pie_sf$hex_color[is.na(pie_sf$hex_color)] <- "#808080"
  } else {
    pal <- leaflet::colorFactor("Set1", domain = pie_sf$seq_short)
    pie_sf$hex_color <- pal(pie_sf$seq_short)
  }
  
  # 3. Render GPU WebGL Layer
  leafletProxy("asv_map") |>
    clearGlLayers() |> # Fast WebGL layer cleanup
    addGlPolygons(
      data = pie_sf,
      fillColor = pie_sf$hex_color,
      fillOpacity = 0.9,
      color = "#ffffff", # White slice borders
      weight = 0.5,
      popup = paste0(
        "<strong>Lokalitet: </strong>", pie_sf$locality, "<br>",
        "<strong>ASV: </strong>", pie_sf$seq_short, "<br>",
        "<strong>Andel: </strong>", round(pie_sf$perc_min_no_ind * 100, 1), "%"
      ),
      group = "pie_gl_layer"
    )
})