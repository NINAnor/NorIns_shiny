



basemap <- leaflet(width = "100%",
                   height = "400px") %>% 
  addTiles(group = "OpenStreetMap")


asv_to_leaflet <- function(species = "NULL"){

  sel_asv <- asv_perc_reads %>%
  filter(species_latin_gbif == species)  %>%
  #  filter(!!input$species_select_asv %in% (species_latin_gbif)) %>%
  mutate(asv = as.factor(sequence_id),
         perc_reads = round(perc_reads*100, 0))
  
  
  to_plot  <- sel_asv %>% 
  mutate(lat = st_coordinates(geometry)[,2],
         lon = st_coordinates(geometry)[,1]) %>%
  st_drop_geometry() %>% 
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


asv_leaflet <- function(species = NULL){
  
  to_plot <- asv_to_leaflet(species)
  
  basemap %>% 
   addMinicharts(to_plot$lon,
                to_plot$lat,
                type = "pie",
                chartdata = to_plot[, which(grepl("seq_", names(to_plot)))],
                width = log(to_plot$sum_reads) * 2 ,
                legend = FALSE,
               )
  
  
}

  asv_leaflet(species = "Malthodes fuscus")

