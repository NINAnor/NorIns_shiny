#require(Norimon)
require(sf)
#library(tidyverse)
library(dplyr)
library(tidyr)
require(DBI)

load("data/shinyPass.Rdata")
Norimon::connect_to_insect_db(user = my_username,
                     password = my_password)

load("data/recalculate_number.Rdata")

shiny_rules <- tbl(con,
                   Id(schema = "lookup",
                      table = "shiny_rules"))

recalculate_status <- shiny_rules %>% 
                      select(recalculate_number) %>% 
                      pull()

if(recalculate_number != recalculate_status) {

  locality_sampling <- dplyr::tbl(con, dbplyr::in_schema("events", "locality_sampling"))
  
  
# prep data for div_map

redlisted_obs_2021 <- read_sf(con,
                              Id(schema = "views",
                                 table = "redlisted_obs")) %>% 
  mutate(kategori = kategori_2021)

redlisted_obs_2021 <- redlisted_obs_2021 %>% 
  #mutate(expl_fact = gsub("([a-zA-Z]*)( >)(.*)", "\\1", expl_fact)) %>% 
  separate(expl_fact, 
           into = c("expl_1",
                    "expl_2",
                    "expl_3",
                    "expl_4",
                    "expl_5"),
           sep = ">") %>% 
  separate(expl_3,
           into = "expl_3_main",
           sep = "_") %>%
  mutate(expl_3_main = ifelse(is.na(expl_3_main), expl_1, expl_3_main)) %>% 
  mutate(expl_3_main = stringr::str_trim(expl_3_main)) %>% 
  mutate(expl_3_main = ifelse(expl_3_main == " ", NA, expl_3_main),
         expl_3_main = ifelse(expl_3_main == "", NA, expl_3_main),
         expl_3_main = ifelse(expl_3_main == "Ukjent ", NA, expl_3_main),
         expl_3_main = ifelse(expl_3_main == "Ukjent", NA, expl_3_main)) 

redlisted_obs_2021 <- redlisted_obs_2021 %>%
  group_by(kategori) %>%
  mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>%
  ungroup() %>%
  filter(kategori != "DD")


redlisted_obs_2021_agg <- redlisted_obs_2021 %>% 
  group_by(locality,
           kategori) %>% 
  summarise(no_spec = n_distinct(species_latin_fixed)) %>% 
  ungroup() %>% 
  st_jitter(redlisted_obs_2021_agg, amount = 7000)  %>% 
  st_transform(4326)

##End get redlisted species

##Get pot alien species

fennoskand_obs_q <- "
      SELECT 
      obs.species_latin_fixed,
      yl.year,
      ls.sampling_name,
      l.locality,
      ST_Centroid(l.geom) as geom
      FROM occurrences.observations obs,
      lookup.fennoscand_species2 fennoscand,
      events.identifications,
      events.year_locality yl,
      events.locality_sampling ls,
      locations.localities l,
      events.sampling_trap st
      WHERE obs.identification_id = identifications.id
      AND obs.species_latin_fixed = fennoscand.species_latin_fixed
      AND identifications.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
      AND yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      AND obs.identification_confidence = 'HIGH'
      "

fennoskand_obs <- read_sf(con,
                          query = fennoskand_obs_q) %>% 
  mutate(kategori = "Fennoskandisk forek.")


pot_alien_obs_q <- "
      SELECT 
      obs.species_latin_fixed,
      yl.year,
      ls.sampling_name,
      l.locality,
      ST_Centroid(l.geom) as geom
      FROM occurrences.observations obs,
      lookup.pot_alien_species2 pot_alien,
      events.identifications,
      events.year_locality yl,
      events.locality_sampling ls,
      locations.localities l,
      events.sampling_trap st
      WHERE obs.identification_id = identifications.id
      AND obs.species_latin_fixed = pot_alien.species_latin_fixed
      AND identifications.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
      AND yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      AND obs.identification_confidence = 'HIGH'
      "

pot_alien_obs <- read_sf(con,
                         query = pot_alien_obs_q) %>% 
  mutate(kategori = "Potensielt fremmede arter")



alien_obs_q <- "
      SELECT 
      obs.species_latin_fixed,
      yl.year,
      ls.sampling_name,
      l.locality,
      ST_Centroid(l.geom) as geom
      FROM occurrences.observations obs,
      lookup.fremmedartslista_2018_artsdatabanken alien,
      events.identifications,
      events.year_locality yl,
      events.locality_sampling ls,
      locations.localities l,
      events.sampling_trap st
      WHERE obs.identification_id = identifications.id
      AND obs.species_latin_fixed = alien.\"scientificName\"
      AND identifications.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
      AND yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      AND alien.\"riskCategory\" IN ('NK', 'SE', 'HI', 'PH', 'LO')
      AND obs.identification_confidence = 'HIGH'
      "  

alien_obs <- read_sf(con,
                     query = alien_obs_q) %>% 
  mutate(kategori = "På fremmedartslista")


all_alien_obs <- fennoskand_obs %>% 
  rbind(pot_alien_obs)  %>% 
  rbind(alien_obs)


# kat_order <- tibble(kategori = c("Fennoskandisk forek.",
#                                  "Potensielt fremmede arter",
#                                  "På fremmedartslista"),
#                     kat_order = 1:3)

all_alien_obs_agg <- all_alien_obs %>% 
  group_by(kategori) %>% 
  mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>% 
  group_by(locality,
           kategori,
           no_spec_per_kat) %>% 
  summarise(no_spec = n_distinct(species_latin_fixed)) %>% 
  ungroup() %>% 
  mutate(kategori_append = factor(paste0(kategori, " (", no_spec_per_kat, " stk.)"))) %>% 
  # left_join(kat_order,
  #           by = c("kategori" = "kategori")) %>% 
  mutate(kategori = factor(kategori, levels = c("Fennoskandisk forek.",
                                                "Potensielt fremmede arter",
                                                "På fremmedartslista"))
  )

all_alien_obs_agg <- all_alien_obs_agg %>% 
  st_jitter(all_alien_obs_agg, amount = 7000)  %>% 
  st_transform(4326) %>%
  select(locality,
         kategori,
         no_spec)


#Get pollinators
get_pollinators <- function(){
  
  out <- dplyr::tibble(type_norwegian = c("Bier",
                                          "Bier",
                                          "Bier",
                                          "Bier",
                                          "Bier",
                                          "Bier",
                                          "Blomsterfluer",
                                          "Sommerfugler",
                                          "Sommerfugler",
                                          "Sommerfugler",
                                          "Sommerfugler",
                                          "Sommerfugler",
                                          "Sommerfugler"),
                       type_english = c("Bees",
                                        "Bees",
                                        "Bees",
                                        "Bees",
                                        "Bees",
                                        "Bees",
                                        "Hoverflies",
                                        "Butterflies",
                                        "Butterflies",
                                        "Butterflies",
                                        "Butterflies",
                                        "Butterflies",
                                        "Butterflies"),
                       family_norwegian = c("Gravebier",
                                            "Langtungbebier",
                                            "Korttungebier",
                                            "Markbier",
                                            "Buksamlerbier",
                                            "Blomsterbier",
                                            "Blomfluer",
                                            "Smygere",
                                            "Glansvinger",
                                            "Nymfevinger",
                                            "Svalestjerter",
                                            "Hvitvinger",
                                            "Metallmerker (uoffisiell)"),
                       family_latin = c("Andrenidae",
                                        "Apidae",
                                        "Colletidae",
                                        "Halictidae",
                                        "Megachilidae",
                                        "Melittidae",
                                        "Syrphidae",
                                        "Hesperiidae",
                                        "Lycaenidae",
                                        "Nymphalidae",
                                        "Papilionidae",
                                        "Pieridae",
                                        "Riodinidae")
  )
  
  return(out)
  
}

pollinators <- get_pollinators() 

poll_obs_q <- "
      SELECT 
      obs.id_family,
      obs.species_latin_fixed,
      yl.year,
      ls.sampling_name,
      l.locality,
      ST_Centroid(l.geom) as geom
      FROM occurrences.observations obs,
      events.identifications,
      events.year_locality yl,
      events.locality_sampling ls,
      locations.localities l,
      events.sampling_trap st
      WHERE obs.identification_id = identifications.id
      AND obs.id_family IN ('Andrenidae',
                             'Apidae',
                             'Colletidae',
                             'Halictidae',
                             'Megachilidae',
                             'Melittidae',
                             'Syrphidae',
                             'Hesperiidae',
                             'Lycaenidae',
                             'Nymphalidae',
                             'Papilionidae',
                             'Pieridae',
                             'Riodinidae')
      AND identifications.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
      AND yl.locality_id = l.id
      AND yl.project_short_name = 'NasIns'
      AND obs.identification_confidence = 'HIGH'
      "  

poll_obs <- read_sf(con,
                     query = poll_obs_q) %>% 
  left_join(pollinators,
            by = c("id_family" = "family_latin"))


poll_obs_agg <- poll_obs %>% 
  mutate(kategori = type_norwegian) %>% 
  group_by(kategori) %>% 
  mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>% 
  group_by(locality,
           kategori,
           no_spec_per_kat) %>% 
  summarise(no_spec = n_distinct(species_latin_fixed)) %>% 
  ungroup() %>% 
  #mutate(kategori_append = factor(paste0(kategori, " (", no_spec_per_kat, " stk.)"))) %>% 
  # left_join(kat_order,
  #           by = c("kategori" = "kategori")) %>% 
  mutate(kategori = factor(kategori, levels = c("Bier",
                                                "Blomsterfluer",
                                                "Sommerfugler"))
  )

poll_obs_agg <- poll_obs_agg %>% 
  st_jitter(poll_obs_agg, amount = 7000)  %>% 
  st_transform(4326) %>%
  select(locality,
         kategori,
         no_spec)

#Save the data

tryCatch(
  write_sf(obj = poll_obs_agg,
           dsn = "data/poll_obs_agg.shp",
           overwrite = TRUE)
)

tryCatch(
  write_sf(obj = redlisted_obs_2021_agg,
         dsn = "data/redlisted_obs_2021_agg.shp",
         overwrite = TRUE)
         )

tryCatch(
write_sf(obj = all_alien_obs_agg,
         dsn = "data/all_alien_obs_agg.shp",
         overwrite = TRUE)
)

# end prep data for div map


# prep data for dashboard

get_year_locality_stats <- function(){
 
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
    mutate(year = factor(year, levels = 2024:min(year))) %>% 
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

year_locality_stats <- get_year_locality_stats()

taxonomic_perc <- function(){
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
            .groups = "drop") %>% 
  arrange(desc(share))

family_level_data_mf <- loc_traptype_species_list %>% 
  filter(trap_type == "Malaise")  %>% 
  collect() %>% 
  group_by(id_order, 
           id_family,
           trap_type) %>% 
  summarise(share = n()/nrow(.),
            .groups = "drop") %>% 
  arrange(desc(share))

order_level_data_vf <- loc_traptype_species_list %>% 
  filter(trap_type == "Window") %>% 
  collect() %>% 
  group_by(id_order,
           trap_type) %>% 
  summarise(share = n()/nrow(.),
            .groups = "drop") %>% 
  arrange(desc(share))

family_level_data_vf <- loc_traptype_species_list %>% 
  filter(trap_type == "Window")  %>% 
  collect() %>% 
  group_by(id_order, 
           id_family,
           trap_type) %>% 
  summarise(share = n()/nrow(.),
            .groups = "drop") %>% 
  arrange(desc(share))

list("order_level_data_mf" = order_level_data_mf,
     "family_level_data_mf" = family_level_data_mf,
     "order_level_data_vf" = order_level_data_vf,
     "family_level_data_vf" = family_level_data_vf
)
}

taxonomic_perc_data <- taxonomic_perc()


catch_per_locality_sampling <- function(){

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

catch_per_locality_sampling_data <- catch_per_locality_sampling()

catch_per_year_locality_data <- catch_per_year_locality()

#Write dashboard data
readr::write_rds(year_locality_stats,
      file = "data/year_locality_stats.Rdata")

rlist::list.save(taxonomic_perc_data,
                 file = "data/taxonomic_perc_data.Rdata")

rlist::list.save(catch_per_locality_sampling_data,
      file = "data/catch_per_locality_sampling_data.Rdata")

rlist::list.save(catch_per_year_locality_data,
      file = "data/catch_per_year_locality_data.Rdata")

# end prep data for dashboard


# prep data for timeseries graphs

biomass_raw <- Norimon::get_biomass(agg_level = "locality_sampling",
                           trap_type = "MF",
                           subset_region = NULL
) 

biomass_mf_locality_sampling_time <- biomass_raw %>% 
  left_join(locality_sampling,
            by = c("sampling_name" = "sampling_name"),
            copy = T) %>% 
  mutate(julian_day = lubridate::yday(end_date),
         habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type),
         start_month = lubridate::month(start_date))  %>% 
  filter(sum_wet_weight > 0)


  # loc_species_data <- function() {
  #   
  #   loc_species_q <- paste0("
  #       
  #       SELECT year, foo.locality, l.region_name, foo.habitat_type, no_spec as tot_spec
  #       FROM
  #       (SELECT locality,
  #       year, 
  #       habitat_type, 
  #       count(distinct(loc_spec.species_latin_gbif))::numeric no_spec
  #       FROM views.loc_traptype_species_list loc_spec
  #       WHERE trap_type = 'Malaise'
  #       GROUP BY year, habitat_type, locality) foo,
  #       locations.localities l
  #       WHERE foo.locality = l.locality
  #       " )
  #   
  #   
  #   loc_species_res <- dbGetQuery(con,
  #                                 loc_species_q)  %>% 
  #     mutate(year = as.factor(year),
  #            habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type)) 
  #   
  #   return(loc_species_res)
  #   
  # }
  #loc_spec_data <- loc_species_data()

  diversity_raw <- Norimon::get_observations(agg_level = "locality_sampling",
                                    #trap_type = "MF",
                                    subset_region = NULL
  ) 
  
  diversity_locality_sampling_time <- diversity_raw %>% 
    left_join(locality_sampling,
              by = c("sampling_name" = "sampling_name"),
              copy = T) %>% 
    mutate(julian_day = lubridate::yday(end_date),
           habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type),
           start_month = lubridate::month(start_date))  %>% 
    filter(no_species > 0)
  

# Write time series data

save(biomass_mf_locality_sampling_time,
     file = "data/biomass_mf_locality_sampling_time.Rdata")

save(diversity_locality_sampling_time,
     file = "data/diversity_locality_sampling_time.Rdata")

# end prep data for timeseries graphs

#Update recalculate number
recalculate_number <- recalculate_status

save(recalculate_number,
     file = "data/recalculate_number.Rdata")
} else {
  
  return(NULL)
}
