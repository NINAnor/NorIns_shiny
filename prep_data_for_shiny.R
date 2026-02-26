# require(Norimon)
require(sf)
# library(tidyverse)
library(dplyr)
library(tidyr)
require(DBI)

#define new functions that handles the connection better.


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


load("data/shinyPass.Rdata")
prep_con <- pool::dbPool(RPostgres::Postgres(),
                          host = "t2lippgsql03.nina.no",
                          dbname = "insect_monitoring",
                          user = my_username,
                          password = my_password)

load("data/recalculate_number.Rdata")

shiny_rules <- tbl(
  prep_con,
  Id(
    schema = "lookup",
    table = "shiny_rules"
  )
)

recalculate_status <- shiny_rules %>%
  select(recalculate_number) %>%
  pull()

if (recalculate_number != recalculate_status) {
  locality_sampling <- dplyr::tbl(prep_con, dbplyr::in_schema("events", "locality_sampling"))

  # prep data for div_map

  redlisted_obs_2021 <- read_sf(
    prep_con,
    Id(
      schema = "views",
      table = "redlisted_obs"
    )
  ) %>%
    mutate(kategori = kategori_2021)

  redlisted_obs_2021 <- redlisted_obs_2021 %>%
    # mutate(expl_fact = gsub("([a-zA-Z]*)( >)(.*)", "\\1", expl_fact)) %>%
    separate(expl_fact,
      into = c(
        "expl_1",
        "expl_2",
        "expl_3",
        "expl_4",
        "expl_5"
      ),
      sep = ">"
    ) %>%
    separate(expl_3,
      into = "expl_3_main",
      sep = "_"
    ) %>%
    mutate(expl_3_main = ifelse(is.na(expl_3_main), expl_1, expl_3_main)) %>%
    mutate(expl_3_main = stringr::str_trim(expl_3_main)) %>%
    mutate(
      expl_3_main = ifelse(expl_3_main == " ", NA, expl_3_main),
      expl_3_main = ifelse(expl_3_main == "", NA, expl_3_main),
      expl_3_main = ifelse(expl_3_main == "Ukjent ", NA, expl_3_main),
      expl_3_main = ifelse(expl_3_main == "Ukjent", NA, expl_3_main)
    )

  redlisted_obs_2021 <- redlisted_obs_2021 %>%
    group_by(kategori) %>%
    mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>%
    ungroup() %>%
    filter(kategori != "DD")


  redlisted_obs_2021_agg <- redlisted_obs_2021 %>%
    group_by(
      locality,
      kategori
    ) %>%
    summarise(no_spec = n_distinct(species_latin_fixed)) %>%
    ungroup() %>%
    st_jitter(redlisted_obs_2021_agg, amount = 5000) %>%
    st_transform(4326)

  ## End get redlisted species

  ## Get pot alien species

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
      AND yl.project_short_name = 'NorIns'
      AND obs.identification_confidence = 'HIGH'
      "

  fennoskand_obs <- read_sf(prep_con,
    query = fennoskand_obs_q
  ) %>%
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
      AND yl.project_short_name = 'NorIns'
      AND obs.identification_confidence = 'HIGH'
      "

  pot_alien_obs <- read_sf(prep_con,
    query = pot_alien_obs_q
  ) %>%
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
      AND yl.project_short_name = 'NorIns'
      AND alien.\"riskCategory\" IN ('NK', 'SE', 'HI', 'PH', 'LO')
      AND obs.identification_confidence = 'HIGH'
      "

  alien_obs <- read_sf(prep_con,
    query = alien_obs_q
  ) %>%
    mutate(kategori = "På fremmedartslista")


  all_alien_obs <- fennoskand_obs %>%
    rbind(pot_alien_obs) %>%
    rbind(alien_obs)


  # kat_order <- tibble(kategori = c("Fennoskandisk forek.",
  #                                  "Potensielt fremmede arter",
  #                                  "På fremmedartslista"),
  #                     kat_order = 1:3)

  all_alien_obs_agg <- all_alien_obs %>%
    group_by(kategori) %>%
    mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>%
    group_by(
      locality,
      kategori,
      no_spec_per_kat
    ) %>%
    summarise(no_spec = n_distinct(species_latin_fixed)) %>%
    ungroup() %>%
    mutate(kategori_append = factor(paste0(kategori, " (", no_spec_per_kat, " stk.)"))) %>%
    # left_join(kat_order,
    #           by = c("kategori" = "kategori")) %>%
    mutate(kategori = factor(kategori, levels = c(
      "Fennoskandisk forek.",
      "Potensielt fremmede arter",
      "På fremmedartslista"
    )))

  all_alien_obs_agg <- all_alien_obs_agg %>%
    st_jitter(all_alien_obs_agg, amount = 5000) %>%
    st_transform(4326) %>%
    select(
      locality,
      kategori,
      no_spec
    )


  # Get pollinators
  get_pollinators <- function() {
    out <- dplyr::tibble(
      type_norwegian = c(
        "Bier",
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
        "Sommerfugler"
      ),
      type_english = c(
        "Bees",
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
        "Butterflies"
      ),
      family_norwegian = c(
        "Gravebier",
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
        "Metallmerker (uoffisiell)"
      ),
      family_latin = c(
        "Andrenidae",
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
        "Riodinidae"
      )
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
      AND yl.project_short_name = 'NorIns'
      AND obs.identification_confidence = 'HIGH'
      "

  poll_obs <- read_sf(prep_con,
    query = poll_obs_q
  ) %>%
    left_join(pollinators,
      by = c("id_family" = "family_latin")
    )


  poll_obs_agg <- poll_obs %>%
    mutate(kategori = type_norwegian) %>%
    group_by(kategori) %>%
    mutate(no_spec_per_kat = n_distinct(species_latin_fixed)) %>%
    group_by(
      locality,
      kategori,
      no_spec_per_kat
    ) %>%
    summarise(no_spec = n_distinct(species_latin_fixed)) %>%
    ungroup() %>%
    # mutate(kategori_append = factor(paste0(kategori, " (", no_spec_per_kat, " stk.)"))) %>%
    # left_join(kat_order,
    #           by = c("kategori" = "kategori")) %>%
    mutate(kategori = factor(kategori, levels = c(
      "Bier",
      "Blomsterfluer",
      "Sommerfugler"
    )))

  poll_obs_agg <- poll_obs_agg %>%
    st_jitter(poll_obs_agg, amount = 5000) %>%
    st_transform(4326) %>%
    select(
      locality,
      kategori,
      no_spec
    )


  tot_richn <- tbl(
    prep_con,
    Id(
      schema = "views",
      table = "no_spec_year_locality"
    )
  ) %>%
    collect()

  project_year_localities <- sf::read_sf(
    prep_con,
    Id(
      schema = "views",
      table = "project_year_localities"
    )
  )

  tot_richn_loc <- project_year_localities %>%
    left_join(tot_richn,
      by = c(
        "locality" = "locality",
        "region_name" = "region_name",
        "habitat_type" = "habitat_type",
        "year" = "year",
        "project_short_name" = "project_short_name"
      )
    )

  NorIns_richn_loc <- tot_richn_loc %>%
    mutate(
      geometry = center_geom,
      no_spec = as.integer(tot_no_spec)
    ) %>%
    sf::st_set_geometry("geometry") %>%
    st_jitter(poll_obs_agg, amount = 5000) %>%
    st_transform(4326) %>%
    filter(project_short_name == "NorIns") %>%
    select(locality,
      kategori = habitat_type,
      no_spec
    )


  # Save the data

  tryCatch(
    write_sf(
      obj = poll_obs_agg,
      dsn = "data/poll_obs_agg.shp",
      overwrite = TRUE
    )
  )

  tryCatch(
    write_sf(
      obj = redlisted_obs_2021_agg,
      dsn = "data/redlisted_obs_2021_agg.shp",
      overwrite = TRUE
    )
  )

  tryCatch(
    write_sf(
      obj = all_alien_obs_agg,
      dsn = "data/all_alien_obs_agg.shp",
      overwrite = TRUE
    )
  )

  tryCatch(
    write_sf(
      obj = NorIns_richn_loc,
      dsn = "data/NorIns_richn_loc.shp",
      overwrite = TRUE
    )
  )

  # end prep data for div map


  # prep data for dashboard

  get_year_locality_stats <- function(last_year = 2024) {
    project_year_localities <- tbl(
      prep_con,
      Id(
        schema = "views",
        table = "project_year_localities"
      )
    ) %>%
      mutate(habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type))

    proj_sum <- project_year_localities %>%
      filter(project_short_name == "NorIns") %>%
      collect() %>%
      mutate(region_name = factor(region_name,
        levels = c(
          "Sørlandet",
          "Østlandet",
          "Vestlandet",
          "Trøndelag",
          "Nord-Norge"
        )
      )) %>%
      mutate(habitat_type = factor(habitat_type)) %>%
      mutate(year = factor(year, levels = last_year:min(year))) %>%
      group_by(region_name,
        habitat_type,
        year,
        .drop = FALSE
      ) %>%
      summarise(
        visits = as.integer(n()),
        .groups = "drop"
      ) %>%
      mutate(habitat_type = as.character(habitat_type)) %>%
      arrange(
        region_name,
        year,
        habitat_type
      )


    return(proj_sum)
  }

  year_locality_stats <- get_year_locality_stats(last_year = 2025)

  taxonomic_perc <- function() {
    loc_traptype_species_list <- tbl(
      prep_con,
      Id(
        schema = "views",
        table = "loc_traptype_species_list"
      )
    ) %>%
      filter(id_class == "Insecta")

    order_level_data_mf <- loc_traptype_species_list %>%
      filter(trap_type == "Malaise") %>%
      collect() %>%
      group_by(
        id_order,
        trap_type
      ) %>%
      summarise(
        share = n() / nrow(.),
        .groups = "drop"
      ) %>%
      arrange(desc(share))

    family_level_data_mf <- loc_traptype_species_list %>%
      filter(trap_type == "Malaise") %>%
      collect() %>%
      group_by(
        id_order,
        id_family,
        trap_type
      ) %>%
      summarise(
        share = n() / nrow(.),
        .groups = "drop"
      ) %>%
      arrange(desc(share))

    order_level_data_vf <- loc_traptype_species_list %>%
      filter(trap_type == "Window") %>%
      collect() %>%
      group_by(
        id_order,
        trap_type
      ) %>%
      summarise(
        share = n() / nrow(.),
        .groups = "drop"
      ) %>%
      arrange(desc(share))

    family_level_data_vf <- loc_traptype_species_list %>%
      filter(trap_type == "Window") %>%
      collect() %>%
      group_by(
        id_order,
        id_family,
        trap_type
      ) %>%
      summarise(
        share = n() / nrow(.),
        .groups = "drop"
      ) %>%
      arrange(desc(share))

    list(
      "order_level_data_mf" = order_level_data_mf,
      "family_level_data_mf" = family_level_data_mf,
      "order_level_data_vf" = order_level_data_vf,
      "family_level_data_vf" = family_level_data_vf
    )
  }

  taxonomic_perc_data <- taxonomic_perc()


  catch_per_locality_sampling <- function() {
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
    	AND yl.project_short_name = 'NorIns'
    	AND ls.end_date IS NOT NULL
    	AND ls.start_date IS NOT NULL
    	AND st.wet_weight IS NOT NULL
    	GROUP BY ls.id, yl.year, l.region_name, l.habitat_type, l.locality, tt.trap_type
      "

    biomass_per_ls <- dbGetQuery(
      prep_con,
      biomass_per_ls_q
    ) %>%
      filter(value > 0)


    tot_spec_per_ls_q <- "
      SELECT *
      FROM views.no_spec_locality_sampling
      "

    tot_spec_per_ls <- dbGetQuery(
      prep_con,
      tot_spec_per_ls_q
    ) %>%
      arrange(desc(tot_no_spec)) %>%
      mutate(
        row_number = as.integer(row_number()),
        value = as.integer(tot_no_spec)
      ) %>%
      select(-tot_no_spec)

    out <- list(
      "biomass" = biomass_per_ls,
      "species" = tot_spec_per_ls
    )

    return(out)
  }


  catch_per_year_locality <- function() {
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
    	AND yl.project_short_name = 'NorIns'
    	AND ls.end_date IS NOT NULL
    	AND ls.start_date IS NOT NULL
    	AND st.wet_weight IS NOT NULL
    	GROUP BY yl.id, yl.year, l.region_name, l.habitat_type, l.locality, tt.trap_type

      "

    biomass_per_yl <- dbGetQuery(
      prep_con,
      biomass_per_yl_q
    )



    tot_spec_per_yl_q <- "
      SELECT *
      FROM views.no_spec_year_locality
      "

    tot_spec_per_yl <- dbGetQuery(
      prep_con,
      tot_spec_per_yl_q
    ) %>%
      arrange(desc(tot_no_spec)) %>%
      mutate(
        row_number = as.integer(row_number()),
        value = as.integer(tot_no_spec)
      ) %>%
      select(-tot_no_spec)


    out <- list(
      "biomass" = biomass_per_yl,
      "species" = tot_spec_per_yl
    )

    return(out)
  }

  catch_per_locality_sampling_data <- catch_per_locality_sampling()

  catch_per_year_locality_data <- catch_per_year_locality()

  # Write dashboard data
  readr::write_rds(year_locality_stats,
    file = "data/year_locality_stats.Rdata"
  )

  rlist::list.save(taxonomic_perc_data,
    file = "data/taxonomic_perc_data.Rdata"
  )

  rlist::list.save(catch_per_locality_sampling_data,
    file = "data/catch_per_locality_sampling_data.Rdata"
  )

  rlist::list.save(catch_per_year_locality_data,
    file = "data/catch_per_year_locality_data.Rdata"
  )

  # end prep data for dashboard


  # prep data for timeseries graphs

  biomass_raw <- Norimon::get_biomass(
    agg_level = "locality_sampling",
    trap_type = "MF",
    subset_region = NULL
  )

  biomass_raw_TidVar <- Norimon::get_biomass(
    agg_level = "locality_sampling",
    trap_type = "MF",
    subset_region = NULL,
    dataset = "TidVar"
  )
  biomass_raw <- biomass_raw %>%
    rbind(biomass_raw_TidVar)

  biomass_mf_locality_sampling_time <- biomass_raw %>%
    left_join(locality_sampling,
      by = c("sampling_name" = "sampling_name"),
      copy = T
    ) %>%
    mutate(
      julian_day = lubridate::yday(end_date),
      habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type),
      start_month = lubridate::month(start_date)
    ) %>%
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
  # loc_spec_data <- loc_species_data()

  diversity_raw <- Norimon::get_observations(
    agg_level = "locality_sampling",
    trap_type = "MF",
    subset_region = NULL,
    dataset = "NorIns"
  )
  
  diversity_raw_TidVar <- Norimon::get_observations(
    agg_level = "locality_sampling",
    trap_type = "MF",
    subset_region = NULL,
    dataset = "TidVar"
  )

  diversity_raw <- diversity_raw %>%
    rbind(diversity_raw_TidVar)

  diversity_locality_sampling_time <- diversity_raw %>%
    left_join(locality_sampling,
      by = c("sampling_name" = "sampling_name"),
      copy = T
    ) %>%
    mutate(
      julian_day = lubridate::yday(end_date),
      habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type),
      start_month = lubridate::month(start_date)
    ) %>%
    filter(no_species > 0)


  # diversity_raw_high <- get_observations(agg_level = "locality_sampling",
  #                                        trap_type = "MF",
  #                                        subset_region = NULL,
  #                                        id_conf = "HIGH"
  # )
  #
  # diversity_raw_high_TidVar <- get_observations(agg_level = "locality_sampling",
  #                                              trap_type = "MF",
  #                                              subset_region = NULL,
  #                                              id_conf = "HIGH",
  #                                              dataset = "TidVar"
  # )
  #
  # diversity_raw_high <- diversity_raw_high %>%
  #   rbind(diversity_raw_high_TidVar)
  #
  #
  # diversity_locality_sampling_time <- diversity_raw %>%
  #   left_join(locality_sampling,
  #             by = c("sampling_name" = "sampling_name"),
  #             copy = T) %>%
  #   mutate(julian_day = lubridate::yday(end_date),
  #          habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type),
  #          start_month = lubridate::month(start_date))  %>%
  #   filter(no_species > 0)
  #
  # diversity_locality_sampling_time_id_high <- diversity_raw_high %>%
  #   left_join(locality_sampling,
  #             by = c("sampling_name" = "sampling_name"),
  #             copy = T) %>%
  #   mutate(julian_day = lubridate::yday(end_date),
  #          habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type),
  #          start_month = lubridate::month(start_date))  %>%
  #   filter(no_species > 0)



  # Write time series data

  save(biomass_mf_locality_sampling_time,
    file = "data/biomass_mf_locality_sampling_time.Rdata"
  )

  save(diversity_locality_sampling_time,
    file = "data/diversity_locality_sampling_time.Rdata"
  )

  # save(diversity_locality_sampling_time_id_high,
  #     file = "data/diversity_locality_sampling_time_id_high.Rdata")

  # end prep data for timeseries graphs

  # Update recalculate number
  recalculate_number <- recalculate_status

  save(recalculate_number,
    file = "data/recalculate_number.Rdata"
  )
  
  
} 

#pool::poolClose(prep_con)
