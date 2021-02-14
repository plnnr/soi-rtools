pacman::p_load(tidyverse, tigris, purrr, sf, mapview)

options(
  scipen = 999,
  digits = 4,
  tigris_class = "sf",
  tigris_use_cache = T
)

cnty <- counties(cb = T) %>% select(GEOID, NAME)
msas <- tigris::core_based_statistical_areas(cb = T, year = 2015)

# saveRDS(cnty, "data/processed/cnty.rds")
# saveRDS(msas, "data/processed/msas.rds")


### Functions to grab 2011-2018 data ####

generate_soi_links <- function(y1, y2, direction) {
  return(paste0("https://www.irs.gov/pub/irs-soi/county", direction, y1, y2, ".csv"))
}

read_soi_url <- function(url){
  read_csv(url) %>%
    mutate(direction = ifelse(grepl("inflow", url), "inflow", "outflow"), # Assign inflow/outflow indicator
           period = case_when(direction == "inflow" ~substr(url, 45, 48), # Extract the year from URL
                              direction == "outflow" ~ substr(url, 46, 49)),
           y1 = as.numeric(paste0("20", substr(period, 1,2))), # Year 1
           y2 = as.numeric(paste0("20", substr(period, 3,4)))) # Year 2
}

### Grab 2011-2018 data ####

inputs <- purrr::cross_df(.l = list(y1 = 11:17,
                                    y2 = 12:18,
                                    direction = c("inflow", "outflow"))) %>% filter(y1+1 == y2)

links <- pmap_chr(.l = as.list(inputs), .f = generate_soi_links)

soi_migration_data <- rbind(map_df(.x = links, .f = read_soi_url)) %>%
  left_join(., cpi, by = c("y1" = "year")) %>%
  mutate(n1 = n1 %>% na_if(., -1),
         n2 = n2 %>% na_if(., -1),
         agi = agi %>% na_if(., -1),
         agi_adj = agi * inflation_factor_2020,
         y2_fips = paste0(y2_statefips, y2_countyfips),
         y1_fips = paste0(y1_statefips, y1_countyfips)) %>%
  select(y1, y2, y1_fips, y2_fips, n1, n2, agi, agi_adj, direction, inflation_factor_2020)

# saveRDS(soi_migration_data, "data/processed/soi_migration_data_2011t2018.rds")
