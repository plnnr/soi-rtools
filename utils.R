
options(
  scipen = 999,
  digits = 4,
  tigris_class = "sf",
  tigris_use_cache = T
)

# Skip second line on import. 
county2msa <- read.csv(textConnection(readLines("data/resources/geocorr2014_county_to_msa.csv")[-2]), 
                       colClasses = c("character", "character", "character", "character", "integer", "integer"), 
                       header = TRUE, sep=",")

cnty <- readRDS("data/processed/cnty.rds")

msas <- readRDS("data/processed/msas.rds")

msa_shortname <- rio::import("data/resources/msa_shortname_brookings.xlsx") %>%
  rename(cbsa13 = `CBSA FIPS (2013)`, cbsa_shortname = `CBSA Short Name (2013)`) %>%
  filter(cbsa13 %in% msas$GEOID)

cpi <- rio::import("data/resources/CPI-U-West_BLS.xlsx", which = "annual") %>%
  select(year, inflation_factor_2020)

soi_migration_data <- readRDS("data/processed/soi_migration_data_2011t2018.rds")

## Will be useful later
totals_fips <- c("98000", "97003", "97001", "97000", "96000")

##### Functions ########
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


region_migration <- function(.x, cbsafips, direction = "inflow") {
  region_counties <- county2msa %>% 
    filter(cbsa == cbsafips) %>%
    pull(county)
  
  if (direction == "outflow") {
    .x %>%
      filter(y2_fips %in% region_counties | y1_fips %in% region_counties) %>%
      filter(direction == "outflow", 
             y1_fips != y2_fips) %>% # Remove non-movers
      mutate(in_region = y1_fips %in% region_counties) %>%
      left_join(., county2msa, by = c("y2_fips" = "county")) %>% # Join by where they moved to
      filter(cbsa != cbsafips) %>%
      mutate(cbsaname15 = ifelse(cbsaname15 == "99999", y2_fips, cbsaname15)) # If not in metro, provide destination state-county FIPS
  }
  else {
    .x %>%
      filter(y2_fips %in% region_counties | y1_fips %in% region_counties) %>%
      filter(direction == "inflow", 
             y1_fips != y2_fips) %>% # Remove non-movers
      mutate(in_region = y2_fips %in% region_counties) %>%
      left_join(., county2msa, by = c("y1_fips" = "county")) %>% # Join by where they moved from
      filter(cbsa != cbsafips) %>%
      mutate(cbsaname15 = ifelse(cbsaname15 == "99999", y1_fips, cbsaname15)) # If not in metro, provide origin state-county FIPS
  }
}

reduce_region_migration <- function(regional_flow) {
  ## Summarize migration by region and year
  regional_flow %>%
    group_by(y2, cbsa, cbsaname15) %>% # y1, y2, 
    summarize(n1 = sum(n1, na.rm = T),
              n2 = sum(n2, na.rm = T),
              agi_adj = sum(agi_adj, na.rm = T)) %>%
    arrange(desc(y2), desc(n2)) %>% 
    ungroup()
}

filter_for_major_metros <- function(.x, topn = 20){
  ## Filter for top metros (top 20) based on throughput or in/out-migration
  
  if("n2" %in% names(.x)) {
    ## This captures inflows or outflows separately, as opposed to a combined net migration input (as below)
    major_metros <- .x %>%
      group_by(cbsa, cbsaname15) %>%  
      summarize(n2 = sum(n2, na.rm = T)) %>%
      ungroup() %>%
      ## may have issues with encoding so use iconv https://stackoverflow.com/questions/13187605/error-in-tolower-invalid-multibyte-string/13189045
      filter(nchar(iconv(x = cbsaname15, "WINDOWS-1252","UTF-8")) > 5) %>% # Remove counties that aren't part of a CBSA
      top_n(n = topn, wt = n2)
  }
  
  else{
    major_metros <- .x %>%
      group_by(cbsa, cbsaname15) %>% # y1, y2, 
      summarize(n2.out = sum(n2.out, na.rm = T),
                n2.in = sum(n2.in, na.rm = T)) %>%
      ungroup() %>%
      mutate(n2.throughput = n2.in + n2.out) %>%
      ## may have issues with encoding so use iconv https://stackoverflow.com/questions/13187605/error-in-tolower-invalid-multibyte-string/13189045
      filter(nchar(iconv(x = cbsaname15, "WINDOWS-1252","UTF-8")) > 5) %>% # Remove counties that aren't part of a CBSA
      top_n(n = topn, wt = n2.throughput)
  }
  
  .x %>% filter(cbsa %in% major_metros$cbsa)
}

region_net_summarized_migration <- function(.x, cbsafips) {
  ## Generate net migration by year by region (CBSA)
  
  inflow <- region_migration(.x = .x, cbsafips = cbsafips, direction = "inflow") %>%
    reduce_region_migration() %>% 
    select(y2, cbsa, cbsaname15, n1.in = n1, n2.in = n2, agi_adj.in = agi_adj)
  
  outflow <- region_migration(.x = .x, cbsafips = cbsafips, direction = "outflow") %>%
    reduce_region_migration() %>% 
    select(y2, cbsa, cbsaname15, n1.out = n1, n2.out = n2, agi_adj.out = agi_adj)
  
  full_join(inflow, outflow, by = c("y2", "cbsa", "cbsaname15")) %>%
    mutate(n1.net = n1.in - n1.out,
           n1.throughput = n1.in + n1.out,
           n2.net = n2.in - n2.out,
           n2.throughput = n2.in + n2.out,
           agi_adj.net = agi_adj.in - agi_adj.out,
           agi_adj.throughput = agi_adj.in + agi_adj.out)
}

graph_var_by_year <- function(.x, var) {
  var <- sym(var)
  .x %>%
    left_join(., select(st_drop_geometry(msas), GEOID), by = c("cbsa" = "GEOID")) %>%
    left_join(., msa_shortname, by = c("cbsa" = "cbsa13")) %>%
    mutate(cbsa_shortname = fct_reorder(cbsa_shortname, -!!var)) %>%
    ggplot(aes(x = y2, y = !!var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~cbsa_shortname, scales = "free_y") +
    theme_minimal()
}

graph_flow_by_year <- function(.x, var) {
  var <- sym(var)
  .x %>%
    group_by(y2, cbsa, cbsaname15) %>% # y1, y2, 
    summarize(n1 = sum(n1, na.rm = T),
              n2 = sum(n2, na.rm = T),
              agi_adj = sum(agi_adj, na.rm = T)) %>%
    arrange(desc(y2), desc(n2)) %>%  ungroup() %>%
    left_join(., select(st_drop_geometry(msas), GEOID), by = c("cbsa" = "GEOID")) %>%
    left_join(., msa_shortname, by = c("cbsa" = "cbsa13")) %>%
    mutate(avg_hh_income = agi_adj / n1,
           cbsa_shortname = fct_reorder(cbsa_shortname, -!!var)) %>%
    ggplot(aes(x = y2, y = !!var, fill = avg_hh_income)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_c(trans = "log") +
    facet_wrap(~cbsa_shortname, scales = "free_y") +
    theme_minimal() +
    theme(legend.position="bottom") +
    guides(fill = guide_colourbar(barwidth = 16, barheight = .5))
}