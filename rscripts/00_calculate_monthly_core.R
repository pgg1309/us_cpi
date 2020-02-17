library(tidyverse)

# Functions ---------------------------------------------------------------
source('functions/download_us_cpi.R')
source('functions/read_cpi_weights.R')

select_cpi <- function(db, series, seas) {
  ids <- series %>%
    filter(seasonal == seas,
           periodicity_code == "R",    # R = Monthly S = Semi-Annual
           base_code == "S",           # S = Current A = Alternate
           area_code == "0000") %>%
    pull(series_id)
  db %>% 
    filter(series_id %in% ids,
           period != "M13") %>%        # Remove M13 = Annual Average
    mutate(date = lubridate::ymd(paste0(year, period, 15))) %>%
    arrange(series_id, date) %>%
    return()
}

seas_adjust <- function(x) {
  #' Seasonally adjust monthly CPI series
  #' @param x is a tibble with columns "date" and "value"
  #' @return returns an object of class "stl"
  sdate <-
    c(lubridate::year(min(x$date)), lubridate::month(min(x$date)))
  x.ts <- ts(log(x$value), start = sdate, frequency = 12)
  y <-
    stl(
      x.ts,
      s.window = 7,
      s.degree = 0,
      t.window = 37,
      robust = TRUE
    )
  y$time.series <- exp(y$time.series)
  e <- forecast::remainder(y)
  s <- forecast::seasonal(y)
  f <- forecast::trendcycle(y)
  seas.strength  <- max(0, min(1, 1 - var(e) / var(e * s)))
  trend.strength <- max(0, min(1, 1 - var(e) / var(f * e)))
  y$seas.strength  <- seas.strength
  y$trend.strength <- trend.strength
  return(y)
}

ts2df <- function(x){
  #' Convert mts to data.frame
  dt <- zoo::as.Date.yearmon(as.numeric(time(x))) + 14
  data.frame(date = dt, as.matrix(x))
}


# UPDATE BLS data ---------------------------------------------------------
download_us_cpi("us_cpi_db", "raw_data/")
db <- read_rds("raw_data/us_cpi_db.rds")
series <- read_rds("raw_data/bls_files/cu_series.rds")

INITIAL_DATE <- "1999-12-15"

# Select NSA series
cpi.nsa <- select_cpi(db, series, "U")

# Read regional OER
regions <- c("0100", "0200", "0300", "0400")
codes <- c("SEHC", "SEHA")
oer.id <- series %>%
  filter(area_code %in% regions,
         item_code %in% codes,
         seasonal == "U",
         periodicity_code == "R",
         base_code == "S") %>%
  pull(series_id)

cpi.oer <- db %>% 
  filter(series_id %in% oer.id, period != "M13") %>%
  mutate(date = lubridate::ymd(paste0(year, period, 15))) %>%
  arrange(series_id, date)

# Get weights
cpi.weights <- read_cpi_weights("raw_data/bls_files/weights_c.xlsx", series)

# Update weights
cpi.nsa <- bind_rows(cpi.nsa, cpi.oer) # Adds regional OER and rents to NSA data
rm(cpi.oer, oer.id, codes, regions, db)


# Get full CPI Index
all.items <- cpi.nsa %>% 
  filter(series_id == "CUUR0000SA0", date >= INITIAL_DATE) %>% 
  select(date, value) %>%
  mutate(dec_value = ifelse(lubridate::month(date) == 12, value, NA),
         dec_value = zoo::na.locf(dec_value, na.rm = FALSE),
         ytd = value / dec_value)

# Selects all cpi series with correspondend entry in 'weight' worksheet
cpi.selected.nsa <- cpi.nsa %>% 
  filter(series_id %in% unique(cpi.weights$series_id)) %>% 
  select(series_id, date, value) %>% 
  filter(date >= INITIAL_DATE) %>%
  arrange(series_id, date)
#
# WARNING !
# Some time series downloaded from BLS contain GAPs!
# The procedure below fill the gaps.

# miss <- c("CUUR0000SEMG",
#           "CUUR0000SEMF02",
#           "CUUR0000SEED04",
#           "CUUR0000SEME",
#           "CUUR0000SEMD03",
#           "CUUR0000SETA03",
#           "CUUR0000SEHL02",
#           "CUUR0000SERG02")

# The only one with gap is CUUR0000SEHL02
# procedure below interpolates missing data in the GAP
cpi.selected.nsa <- full_join(all.items,
  filter(cpi.selected.nsa, series_id == "CUUR0000SEHL02"),
  by = 'date') %>%
  mutate(y = zoo::na.approx(value.y, na.rm = FALSE)) %>%
  transmute(series_id = "CUUR0000SEHL02", date = date, value = y) %>%
  bind_rows(filter(cpi.selected.nsa, !series_id == "CUUR0000SEHL02")) %>%
  arrange(series_id, date)

# cpi.selected.nsa %>%
#   filter(series_id %in% miss) %>%
#   pivot_wider(names_from = 'series_id', values_from = 'value') %>% 
#   arrange(date) %>% 
#   naniar::vis_miss()
# 

cpi.selected.nsa <- cpi.selected.nsa %>%
  left_join(cpi.weights, by = c("series_id", "date")) %>% 
  left_join(select(all.items,date, ytd), by = "date") %>% 
  arrange(series_id, date) %>%               # Make sure sample is ordered
  mutate(dec_weight = zoo::na.locf(weight, na.rm = FALSE),
         dec_value  = ifelse(is.na(item_code), NA, value),
         dec_value  = zoo::na.locf(dec_value, na.rm = FALSE),
         weight = dec_weight * (value / dec_value) / ytd) %>%
  select(series_id, date, value, weight)

wtotal <- cpi.selected.nsa %>%
  group_by(date) %>%
  summarise(total = sum(weight)) 

cpi.selected.nsa <- left_join(cpi.selected.nsa, wtotal, by = "date") %>% 
  mutate(weight = weight * 100 / total) %>% 
  select(-total)
rm(wtotal)


# Adds seasonally adjusted and trend to database
cpi.selected <- full_join(
  cpi.selected.nsa,
  cpi.selected.nsa %>%
    nest(data = -series_id) %>%
    mutate(
      model.sa = map(data, seas_adjust),
      ts.sa = map(model.sa, "time.series"),
      df.sa = map(ts.sa, ts2df)
    ) %>%
    select(series_id, df.sa) %>%
    unnest(df.sa),
  by = c('series_id', 'date')
) %>% 
  mutate(value.sa = trend * remainder)

# Select names for each series_id
item <-  read_rds("raw_data/bls_files/cu_item.rds")

cpi.names <- series %>%
  filter(series_id %in% unique(cpi.selected$series_id)) %>%
  select(series_id, item_code) %>%
  left_join(item, by = "item_code") %>%
  select(series_id, item_name)

# Add names to cpi data
cpi.selected <- left_join(cpi.selected, cpi.names, by = "series_id")

# add mom and remove NAs
cpi.selected <- cpi.selected %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(mom    = 100*log(value    / lag(value,    1)),
         mom.sa = 100*log(value.sa / lag(value.sa, 1)),
         weight = lag(weight, 1)) %>% 
  ungroup() %>%
  na.omit() 

last.date <- format(max(cpi.selected$date), "%Y%m") # last date BEFORE forecasts
write_rds(cpi.selected, paste0("data/",last.date,"_uscpi_selected.rds"), compress = "gz")
write_rds(cpi.selected, "data/uscpi_selected.rds", compress = "gz")


# Calculate median CPI
median.cpi <- cpi.selected %>%
  arrange(date,mom.sa) %>%     # Using Seasonally Adjusted data !
  group_by(date) %>%
  mutate(cum.weight = cumsum(weight)) %>%
  ungroup() %>% 
  filter(cum.weight >= 50) %>% 
  group_by(date) %>%
  top_n(-1,cum.weight) %>% 
  ungroup() %>%
  select(date, item_name, mom.sa) %>%
  mutate(m.ar = 12 * mom.sa,
         q.ar = 12 * zoo::rollmeanr(mom.sa,  3, fill = NA),
         s.ar = 12 * zoo::rollmeanr(mom.sa,  6, fill = NA),
         q3.ar= 12 * zoo::rollmeanr(mom.sa,  9, fill = NA),
         y.ar = 12 * zoo::rollmeanr(mom.sa, 12, fill = NA)) 

last.date <- format(max(median.cpi$date), "%Y%m") # last date BEFORE forecasts
write_rds(median.cpi, paste0("data/",last.date,"_uscpi_median.rds"), compress = "gz")
write_rds(median.cpi, "data/uscpi_median.rds", compress = "gz")

# calculate TRIMMED 16
trim16.cpi <- cpi.selected %>%
  arrange(date,mom.sa) %>%
  group_by(date) %>%
  mutate(cum.weight = cumsum(weight)) %>%
  ungroup() %>% 
  filter(cum.weight >= 16, cum.weight <= 100 - 16) %>% 
  mutate(contrib = mom.sa * weight) %>% 
  group_by(date) %>%
  summarize(t.weight = sum(weight), mom.sa = sum(contrib) / t.weight) %>%
  select(-t.weight) %>%
  mutate(m.ar = 12 * mom.sa,
         q.ar = 12 * zoo::rollmeanr(mom.sa,  3, fill = NA),
         s.ar = 12 * zoo::rollmeanr(mom.sa,  6, fill = NA),
         q3.ar= 12 * zoo::rollmeanr(mom.sa,  9, fill = NA),
         y.ar = 12 * zoo::rollmeanr(mom.sa, 12, fill = NA))

last.date <- format(max(trim16.cpi$date), "%Y%m") # last date BEFORE forecasts
write_rds(trim16.cpi, paste0("data/",last.date,"_uscpi_trimmed16.rds"), compress = "gz")
write_rds(trim16.cpi, "data/uscpi_trimmed16.rds", compress = "gz")

# calculate TRIMMED 30
trim30.cpi <- cpi.selected %>%
  arrange(date, mom.sa) %>%
  group_by(date) %>%
  mutate(cum.weight = cumsum(weight)) %>%
  ungroup() %>% 
  filter(cum.weight >= 30, cum.weight <= 100 - 30) %>% 
  mutate(contrib = mom.sa * weight) %>% 
  group_by(date) %>%
  summarize(t.weight = sum(weight), mom.sa = sum(contrib) / t.weight) %>%
  select(-t.weight) %>%
  mutate(m.ar = 12 * mom.sa,
         q.ar = 12 * zoo::rollmeanr(mom.sa,  3, fill = NA),
         s.ar = 12 * zoo::rollmeanr(mom.sa,  6, fill = NA),
         q3.ar= 12 * zoo::rollmeanr(mom.sa,  9, fill = NA),
         y.ar = 12 * zoo::rollmeanr(mom.sa, 12, fill = NA))

last.date <- format(max(trim30.cpi$date), "%Y%m") # last date BEFORE forecasts
write_rds(trim30.cpi, paste0("data/",last.date,"_uscpi_trimmed30.rds"), compress = "gz")
write_rds(trim30.cpi, "data/uscpi_trimmed30.rds", compress = "gz")

rm(
  all.items,
  cpi.names,
  cpi.nsa,
  cpi.selected,
  cpi.selected.nsa,
  cpi.weights,
  item,
  median.cpi,
  series,
  trim16.cpi,
  trim30.cpi,
  INITIAL_DATE,
  last.date,
  download_us_cpi,
  read_cpi_weights,
  seas_adjust,
  select_cpi,
  ts2df
)
