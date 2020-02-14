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

all_items <- cpi.nsa %>% 
  filter(series_id == "CUUR0000SA0", date >= "1999-12-15") %>% 
  select(date, value) %>%
  mutate(dec_value = ifelse(lubridate::month(date) == 12, value, NA),
         dec_value = zoo::na.locf(dec_value, na.rm = FALSE),
         ytd = value / dec_value)

# Selects all cpi series with correspondend entry in 'weight' worksheet
cpi.selected.nsa <- cpi.nsa %>% 
  filter(series_id %in% unique(cpi.weights$series_id)) %>% 
  select(series_id, date, value) %>% 
  filter(date >= "1999-12-15") %>%
  left_join(cpi.weights, by = c("series_id", "date")) %>% 
  left_join(select(all_items,date, ytd), by = "date") %>% 
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

tmp <- full_join(
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


