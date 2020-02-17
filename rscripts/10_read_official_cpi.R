library(tidyverse)
db <- read_rds("raw_data/us_cpi_db.rds")

official <-
  c(
    "CUSR0000SA0",
    "CUSR0000SA0L1E",
    "CUSR0000SA0L12E",
    "CUUR0000SA0",
    "CUUR0000SA0L1E",
    "CUUR0000SA0L12E"
  )

db <- db %>%
  filter(series_id %in% official, period != "M13") %>%
  mutate(date = lubridate::ymd(paste0(year, period, 15))) %>%
  arrange(series_id, date) %>%
  mutate(
    series = case_when(
      series_id == "CUSR0000SA0"     ~ "all.sa",
      series_id == "CUSR0000SA0L1E"  ~ "core.sa",
      series_id == "CUSR0000SA0L12E" ~ "corex.sa",
      series_id == "CUUR0000SA0"     ~ "all",
      series_id == "CUUR0000SA0L1E"  ~ "core",
      series_id == "CUUR0000SA0L12E" ~ "corex"
    )
  ) %>%
  select(date, series, value) 

last.date <- format(max(db$date), "%Y%m")
write_rds(db, paste0("data/",last.date,"_uscpi_official.rds"), compress = "gz")
write_rds(db, "data/uscpi_official.rds", compress = "gz")

rm(db, last.date, official)
