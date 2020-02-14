read_cpi_weights <- function(file_name = "raw_data/bls_files/weights_c.xlsx",
                             series = NULL) {
  #'  Read OER and rent weights
  #'
  #'  This function reads the manually updated excel file with annual weights
  #'
  #'  Read annual weights and prepare a tibble with the resulting
  #'  series_id, weights
  #'
  #' @param file_name location of the excel file with annual weights
  #' @param series a tibble with the cu_series table from BLS
  #' @return a tibble with series_id, item_code, date, weight.
  require(tidyverse)
  weights.oer <- readxl::read_excel(file_name, sheet = "oer")
  weights.oer <- weights.oer %>%
    separate(item_code, c("item_code", "area_code")) %>%
    gather(date, weight, -item_code, -area_code) %>%
    mutate(date = lubridate::ymd(paste0(as.numeric(date) - 1, "1215")))
  
  weights.oer <- series %>%
    filter(
      item_code %in% unique(weights.oer$item_code),
      area_code %in% unique(weights.oer$area_code),
      seasonal == "U",
      periodicity_code == "R"
    ) %>%
    select(series_id, area_code, item_code) %>%
    right_join(weights.oer, by = c("area_code", "item_code")) %>%
    select(-area_code)
  
  # Read weights ex. OER and rent
  weights.ex <- readxl::read_excel(file_name, sheet = "weight_ex")
  
  weights.ex <- weights.ex %>%
    gather(date, weight, -item_code) %>%
    mutate(date = lubridate::ymd(paste0(as.numeric(date) - 1, "1215")))
  
  weights.ex <- series %>%
    filter(
      item_code %in% unique(weights.ex$item_code),
      seasonal == "U",
      area_code == "0000",
      periodicity_code == "R"
    ) %>%
    select(series_id, item_code) %>%
    right_join(weights.ex, by = c("item_code"))
  
  # Join both
  cpi.weights <- bind_rows(weights.ex, weights.oer) %>%
    arrange(series_id, date)
  return(cpi.weights)
}
