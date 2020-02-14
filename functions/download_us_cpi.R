download_us_cpi <- function(file_name, file_dir = "raw_data/") {
  #' Download  US CPI
  #' 
  #' This function saves all the txt files in the specified directory
  #' 
  #' Downloads all the US CPI data from BLS
  #' \url{https://download.bls.gov/pub/time.series/cu/}
  #' and saves in the specified directory/name
  #' 
  #' @param file_name destination file (do \emph{not} include '.rds').
  #' @param file_dir  destination directory.
  #' @return an '.rds' file with the specified \emph{file_name}.
  #'     other supporting files in the 'bls_files' sub-directory  
  require(tidyverse)
  Current   <- 'http://download.bls.gov/pub/time.series/cu/cu.data.0.Current'
  AllItems  <- 'http://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems'
  Summaries <- 'http://download.bls.gov/pub/time.series/cu/cu.data.2.Summaries'
  AsizeNorthEast <- 'http://download.bls.gov/pub/time.series/cu/cu.data.3.AsizeNorthEast'
  AsizeNorthCentral <- 'http://download.bls.gov/pub/time.series/cu/cu.data.4.AsizeNorthCentral'
  AsizeSouth <- 'http://download.bls.gov/pub/time.series/cu/cu.data.5.AsizeSouth'
  AsizeWest <- 'http://download.bls.gov/pub/time.series/cu/cu.data.6.AsizeWest'
  OtherNorthEast <- 'http://download.bls.gov/pub/time.series/cu/cu.data.7.OtherNorthEast'
  OtherNorthCentral <- 'http://download.bls.gov/pub/time.series/cu/cu.data.8.OtherNorthCentral'
  OtherSouth <- 'http://download.bls.gov/pub/time.series/cu/cu.data.9.OtherSouth'
  OtherWest <- 'http://download.bls.gov/pub/time.series/cu/cu.data.10.OtherWest'
  USFoodBeverage <- 'http://download.bls.gov/pub/time.series/cu/cu.data.11.USFoodBeverage'
  USHousing <- 'http://download.bls.gov/pub/time.series/cu/cu.data.12.USHousing'
  USApparel <- 'http://download.bls.gov/pub/time.series/cu/cu.data.13.USApparel'
  USTransportation <- 'http://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation'
  USMedical <- 'http://download.bls.gov/pub/time.series/cu/cu.data.15.USMedical'
  USRecreation <- 'http://download.bls.gov/pub/time.series/cu/cu.data.16.USRecreation'
  USEducationAndCommunication <- 'http://download.bls.gov/pub/time.series/cu/cu.data.17.USEducationAndCommunication'
  USOtherGoodsAndServices <- 'http://download.bls.gov/pub/time.series/cu/cu.data.18.USOtherGoodsAndServices'
  PopulationSize <- 'http://download.bls.gov/pub/time.series/cu/cu.data.19.PopulationSize'
  USCommoditiesServicesSpecial <- 'http://download.bls.gov/pub/time.series/cu/cu.data.20.USCommoditiesServicesSpecial'
  
  
  db00 <- read_tsv(Current, col_types = 'cicn_')
  db01 <- read_tsv(AllItems, col_types = 'cicn_')
  db02 <- read_tsv(Summaries, col_types = 'cicn_')
  db03 <- read_tsv(AsizeNorthEast, col_types = 'cicn_')
  db04 <- read_tsv(AsizeNorthCentral, col_types = 'cicn_')
  db05 <- read_tsv(AsizeSouth, col_types = 'cicn_')
  db06 <- read_tsv(AsizeWest, col_types = 'cicn_')
  db07 <- read_tsv(OtherNorthEast, col_types = 'cicn_')
  db08 <- read_tsv(OtherNorthCentral, col_types = 'cicn_')
  db09 <- read_tsv(OtherSouth, col_types = 'cicn_')
  db10 <- read_tsv(OtherWest, col_types = 'cicn_')
  db11 <- read_tsv(USFoodBeverage, col_types = 'cicn_')
  db12 <- read_tsv(USHousing, col_types = 'cicn_')
  db13 <- read_tsv(USApparel, col_types = 'cicn_')
  db14 <- read_tsv(USTransportation, col_types = 'cicn_')
  db15 <- read_tsv(USMedical, col_types = 'cicn_')
  db16 <- read_tsv(USRecreation, col_types = 'cicn_')
  db17 <- read_tsv(USEducationAndCommunication, col_types = 'cicn_')
  db18 <- read_tsv(USOtherGoodsAndServices, col_types = 'cicn_')
  db19 <- read_tsv(PopulationSize, col_types = 'cicn_')
  db20 <- read_tsv(USCommoditiesServicesSpecial, col_types = 'cicn_')

  # Merge all data
  db <- bind_rows(db01, db02, db03, db04, db05, db06, db07, db08, db09, db10,
                  db11, db12, db13, db14, db15, db16, db17, db18, db19, db20)
  db <- distinct(db)
  write_rds(db, paste0(file_dir, file_name,".rds"))
  
  # download other files
  read_tsv("https://download.bls.gov/pub/time.series/cu/cu.area",
           col_types = 'ccili') %>%
    write_rds(paste0(file_dir, "bls_files/cu_area.rds"))
  
  read_tsv("https://download.bls.gov/pub/time.series/cu/cu.item",
           col_types = 'ccili') %>%
    write_rds(paste0(file_dir, "bls_files/cu_item.rds"))
  
  read_tsv("https://download.bls.gov/pub/time.series/cu/cu.series",
           col_types = 'cccccccccicic') %>%
    write_rds(paste0(file_dir, "bls_files/cu_series.rds"))
}
