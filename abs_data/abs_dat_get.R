source("abs_data/ABS_eco_data2.R")
source("abs_data/run_all_v2.R")
source("abs_data/get_local_abs.R")

abs_data <- eco_tables_abs()

stp_data_indus <- read.xlsx("abs_data/stp_industry_2021-01-20.xlsx",sheet = 2,startRow = 6,detectDates = TRUE) %>%
  as_tibble() %>% clean_stp_indus()

stp_national <- read.xlsx("abs_data/stp_national-2021-01-20.xlsx",sheet = 2,startRow = 6,detectDates = TRUE) %>%
  as_tibble() %>% clean_stp_national()

returned_data <- c(abs_data,list(stp_data_indus), list(stp_national))

# returned_data[[37]] <- indus_occ_gender_state

save(returned_data,file = "abs_data.RData")
# save(returned_data,file = "temp_dta.RData")


