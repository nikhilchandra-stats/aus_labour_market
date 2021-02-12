debugSource("C:/Users/nchandra/Documents/dev_nik/scripts/trading_eco/clean_trading_eco.R")
debugSource("C:/Users/nchandra/Documents/dev_nik/scripts/under_development/ABS/ABS_eco_data2.R")
debugSource("C:/Users/nchandra/Documents/dev_nik/employment_adhoc/stp_data_clean.R")
debugSource("C:/Users/nchandra/Documents/dev_nik/employment_adhoc/read_jobkeeper_local.R")
debugSource("C:/Users/nchandra/Documents/dev_nik/scripts/brief_v2/run_all_v2.R")

abs_data <- eco_tables_abs()

#------------------------------------LLFS
labour_force <- abs_data[[2]][[2]] %>%
  filter(series_type == "Original") %>%
  group_by(date,state,employment_status) %>%
  summarise(value = sum(value) ) %>%
  filter(employment_status %in% c("Unemployed total","Employed total") ) %>%
  pivot_wider(names_from = employment_status, values_from = value) %>%
  mutate(rate = `Unemployed total`/(`Employed total` + `Unemployed total` ) ) %>%
  group_by(state) %>%
  mutate(diff = rate - lag(rate) ) %>%
  mutate(diff_total = `Unemployed total` - lag(`Unemployed total`) ) 

#-------------------------------GDP by industry
gdp_indus <- abs_data[[12]][[2]] %>%
  mutate(sector = trimws(sector)) %>%
  mutate(sector = stringr::str_remove(sector, "\\(") ) %>%
  mutate(sector = stringr::str_remove(sector, "\\)") ) %>%
  mutate(sector = stringr::str_remove(sector, " [:upper:]") ) %>%
  mutate(sector = trimws(sector)) %>%
  rename(indus2 = industry, 
         industry = sector) %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(unit == "$ Millions") %>%
  group_by(industry,date) %>%
  summarise(
    gdp_value = sum(value, na.rm = TRUE)
  ) %>%
  mutate(industry = stringr::str_replace(industry,pattern = "and",replacement = "&")
         ) %>%
  group_by(industry) %>%
  mutate(
    gdp_change = gdp_value - lag(gdp_value,order_by = date)
  ) %>%
  mutate(
    gdp_change_yr_yr = (gdp_value - lag(gdp_value,12,order_by = date) )/lag(gdp_value,12,order_by = date)
  )
  
gdp_indus_max_date <- gdp_indus %>%
  filter(date == max(date)) %>%
  mutate(gdp_unit = "millions")
#------------------------------------Job Adds
job_ads <- get_ads_lmip_regions() %>%
  group_by(date) %>%
  summarise(ads = sum(ads)) %>%
  mutate(diff = ads - lag(ads)) %>%
  mutate(Years = year(date))  %>%
  mutate(months = as.character(month(date) ) ) 

job_ads2 <- get_ads_burning_glass()

#------------------------------------------Job seeker

jobseeker <- covidpmc::get_jobseeker_postcode() %>%
  rename(poa_code = postcode)

concord_table <- get_db_geos(db_con = db_con_r,all_cols = TRUE) %>%
  dplyr::select(poa_code, state_name,) %>% filter(!duplicated(poa_code) )

jobseeker <- left_join(jobseeker, concord_table, by = "poa_code") %>%
  group_by(state_name,date) %>%
  summarise(jobseeker = sum(jobseeker,na.rm = TRUE))

#----------------------------

#------------------------------STP DATA SA4
stp_data_sa3 <- read.xlsx("abs data/stp_12_15_2020.xlsx",sheet = 3,startRow = 6,detectDates = TRUE) %>%
  as_tibble() %>% 
  clean_stp_sa4()


stp_data_indus <- read.xlsx("abs data/stp_industry_12_15_2020.xlsx",sheet = 2,startRow = 6,detectDates = TRUE) %>%
  as_tibble() %>% clean_stp_indus() %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "[:upper:]-") ) %>%
  mutate(industry = trimws(industry))

#----------------------------
industries <- stp_data_indus %>% distinct(industry)
interested_industries <- c(
  "H-Accommodation & food services",
  "R-Arts & recreation services",
  "I-Transport, postal & warehousing",
  "M-Professional, scientific & technical service",
  "N-Administrative & support services",
  "G-Retail trade"
)
  
stp_data_indus %>%
  filter(industry %in% interested_industries) %>%
  filter(!is.na(industry)) %>%
  group_by(industry,date) %>%
  summarise(value = mean(value,na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = industry, fill = value) ) + 
  theme_minimal() +
  geom_tile() + 
  scale_fill_gradient(low="white", high="dark green")

stp_data_indus2 <- stp_data_indus %>%
  filter(!is.na(industry)) %>%
  group_by(industry,date) %>% 
  summarise(value = mean(value,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(industry) %>%
  mutate(
    value_at_feb_jan = 
      case_when(
        
        month(date) == 1 ~ value,
        month(date) == 2 ~ value
        
   
      )
  ) %>%
  mutate(
    average_pre_covid = mean(value_at_feb_jan,na.rm = TRUE)
  ) %>% ungroup() %>%
  mutate(
    difference_since_covid = value - average_pre_covid
  ) 
  

stp_data_gdp <- stp_data_indus2 %>%
  filter(date == max(date)) %>%
  left_join(gdp_indus_max_date, by = "industry") %>%
  mutate(industry = str_wrap(industry,15) )
# %>%
#   fill(gdp_value, .direction = "down") %>%
#   fill(gdp_value, .direction = "up")

stp_data_gdp %>% 
  ggplot(aes(x = gdp_change_yr_yr, y = difference_since_covid) ) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_point(size = 2) +
  geom_text(aes(x = -0.22,y = 10, label = "Labour Force Recovered\nIndustry GDP\nStruggling"), size = 3) +
  geom_text(aes(x = -0.22,y = -12, label = "Labour Force Not Recovered\nIndustry GDP\nStruggling"), size = 3) +
  geom_text(aes(x = 0.15,y = 10, label = "Labour Force Recovered\nIndustry GDP\nRecovered"), size = 3) +
  geom_text(aes(x = 0.15,y = -12, label = "Labour Force Not Recovered\nIndustry GDP\nRecovered"), size = 3) +
  geom_label_repel(aes(label = industry), size = 2.5) +
  theme_minimal() + 
  ylab( ("Labour Force Index\n(Pre-Covid - Current)") ) +
  xlab( ("Year on Year GDP Change\n(%)") ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) 
  
  
