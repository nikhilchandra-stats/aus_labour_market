
debugSource("C:/Users/nchandra/Documents/dev_nik/scripts/under_development/ABS/ABS_eco_data2.R")
debugSource("C:/Users/nchandra/Documents/dev_nik/employment_adhoc/stp_data_clean.R")
debugSource("C:/Users/nchandra/Documents/dev_nik/employment_adhoc/read_jobkeeper_local.R")
debugSource("C:/Users/nchandra/Documents/dev_nik/scripts/brief_v2/run_all_v2.R")

abs_data <- eco_tables_abs()

#-----------------------GDP BY INDUSTRY
gdp_indus <- abs_data[[12]][[2]] %>%
  mutate(sector = trimws(sector)) %>%
  mutate(sector = stringr::str_remove(sector, "\\(") ) %>%
  mutate(sector = stringr::str_remove(sector, "\\)") ) %>%
  mutate(sector = stringr::str_remove(sector, " [:upper:]") ) %>%
  mutate(sector = gsub(x = sector,pattern = "\\&", replacement = "and") ) %>%
  mutate(sector = trimws(sector)) %>%
  mutate(sector = tolower(sector)) %>%
  rename(indus2 = industry, 
         industry = sector) %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(unit == "$ Millions") %>%
  group_by(date,industry) %>%
  summarise(
    gdp_value = sum(value, na.rm = TRUE)
  ) %>%
  group_by(industry) %>%
  mutate(
    gdp_change = (gdp_value - lag(gdp_value,order_by = date))/lag(gdp_value,order_by = date)
  ) %>%
  mutate(
    gdp_change_yr_yr = (gdp_value - lag(gdp_value,4,order_by = date) )/lag(gdp_value,4,order_by = date)
  )

gdp_indus_max_date <- gdp_indus %>%
  filter(date == max(date)) %>%
  mutate(gdp_unit = "millions")

#-----------------------Business profit Ratio
profit_ratio <- abs_data[[21]][[2]] %>%
  dplyr::select(date,industry,value) %>%
  rename(profit_ratio = value) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "\\(") ) %>%
  mutate(industry = stringr::str_remove(industry, "\\)") ) %>%
  mutate(industry = stringr::str_remove(industry, " [:upper:]") ) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = tolower(industry))

#-----------------------Business sales 
profit_ratio <- abs_data[[23]][[2]] %>%
  filter(series_type == "Seasonally Adjusted") %>%
  dplyr::select(date,industry,value) %>%
  rename(profit_ratio = value) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "\\(") ) %>%
  mutate(industry = stringr::str_remove(industry, "\\)") ) %>%
  mutate(industry = stringr::str_remove(industry, " [:upper:]") ) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = tolower(industry))


#-----------------------Business sales 
sales <- abs_data[[24]][[2]] %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(measure == "Sales") %>%
  dplyr::select(date,industry,value) %>%
  rename(sales = value) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "\\(") ) %>%
  mutate(industry = stringr::str_remove(industry, "\\)") ) %>%
  mutate(industry = stringr::str_remove(industry, " [:upper:]") ) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = tolower(industry))


#-----------------------Business GROSS profit 
profit_gross <- abs_data[[22]][[2]] %>%
  filter(series_type == "Seasonally Adjusted") %>%
  dplyr::select(date,industry,value) %>%
  rename(gross_profit = value) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "\\(") ) %>%
  mutate(industry = stringr::str_remove(industry, "\\)") ) %>%
  mutate(industry = stringr::str_remove(industry, " [:upper:]") ) %>%
  mutate(industry = trimws(industry))%>%
  mutate(industry = tolower(industry)) 
#-----------------------Business GROSS profit 
wages_gross <- abs_data[[23]][[2]] %>%
  filter(series_type == "Seasonally Adjusted") %>%
  dplyr::select(date,industry,value) %>%
  rename(wages = value) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "\\(") ) %>%
  mutate(industry = stringr::str_remove(industry, "\\)") ) %>%
  mutate(industry = stringr::str_remove(industry, " [:upper:]") ) %>%
  mutate(industry = trimws(industry))%>%
  mutate(industry = tolower(industry)) 
#----------------------join all
gdp_join <- gdp_indus %>%
  dplyr::select(date,industry,gdp_value,gdp_change,gdp_change_yr_yr)

industry_health_qtr <- profit_ratio %>%
  left_join(profit_gross,by = c("date","industry"))%>%
  left_join(wages_gross,by = c("date","industry")) %>%
  left_join(gdp_join,by = c("date","industry")) %>%
  left_join(sales, by = c("date","industry") ) %>%
  filter(date > "2008-01-01") %>%
  filter(!is.na(.data$profit_ratio) ) %>%
  filter(!is.na(.data$wages) ) %>%
  filter(!is.na(.data$sales) ) %>%
  group_by(.data$industry) %>%
  mutate(gross_profit_change = 100*(gross_profit - lag(gross_profit,1,order_by = date) )/
           lag(gross_profit,1,order_by = date) ) %>%
  mutate(gross_profit_change4 = 100*(gross_profit - lag(gross_profit,4,order_by = date) )/
           lag(gross_profit,4,order_by = date) ) %>%
  mutate(gross_profit_ratio_change = 100*(profit_ratio - lag(profit_ratio,1,order_by = date) )/
           lag(profit_ratio,1,order_by = date) ) %>%
  mutate(gdp_perc_change_1 = 100*(gdp_value - lag(gdp_value,1,order_by = date) )/
           lag(gdp_value,1,order_by = date) ) %>%
  mutate(gdp_perc_change_4 = 100*(gdp_value - lag(gdp_value,4,order_by = date) )/
           lag(gdp_value,4,order_by = date) ) %>%
  mutate(sales_per_change_4 = 100*(sales - lag(sales,4,order_by = date) )/
           lag(sales,4,order_by = date) ) %>%
  ungroup() %>%
  filter(!is.na(sales_per_change_4)) %>%
  filter(date > min(date))

#-------------------------------------------- BUILD CUSTOM BUSINESS PERFORMANCE INDEX

industry_health_qtr_list <- industry_health_qtr %>%
  split(.$industry) %>%
  map(~.x %>% 
        dplyr::select(
                      .data$sales_per_change_4,
                      .data$wages,
                      .data$gdp_perc_change_4) 
      ) %>%
  map(~prcomp(.x) ) %>%
  map( ~data.frame(.x[[5]]) %>%
         mutate(date = unique(industry_health_qtr$date ) ))  

for (j in 1:length(industry_health_qtr_list) ) {
  
  industry_health_qtr_list[[j]] <- industry_health_qtr_list[[j]] %>%
    mutate(industry = names(industry_health_qtr_list)[j])
}

industry_health_qtr_list <- industry_health_qtr_list %>%
  map_df(bind_rows)


industry_health_qtr_list2 <- industry_health_qtr_list %>%
  mutate(
    precovid_average = 
      ifelse(date < "2020-03-01" & date > "2018-01-01", PC1, NA)
  ) %>%
  group_by(industry) %>%
  mutate(precovid_average = mean(precovid_average,na.rm = TRUE)) %>%
  mutate(differene_covid = PC1 - precovid_average ) 

industry_health_qtr_list_max_date <- 
  industry_health_qtr_list2 %>%
  filter(date == max(date))

industry_health_qtr_list2 %>%
  filter(date > "2019-01-01") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = differene_covid, color = industry))

raw_industry_health_qtr <- gdp_indus %>%
  filter(!is.nan(gdp_change_yr_yr)) %>%
  group_by(industry) %>%
  filter(date > "2008-01-01") %>%
  mutate(
    gva_index = gdp_change_yr_yr - 
      mean(ifelse(date < "2020-03-01",gdp_change_yr_yr,NA ), na.rm = TRUE)
  ) %>%
  mutate(month_date = ymd( paste0(year(date),"-",month(date), "-01") ) ) %>%
  rename(gva_date = date) 
  

#-----------------------STP BY INDUSTRY


stp_data_indus <- read.xlsx("abs data/stp_industry_2021-01-20.xlsx",sheet = 2,startRow = 6,detectDates = TRUE) %>%
  as_tibble() %>% clean_stp_indus() %>%
  rename(sub_division = subdivision) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "[:upper:]-") ) %>%
  mutate(industry = gsub(x = industry,pattern = "\\&", replacement = "and") ) %>%
  mutate(industry = trimws(industry))%>%
  mutate(industry = tolower(industry)) %>%
  mutate(sub_division = trimws(sub_division)) %>%
  mutate(sub_division = stringr::str_remove(sub_division, "[:upper:]-") ) %>%
  mutate(sub_division = gsub(x = sub_division,pattern = "\\&", replacement = "and") ) %>%
  mutate(sub_division = trimws(sub_division))%>%
  mutate(sub_division = tolower(sub_division))

stp_national <- read.xlsx("abs data/stp_national-2021-01-20.xlsx",sheet = 2,startRow = 6,detectDates = TRUE) %>%
  as_tibble() %>% clean_stp_national() %>%
  rename(state = state_or_territory,
         industry = industry_division) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "[:upper:]-") ) %>%
  mutate(industry = gsub(x = industry,pattern = "\\&", replacement = "and") ) %>%
  mutate(industry = trimws(industry))%>%
  mutate(industry = tolower(industry)) %>%
  mutate(state = trimws(state)) %>%
  mutate(state = stringr::str_remove(state, "[:upper:]-") ) %>%
  mutate(state = gsub(x = state,pattern = "\\&", replacement = "and") ) %>%
  mutate(state = trimws(state))%>%
  mutate(state = tolower(state)) 

#----------------------------

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
  ) %>%
  filter(!is.na(value)) %>%
  mutate(month_date = ymd( paste0(paste0(year(date),"-",month(date), "-01")) ) ) 

last_3_dates <- stp_data_indus2 %>% 
  distinct(date) %>% 
  filter(date %in% c(max(date) - weeks(8),max(date) - weeks(4),max(date) ) )

stp_data_gdp <- stp_data_indus2 %>%
  filter(date %in% last_3_dates$date ) %>%
  left_join(raw_industry_health_qtr, by = c("industry","month_date") ) %>%
  mutate(industry = str_wrap(industry,15) ) %>%
  mutate(industry = stringr::str_to_title(industry)) %>%
  mutate(label_x = ifelse(date < max(date), NA, industry) ) %>%
  mutate(latest_point = ifelse(date < max(date), NA, difference_since_covid)  ) %>%
  group_by(industry) %>%
  fill(gva_index, .direction = "down") %>%
  fill(gdp_change_yr_yr, .direction = "down")

min_plot_gva_value <- stp_data_gdp %>%
  ungroup() %>%
  filter(gva_index == min(gva_index)) %>%
  select(gva_index)

min_plot_stp_value <- stp_data_gdp %>%
  ungroup() %>%
  filter(difference_since_covid == min(difference_since_covid)) %>%
  select(difference_since_covid)

max_plot_gva_value <- stp_data_gdp %>%
  ungroup() %>%
  filter(gva_index == max(gva_index)) %>%
  select(gva_index)


stp_data_gdp %>% 
  cah_plot(aes(x = gva_index, y = difference_since_covid) ) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_point(aes(y =  latest_point,color =  industry ),size = 2, show.legend = FALSE) +
  geom_line(aes(color =  industry ),size = 0.75, show.legend = FALSE) +
  # geom_text(aes(x = -0.22,y = 10, label = "Labour Force Recovered\nIndustry GDP\nStruggling"), size = 3) +
  # geom_text(aes(x = -0.22,y = -13, label = "Labour Force Not Recovered\nIndustry GDP\nStruggling"), size = 3) +
  # geom_text(aes(x = 0.15,y = 10, label = "Labour Force Recovered\nIndustry GDP\nRecovered"), size = 3) +
  # geom_text(aes(x = 0.15,y = -13, label = "Labour Force Not Recovered\nIndustry GDP\nRecovered"), size = 3) +
  geom_label_repel(aes(label = label_x,color =  industry), size = 2.5,
                   force_pull = 1, force = 40, show.legend = FALSE) +
  ylim(min_plot_stp_value$difference_since_covid[1] - 0.01,12)+
  theme_minimal() + 
  ylab( ("Labour Force Index (Indexed from Feb 2020)\n[Pre-Covid - Current]") ) +
  xlab( ("Gross Value Added\n[Pre-Covid - Current]") ) +
  theme(axis.title = element_text(size = 12))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(min_plot_gva_value$gva_index[1] - 0.02,0.17) ) + 
  #labs(title = "Industry Recovery Tracker")+
  ggsave(filename = file.path(G_output_path,
                                strftime(G_timestamp_declaration,
                                         "labour_risk_matrix_%Y_%m_%d_%P.png")),
         dpi = 400,
         width = 8,
         height = 5.0,
         device = "png",
         type = "cairo",
         scale = 1.1) 

#---------------------------------------STP Look AHEAD

#------Create tibble and forecast for average across all industries

create_stp_forecast <- function(.stp_data_indus,.recovery_date = "2020-06-15"){
  
  indus_abbrev <- stringr::str_trunc(as.character(.stp_data_indus$industry[1]),width = 10,ellipsis = "_") 
  
  stp_data_indus_max_date <- .stp_data_indus %>%
    filter(date < as_date("2020-04-01") ) %>%
    summarise(value = mean(value,na.rm = TRUE)) %>%
    rename(max_value = value) %>%
    select(max_value)
  
  .stp_data_indus <-.stp_data_indus %>%
    filter(!is.na(industry)) %>%
    filter(!is.na(value)) 
  
  max_stp_date <- max(.stp_data_indus$date)
  
  stp_data_indus2 <- .stp_data_indus %>%
    filter(!is.na(industry)) %>%
    filter(!is.na(value)) %>%
    filter(value != "NaN") %>%
    group_by(date) %>%
    summarise(value = mean(value,na.rm = TRUE)) %>%
    bind_cols(max_value = stp_data_indus_max_date$max_value[1]) %>%
    mutate(index_value = value - max_value) %>%
    mutate(
      positive_covid = 
        ifelse(index_value>=0, index_value, 0 )
    ) %>%
    mutate(
      negative_covid = 
        ifelse(index_value<=0, index_value, 0 )
    ) %>%
    mutate(reference = 0) 
  
  forecast_frame <- stp_data_indus2 %>%
    mutate(date = .data$date + weeks(12) ) %>%
    filter(.data$date > max_stp_date ) %>%
    mutate(across(!starts_with("date"), ~ NA))
  
  model <- 
    stp_data_indus2 %>%
    filter(date > .recovery_date) %>% 
    mutate(diff = value - lag(value,1, order_by = date) ) %>%
    filter(!is.na(diff)) 
  
  distr_model <- data.frame( fitdistrplus::fitdist(model$diff,distr = "norm" )$estimate )
  
  #Sum of normal IID variables is mean = mu + mu + mu...   var = var + var...
  forecast_frame2 <- stp_data_indus2 %>%
    filter(date == max(date))%>%
    bind_rows(forecast_frame) %>%
    mutate(forecast_mean = distr_model[1,1] ) %>%
    mutate(forecast_sd = distr_model[2,1]) %>%
    mutate(forecast_mean = cumsum(forecast_mean)) %>%
    mutate(forecast_sd = sqrt( cumsum(forecast_sd^2) ) ) %>%
    mutate(forecast_mean = lag(forecast_mean)) %>%
    mutate(forecast_sd = lag(forecast_sd) ) %>%
    fill(value, .direction = "down") %>%
    fill(forecast_sd, .direction = "up") %>%
    fill(forecast_mean, .direction = "up") %>%
    group_by(forecast_mean) %>%
    mutate(expected = value + quantile(rnorm(100,forecast_mean,forecast_sd),probs = 0.5 )) %>%
    mutate(lower_value1 =  value + quantile(rnorm(100,forecast_mean,forecast_sd),probs = 0.25 ) )%>%
    mutate(lower_value2 =  value + quantile(rnorm(100,forecast_mean,forecast_sd),probs = 0.05 ) ) %>% 
    mutate(high_value1 =  value + quantile(rnorm(100,forecast_mean,forecast_sd),probs = 0.75 ) )%>%
    mutate(high_value2 =  value + quantile(rnorm(100,forecast_mean,forecast_sd),probs = 0.95 ) ) %>%
    ungroup() %>%
    mutate(value = ifelse(date > min(date), NA, value)) %>%
    filter(date != min(date))
  
  complete_frame <- stp_data_indus2 %>%
    bind_rows(forecast_frame2) %>%
    mutate(
      lower_value1 = ifelse(is.na(lower_value1), value, lower_value1),
      lower_value2 = ifelse(is.na(lower_value2), value, lower_value2),
      high_value1 = ifelse(is.na(high_value1), value, high_value1),
      high_value2 = ifelse(is.na(high_value2), value, high_value2),
      expected = ifelse(is.na(expected), value, expected)
    ) %>%
    mutate(
      upper_shading_region = ifelse(date >= max_stp_date,high_value1,NA ),
      lower_shading_region = ifelse(date >= max_stp_date,lower_value2,NA ),
      expected_shading_region = ifelse(date >= max_stp_date,expected,NA )
    ) %>%
    mutate(industry = indus_abbrev)
  
  return(complete_frame)
  
}

#------Create PLOT for average across all industries

plot_and_save_industry <- function(.data){
  indus_abbrev <- stringr::str_trunc(as.character(.data$industry[1]),width = 10,ellipsis = "_") 
  .data %>%
    filter(date > "2020-02-01") %>%
    cah_plot(aes(x = date)) +
    geom_hline(yintercept = 100, linetype = "dashed") +
    geom_line(aes(y = expected), linetype = "dashed") +
    geom_line(aes(y = lower_value2), linetype = "dashed") +
    #geom_line(aes(y = lower_value2), linetype = "dashed") +
    geom_line(aes(y = high_value1), linetype = "dashed") +
    # geom_line(aes(y = high_value2), linetype = "dashed") +
    geom_line(aes(y = value)) + 
    geom_ribbon(aes(ymax = upper_shading_region, ymin = expected_shading_region), 
                alpha = 0.25,
                fill = cah_colours[4]) +
    geom_ribbon(aes(ymax = lower_shading_region, ymin = expected_shading_region), 
                alpha = 0.25,
                fill = cah_colours[8]) +
    # geom_ribbon(aes(ymax = negative_covid, ymin = reference), alpha = 0.25, fill = "red") +
    ylab(("Labour Force Index (Indexed from March 2020)\n[Pre-Covid - Current]")) + 
    theme(axis.title.x = element_blank())+
    ggsave(filename = file.path(G_output_path,
                                strftime(G_timestamp_declaration,
                                         paste0(indus_abbrev,"industry_spotlight_%Y_%m_%d_%P.png") )),
           dpi = 400,
           width = 5,
           height = 3,
           device = "png",
           type = "cairo",
           scale = 1.1) 
}

#----------------------------------------------------------------------------------------------------

stp_national2 <- stp_national %>%
  filter(state == "australia") %>%
  filter(industry == "all industries")

complete_frame <- create_stp_forecast(.stp_data_indus = stp_national2) %>%
  plot_and_save()


data_for_plot_list <- stp_data_indus %>%
  split(.$industry) %>%
  map(create_stp_forecast) %>%
  map(plot_and_save_industry)
