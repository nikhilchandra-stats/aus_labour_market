load("temp_dta.RData")

function(.data = returned_data ){
  
#-----------------------GDP BY INDUSTRY
gdp_indus <- .data[[12]][[2]] %>%
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
profit_ratio <- .data[[21]][[2]] %>%
  dplyr::select(date,industry,value) %>%
  rename(profit_ratio = value) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "\\(") ) %>%
  mutate(industry = stringr::str_remove(industry, "\\)") ) %>%
  mutate(industry = stringr::str_remove(industry, " [:upper:]") ) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = tolower(industry))

#-----------------------Business sales 
profit_ratio <- .data[[23]][[2]] %>%
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
sales <- .data[[24]][[2]] %>%
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
profit_gross <- .data[[22]][[2]] %>%
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
wages_gross <- .data[[23]][[2]] %>%
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


stp_data_indus <- .data[[34]] %>%
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

stp_national <- .data[[36]]%>%
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
  mutate(state = tolower(state)) %>%
  mutate(age = trimws(age_group)) %>%
  mutate(state = stringr::str_to_title(state) )
  


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

max_stp_date <- max(last_3_dates$date)
max_gva_date <- max(raw_industry_health_qtr$gva_date)

month_diff <- lubridate::interval(as_date(max_stp_date), as_date(max_gva_date) ) %/% months(1) 

raw_industry_health_qtr<- raw_industry_health_qtr %>%
  mutate(month_date = month_date + months( abs(month_diff + 1) ) )


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

#----------------------------------------GET INDUSTRY SIZES

industry_sizes <- .data[[33]][[2]] %>%
  filter(unit == "000") %>%
  filter(employment_status %in% c("Employed total", "Unemployed total") ) %>%
  group_by(industry,date) %>%
  summarise(
    value = sum(value,na.rm = TRUE)
  ) %>%
  filter(date == max(date)) %>%
  rename(
    pop_size = value 
  ) %>%
  select(-date) %>%
  mutate(industry = trimws(industry)) %>%
  mutate(industry = stringr::str_remove(industry, "[:upper:]-") ) %>%
  mutate(industry = gsub(x = industry,pattern = "\\&", replacement = "and") ) %>%
  mutate(industry = trimws(industry))%>%
  mutate(industry = tolower(industry)) %>%
  mutate(industry = str_wrap(industry,15) ) %>%
  mutate(industry = stringr::str_to_title(industry))

#---------------------------JOIN ON INDUSTRY SIZE
stp_data_gdp2 <- stp_data_gdp %>%
  left_join(industry_sizes, by = "industry") 

#-------------------------------------------------------------Complete Data


#----------------------------------------------------------

 stp_data_gdp2 %>% 
  ggplot(aes(x = gva_index, y = difference_since_covid) ) +
  #geom_ribbon(aes(ymin = -17, ymax = -0.1), alpha = 0.25, fill = "red")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_point(aes(y =  latest_point, 
                 size = pop_size,
                 #color = difference_since_covid,
                 group = industry ), 
             show.legend = TRUE,color = "#003366") +
  geom_line(aes(group = industry
                #,color = difference_since_covid 
                ),size = 0.75, show.legend = FALSE,color = "#003366") +
  # geom_text(aes(x = -0.22,y = 10, label = "Labour Force Recovered\nIndustry GDP\nStruggling"), size = 3) +
  # geom_text(aes(x = -0.22,y = -13, label = "Labour Force Not Recovered\nIndustry GDP\nStruggling"), size = 3) +
  # geom_text(aes(x = 0.15,y = 10, label = "Labour Force Recovered\nIndustry GDP\nRecovered"), size = 3) +
  # geom_text(aes(x = 0.15,y = -13, label = "Labour Force Not Recovered\nIndustry GDP\nRecovered"), size = 3) +
  geom_label_repel(aes(label = label_x), size = 2.5,
                   force_pull = 1, force = 40, show.legend = FALSE,segment.size = 0.25) +
  #scale_color_gradient(  low = "#ff6600",  high = "#003366") +
  ylim(min_plot_stp_value$difference_since_covid[1] - 0.01,12)+
  theme_minimal() + 
  ylab( ("Labour Force Index (Indexed from Feb 2020)\n[Pre-Covid - Current]") ) +
  xlab( ("Gross Value Added\n[Pre-Covid - Current]") ) +
  labs(size = "Labour Force\nSize ('000s')")+
  theme(axis.title = element_text(size = 12),legend.position = "bottom")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(min_plot_gva_value$gva_index[1] - 0.02,0.17) ) 

}

