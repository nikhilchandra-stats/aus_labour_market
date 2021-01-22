

#-----Clean join and wrangle
eco_tables_abs <- function(long_or_wide = "wide"){
  
  # install_check <- svDialogs::dlg_input(message = "Do you have 'readabs' installed ? 
  #                      type YES/NO")$res
  # 
  # if(install_check == "NO"){
  #   
  #   install.packages("readabs")
  # }
  # 
  # 
  # G_input_path <- paste0(pmo_folder("data"), "/")
  # 
  # SA4_regions_mapped <- read_csv( paste0(G_input_path,"SA4_concordance_CED.csv") ) %>%
  #   mutate(sa4_name = as.character(sa4_name) )
  # 
  
  return_list = list()

  # ##Property index
  dta_residential_prop_price_index <- readabs::read_abs("6416.0", tables = 1) %>%
    select(date,series,value,unit)

  property_index_col_1 <- data.frame( stringr::str_split_fixed( dta_residential_prop_price_index$series,pattern = ";",n = 3 ) )[,1:2]
  names(property_index_col_1) = c("type","state")

  dta_residential_prop_price_index <- dta_residential_prop_price_index %>%
    select(date,value,unit) %>% bind_cols(property_index_col_1)

  dta_residential_prop_price_index <- dta_residential_prop_price_index %>%
    mutate(
           type = trimws(type),
           state = trimws(state)
           )

  return_list[[1]] = list( "property index table 6416.0 table 1",dta_residential_prop_price_index )

  # ## Labour Force
  dta_labour_force <- readabs::read_abs("6202.0", tables = c(4,5,6,7,8,9,12)) %>%
    select(date,series,value,unit,series_type)

  labour_force_col_1 <- data.frame( stringr::str_split_fixed( dta_labour_force$series,pattern = ";",n = 4 ) )[,1:3]

  dta_labour_force <- dta_labour_force %>%
    select(date,value,unit,series_type) %>% bind_cols(labour_force_col_1) %>%
    rename(employment_status = X1, person_type = X2, state = X3) %>%
    mutate(employment_status = gsub(employment_status,pattern = ">",replacement = "") ) %>%
    mutate(person_type = gsub(person_type,pattern = ">",replacement = "") ) %>%
    mutate(state = gsub(state,pattern = ">",replacement = "") )

  dta_labour_force <- dta_labour_force %>%
    mutate(

      employment_status = trimws(employment_status),
      person_type = trimws(person_type),
      state = trimws(state)

    )

  return_list[[2]] = list("labour force table 6202.0 tables 4 to 12" ,dta_labour_force )

  # ## Labour Force detailed 6291.0.55.001
  dta_labour_force_2 <- readabs::read_abs("6291.0.55.001", tables = c(2)) %>%
    select(date,series,value,unit,series_type)

  labour_force_col_1 <- data.frame( stringr::str_split_fixed( dta_labour_force_2$series,pattern = ";",n = 4 ) )[,1:3]

  dta_labour_force_2 <- dta_labour_force_2 %>%
    select(date,value,unit,series_type) %>% bind_cols(labour_force_col_1) %>%
    rename(state = X1, employment_status = X2, person_type = X3) %>%
    mutate(employment_status = gsub(employment_status,pattern = ">",replacement = "") ) %>%
    mutate(person_type = gsub(person_type,pattern = ">",replacement = "") ) %>%
    mutate(state = gsub(state,pattern = ">",replacement = "") )

  dta_labour_force_2 <- dta_labour_force_2 %>%
    mutate(

      employment_status = trimws(employment_status),
      person_type = trimws(person_type),
      state = trimws(state)

    )

  return_list[[3]] = list( "labour force detailed breakdown 6291.0.55.001 table 2" ,dta_labour_force_2 )

  # ## Retial Trade
  dta_retail_trade <- readabs::read_abs("8501.0", tables = 12) %>%
    select(date,series,value,unit,series_type)

  dta_retail_trade_col_1 <- data.frame( stringr::str_split_fixed( dta_retail_trade$series,pattern = ";",n = 4 ) )[,2:3]

  dta_retail_trade <- dta_retail_trade %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_retail_trade_col_1) %>%
    rename(state = X2, type = X3)

  dta_retail_trade <- dta_retail_trade %>%
    mutate(

      state = trimws(state),
      type = trimws(type)

    )

  return_list[[4]] = list("retail trade 8501.0 table 11" ,dta_retail_trade )

  # ## Building approvals Dwellings By Statistical Area
  dta_dwelling_approvals <- readabs::read_abs("8731.0", tables = c(10) ) %>%
    select(date,series,value,unit,series_type)

  dta_dwelling_approvals_col_1 <- data.frame( stringr::str_split_fixed( dta_dwelling_approvals$series,pattern = ";",n = 5 ) )[,2:3]

  dta_dwelling_approvals <- dta_dwelling_approvals %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_dwelling_approvals_col_1) %>%
    rename(building_type = X2, SA4 = X3)

  dta_dwelling_approvals <- dta_dwelling_approvals %>%
    mutate(
      building_type = trimws(building_type),
      SA4 = trimws(SA4)
    )

  return_list[[5]] = list("dwelling approvals 8731.0 table 10" ,dta_dwelling_approvals )

  # ## Building approvals non residential by state and industry
  dta_non_residential <- readabs::read_abs("8731.0", tables = c(52,59) ) %>%
    select(date,series,value,unit,series_type)

  dta_non_residential_col_1 <- data.frame( stringr::str_split_fixed( dta_non_residential$series,pattern = ";",n = 5 ) )[,1:4]

  dta_non_residential <- dta_non_residential %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_non_residential_col_1) %>%
    rename(value_type = X1, state = X2, sector = X3, industry = X4)

  dta_non_residential <- dta_non_residential %>%
    mutate(

      value_type = trimws(value_type),
      state = trimws(state),
      sector = trimws(sector),
      industry = trimws(industry)

    )

  return_list[[6]] = list( "dwelling approvals 8731.0 table 52 and 59",dta_non_residential )

  # ## Debt personal commitments
  dta_house_hold_credit <- readabs::read_abs("5601.0", tables = c(27) ) %>%
    select(date,series,value,unit,series_type)

  dta_house_hold_credit_col_1 <- data.frame( stringr::str_split_fixed( dta_house_hold_credit$series,pattern = ";",n = 6 ) )[,1:5]

  dta_house_hold_credit <- dta_house_hold_credit %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_house_hold_credit_col_1) %>%
    rename(house_business = X1, finance_type = X2, finance_type2 = X3, purpose = X4, commitment = X5)

  dta_house_hold_credit <- dta_house_hold_credit %>%
    mutate(

      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      finance_type2 = trimws(finance_type2),
      purpose = trimws(purpose),
      commitment = trimws(commitment)

    )

  return_list[[7]] = list( "house hold credit 5601.0, table 27" ,dta_house_hold_credit )

  ## Business loans by purpose and business size
  dta_business_loans_purpose <- readabs::read_abs("5601.0", tables = c(31,33,35) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_loans_purpose_col_1 <- data.frame( stringr::str_split_fixed( dta_business_loans_purpose$series,pattern = ";",n = 7 ) )[,1:6]

  dta_business_loans_purpose <- dta_business_loans_purpose %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_loans_purpose_col_1) %>%
    rename(house_business = X1, finance_type = X2, loan_type = X3, business_size = X4, purpose = X5,commitment = X6)

  dta_business_loans_purpose <-
    dta_business_loans_purpose %>%
    mutate(
      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      loan_type = trimws(loan_type),
      business_size = trimws(business_size),
      purpose = trimws(purpose),
      commitment = trimws(commitment)
    )

  return_list[[8]] = list( "business loans 5601.0 tables 31 33 and 35" ,dta_business_loans_purpose )

  # ## Business loans by state and business size
  dta_business_loans_state <- readabs::read_abs("5601.0", tables = c(32,34) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_loans_state_col_1 <- data.frame( stringr::str_split_fixed( dta_business_loans_state$series,pattern = ";",n = 7 ) )[,1:6]

  dta_business_loans_state <- dta_business_loans_state %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_loans_state_col_1) %>%
    rename(house_business = X1, finance_type = X2, loan_type = X3, business_size = X4, purpose = X5,state = X6)

  dta_business_loans_state <-
    dta_business_loans_state %>%
    mutate(
      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      loan_type = trimws(loan_type),
      business_size = trimws(business_size),
      purpose = trimws(purpose),
      state = trimws(state)

    )

  return_list[[9]] = list("business loans by state 5601.0 tables 32 and 34" ,dta_business_loans_state )

  # ## Key national aggregates
  dta_key_national_aggregates <- readabs::read_abs("5206.0", tables = c(1) ) %>%
    select(date,series,value,unit,series_type,frequency)

  dta_key_national_aggregates_col_1 <- data.frame( stringr::str_split_fixed( dta_key_national_aggregates$series,pattern = ";",n = 2 ) )[,1:2]

  dta_key_national_aggregates <- dta_key_national_aggregates %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_key_national_aggregates_col_1) %>%
    rename(measure = X1) %>%
    select(-X2)

  dta_key_national_aggregates <-
    dta_key_national_aggregates %>%
    mutate(

      measure = trimws(measure)
    )

  return_list[[10]] = list("key national aggregates 5206.0 table 1" ,dta_key_national_aggregates )

  # ## Compensation of employees by state, current prices
  dta_employee_compensation <- readabs::read_abs("5206.0", tables = c(44) ) %>%
    select(date,series,value,unit,series_type,frequency,series_type)

  dta_employee_compensation_col_1 <- data.frame( stringr::str_split_fixed( dta_employee_compensation$series,pattern = ";",n = 4 ) )[,1:3]

  dta_employee_compensation <- dta_employee_compensation %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_employee_compensation_col_1)%>%
    rename(state = X1, value_type = X2, measure_type = X3)

  dta_employee_compensation <-
    dta_employee_compensation %>%
    mutate(

      measure_type = trimws(measure_type),
      state = trimws(state),
      value_type = trimws(value_type)

    )

  return_list[[11]] = list("employee compensation 5206.0 table 44" ,dta_employee_compensation )

  # ## Gross Value added by industry
  dta_value_added_by_industry <- readabs::read_abs("5206.0", tables = c(6) ) %>%
    select(date,series,value,unit,series_type,frequency,series_type)

  dta_value_added_by_industry_col_1 <- data.frame( stringr::str_split_fixed( dta_value_added_by_industry$series,pattern = ";",n = 4 ) )[,1:2]

  dta_value_added_by_industry <- dta_value_added_by_industry %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_value_added_by_industry_col_1)%>%
    rename(sector = X1, industry = X2)

  dta_value_added_by_industry <-
    dta_value_added_by_industry %>%
    mutate(

      sector = trimws(sector),
      industry = trimws(industry),

    )

  return_list[[12]] = list("gross value added by industry 5206.0 table 6" ,dta_value_added_by_industry )

  # ## state final demand measures the total value of goods sold in a state to buyers who wish to consume them
  # dta_state_final_demand <- readabs::read_abs("5206.0", tables = c(26,27,28,29,30,31,32,33) ) %>%
  #   select(date,series,value,unit,series_type,frequency,table_title)
  # 
  # dta_state_final_demand_col_1 <- data.frame( stringr::str_split_fixed( dta_state_final_demand$series,pattern = ";",n = 4 ) )[,1:2]
  # names(dta_state_final_demand_col_1) = c("sector","drop1")
  # dta_state_final_demand_measure <- data.frame( stringr::str_split_fixed( dta_state_final_demand_col_1$drop1,pattern = ":",n = 2 ) )[,1:2]
  # names(dta_state_final_demand_measure) = c("consumption_type","measure")
  # dta_state_final_demand_states <- data.frame( stringr::str_split_fixed( dta_state_final_demand$table_title,pattern = ":",n = 2 ) )[,1:2]
  # names(dta_state_final_demand_states)= c("drop2","state")
  # 
  # binding_frame <- dta_state_final_demand_col_1 %>% bind_cols(dta_state_final_demand_measure) %>%
  #   bind_cols(dta_state_final_demand_states) %>% select(-c(drop1,drop2))
  # 
  # dta_state_final_demand <- dta_state_final_demand %>%
  #   select(date,value,unit,frequency,series_type) %>% bind_cols(binding_frame)
  # 
  # dta_state_final_demand <-
  #   dta_state_final_demand %>%
  #   mutate(
  # 
  #     sector = trimws(sector),
  #     consumption_type = trimws(consumption_type),
  #     measure = trimws(measure),
  #     state = trimws(state)
  # 
  #   )
# 
#   return_list[[13]] = list( "state final deman total value of goods sold 5206 tables 26-33",dta_state_final_demand )

  # ## state final demand measures the total value of goods sold in a state to buyers who wish to consume them
  # dta_state_final_demand_summary <- readabs::read_abs("5206.0", tables = c(25) ) %>%
  #   select(date,series,value,unit,series_type,frequency,table_title)
  # 
  # dta_state_final_demand_summary_col_1 <- data.frame( stringr::str_split_fixed( dta_state_final_demand_summary$series,pattern = ";",n = 4 ) )[,1:3]
  # names(dta_state_final_demand_summary_col_1) = c("state","sector","consumption_type")
  # dta_state_final_demand_summary_measure <- data.frame( stringr::str_split_fixed( dta_state_final_demand_summary$table_title,pattern = ":",n = 2 ) )[,1:2]
  # names(dta_state_final_demand_summary_measure) = c("drop1","measure")
  # 
  # bind_frame <- bind_cols(dta_state_final_demand_summary_col_1,dta_state_final_demand_summary_measure) %>%
  #   select(-drop1)
  # 
  # dta_state_final_demand_summary <- dta_state_final_demand_summary %>%
  #   select(date,series_type,unit,value,frequency,series_type) %>% bind_cols(bind_frame)
  # 
  # dta_state_final_demand_summary <-
  #   dta_state_final_demand_summary %>%
  #   mutate(
  # 
  #     state = trimws(state),
  #     sector = trimws(sector),
  #     consumption_type = trimws(consumption_type),
  #     measure = trimws(measure)
  # 
  #   )
  # 
  # return_list[[14]] = list( "state final deman total value of goods sold 5206 tables 25",dta_state_final_demand_summary )

  # ## Total factor income by indusry and princpial components
  dta_total_factor_inc_industry <- readabs::read_abs("5204.0", tables = c(46) ) %>%
    select(date,series,value,unit,series_type,frequency,table_title)

  dta_total_factor_inc_industry_col_1 <- data.frame( stringr::str_split_fixed( dta_total_factor_inc_industry$series,pattern = ";",n = 3 ) )[,1:2]
  names(dta_total_factor_inc_industry_col_1) = c("industry","measure")

  bind_frame <- dta_total_factor_inc_industry_col_1

  dta_total_factor_inc_industry <- dta_total_factor_inc_industry %>%
    select(date,series_type,unit,value,frequency,series_type) %>% bind_cols(bind_frame)

  dta_total_factor_inc_industry <-
    dta_total_factor_inc_industry %>%
    mutate(

      industry = trimws(industry),
      measure = trimws(measure)

    )

  return_list[[15]] = list( "Total factor income by indusry and princpial components 5204 tables 46",dta_total_factor_inc_industry )

  # ## GROSS VALUE ADDED BY INDUSTRY
  dta_total_GVA_industry <- readabs::read_abs("5204.0", tables = c(5) ) %>%
    select(date,series,value,unit,series_type,frequency,table_title)

  dta_total_GVA_industry_col_1 <- data.frame( stringr::str_split_fixed( dta_total_GVA_industry$series,pattern = ";",n = 3 ) )[,1:2]
  names(dta_total_GVA_industry_col_1) = c("industry","measure")

  bind_frame <- dta_total_GVA_industry_col_1

  dta_total_GVA_industry <- dta_total_GVA_industry %>%
    select(date,series_type,unit,value,frequency,series_type) %>% bind_cols(bind_frame)

  dta_total_GVA_industry <-
    dta_total_GVA_industry %>%
    mutate(

      industry = trimws(industry),
      measure = trimws(measure)

    )

  return_list[[16]] = list( "GROSS VALUE ADDED BY INDUSTRY 5204 tables 5",dta_total_GVA_industry )

  # ## Business loans
  dta_business_loans <- readabs::read_abs("5601.0", tables = c(30) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_loans_col_1 <- data.frame( stringr::str_split_fixed( dta_business_loans$series,pattern = ";",n = 7 ) )[,1:5]

  dta_business_loans <- dta_business_loans %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_loans_col_1) %>%
    rename(house_business = X1, finance_type = X2, loan_type = X3, purpose = X4,commitment = X5)

  dta_business_loans <-
    dta_business_loans %>%
    mutate(
      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      loan_type = trimws(loan_type),
      purpose = trimws(purpose),
      commitment = trimws(commitment)

    )

  return_list[[17]] = list("business loans by state 5601.0 tables 32 and 34" ,dta_business_loans_state )

  #
  # ## Personal loans
  dta_personal_loans <- readabs::read_abs("5601.0", tables = c(27) ) %>%
    select(date,series,value,unit,series_type,series_type)

  dta_personal_loans_col_1 <- data.frame( stringr::str_split_fixed( dta_personal_loans$series,pattern = ";",n = 7 ) )[,1:5]

  dta_personal_loans <- dta_personal_loans %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_personal_loans_col_1) %>%
    rename(house_business = X1, finance_type = X2, loan_type = X3, purpose = X4,commitment = X5)

  dta_personal_loans <-
    dta_personal_loans %>%
    mutate(
      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      loan_type = trimws(loan_type),
      purpose = trimws(purpose),
      commitment = trimws(commitment)

    )

  return_list[[18]] = list("personal loans/creedit by purpose 5601.0 tables 27" ,dta_personal_loans )

  # ## Personal loans 2
  dta_personal_loans2 <- readabs::read_abs("5601.0", tables = c(28) ) %>%
    select(date,series,value,unit,series_type)

  dta_personal_loans2_col_1 <- data.frame( stringr::str_split_fixed( dta_personal_loans2$series,pattern = ";",n = 7 ) )[,1:5]

  dta_personal_loans2 <- dta_personal_loans2 %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_personal_loans2_col_1) %>%
    rename(house_business = X1, finance_type = X2, loan_type = X3, purpose = X4,commitment = X5)

  dta_personal_loans2 <-
    dta_personal_loans2 %>%
    mutate(
      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      loan_type = trimws(loan_type),
      purpose = trimws(purpose),
      commitment = trimws(commitment)

    )

  return_list[[19]] = list("revolving credit by purpose 5601.0 tables 28" ,dta_personal_loans2 )

  # ## Business loans 2
  dta_personal_loans2 <- readabs::read_abs("5601.0", tables = c(30) ) %>%
    select(date,series,value,unit,series_type)

  dta_personal_loans2_col_1 <- data.frame( stringr::str_split_fixed( dta_personal_loans2$series,pattern = ";",n = 7 ) )[,1:5]

  dta_personal_loans2 <- dta_personal_loans2 %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_personal_loans2_col_1) %>%
    rename(house_business = X1, finance_type = X2, loan_type = X3, purpose = X4,commitment = X5)

  dta_personal_loans2 <-
    dta_personal_loans2 %>%
    mutate(
      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      loan_type = trimws(loan_type),
      purpose = trimws(purpose),
      commitment = trimws(commitment)

    )

  return_list[[20]] = list("revolving credit by purpose 5601.0 tables 29" ,dta_personal_loans2 )

  # #############################################################################################################
  # ## Business loans 2
  dta_personal_loans2 <- readabs::read_abs("5601.0", tables = c(30) ) %>%
    select(date,series,value,unit,series_type)

  dta_personal_loans2_col_1 <- data.frame( stringr::str_split_fixed( dta_personal_loans2$series,pattern = ";",n = 7 ) )[,1:5]

  dta_personal_loans2 <- dta_personal_loans2 %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_personal_loans2_col_1) %>%
    rename(house_business = X1, finance_type = X2, loan_type = X3, purpose = X4,commitment = X5)

  dta_personal_loans2 <-
    dta_personal_loans2 %>%
    mutate(
      house_business = trimws(house_business),
      finance_type = trimws(finance_type),
      loan_type = trimws(loan_type),
      purpose = trimws(purpose),
      commitment = trimws(commitment)

    )

  return_list[[20]] = list("revolving credit by purpose 5601.0 tables 29" ,dta_personal_loans2 )


  # #############################################################################################################
  # ## Business profits
  dta_business_profits <- readabs::read_abs("5676.0", tables = c(22) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_profits_col_1 <- data.frame( stringr::str_split_fixed( dta_business_profits$series,pattern = ";",n = 7 ) )[,1:4]

  dta_business_profits <- dta_business_profits %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_profits_col_1) %>%
    rename(measure = X1, state = X2, industry = X3, unit_type = X4)

  dta_business_profits <-
    dta_business_profits %>%
    mutate(
      measure = trimws(measure),
      state = trimws(state),
      industry = trimws(industry),
      unit_type = trimws(unit_type)

    )

  return_list[[21]] = list("Business Profit/Revenue 5676.0 tables 22" ,dta_business_profits )

  # #############################################################################################################
  # ## Business profits
  dta_business_gross_profits <- readabs::read_abs("5676.0", tables = c(15) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_gross_profits_col_1 <- data.frame( stringr::str_split_fixed( dta_business_gross_profits$series,pattern = ";",n = 7 ) )[,1:4]

  dta_business_gross_profits <- dta_business_gross_profits %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_gross_profits_col_1) %>%
    rename(measure = X1, state = X2, industry = X3, unit_type = X4)

  dta_business_gross_profits <-
    dta_business_gross_profits %>%
    mutate(
      measure = trimws(measure),
      state = trimws(state),
      industry = trimws(industry),
      unit_type = trimws(unit_type)

    )

  return_list[[22]] = list("Business Gross Profit 5676.0 tables 15" ,dta_business_gross_profits )

  # #############################################################################################################
  # ## Business Wages and Salaries
  dta_business_gross_wages <- readabs::read_abs("5676.0", tables = c(18) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_gross_wages_col_1 <- data.frame( stringr::str_split_fixed( dta_business_gross_wages$series,pattern = ";",n = 7 ) )[,1:6]

  dta_business_gross_wages <- dta_business_gross_wages %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_gross_wages_col_1) %>%
    rename(measure = X1, state = X2, industry = X3, unit_type = X4)

  dta_business_gross_wages <-
    dta_business_gross_wages %>%
    mutate(
      measure = trimws(measure),
      state = trimws(state),
      industry = trimws(industry),
      unit_type = trimws(unit_type)

    )

  return_list[[23]] = list("Business Gross wages 5676.0 tables 18" ,dta_business_gross_wages )

  #
  # #############################################################################################################
  ## Business Sales
  dta_business_sales<- readabs::read_abs("5676.0", tables = c(4) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_sales_col_1 <- data.frame( stringr::str_split_fixed( dta_business_sales$series,pattern = ";",n = 7 ) )[,1:6]

  dta_business_sales <- dta_business_sales %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_sales_col_1) %>%
    rename(measure = X1, state = X2, industry = X3, unit_type = X4)

  dta_business_sales <-
    dta_business_sales %>%
    mutate(
      measure = trimws(measure),
      state = trimws(state),
      industry = trimws(industry),
      unit_type = trimws(unit_type)

    )

  return_list[[24]] = list("Business Income from Sales 5676.0 tables 4" ,dta_business_sales )
  #
  # #############################################################################################################
  ## Business Inventories
  dta_business_inventories<- readabs::read_abs("5676.0", tables = c(1) ) %>%
    select(date,series,value,unit,series_type)

  dta_business_inventories_col_1 <- data.frame( stringr::str_split_fixed( dta_business_inventories$series,pattern = ";",n = 7 ) )[,1:6]

  dta_business_inventories <- dta_business_inventories %>%
    select(date,value,unit,series_type) %>% bind_cols(dta_business_inventories_col_1) %>%
    rename(measure = X1, state = X2, industry = X3, unit_type = X4)

  dta_business_inventories <-
    dta_business_inventories %>%
    mutate(
      measure = trimws(measure),
      state = trimws(state),
      industry = trimws(industry),
      unit_type = trimws(unit_type)

    )

  return_list[[25]] = list("Business from inventories 5676.0 tables 4" ,dta_business_inventories )

  #############################################################################################################
  # ## labour by state and sex
  dta_emp_state_sex<- readabs::read_abs("6202.0", tables = c("4","5",'6',"7","8","9","10a","11a") ) %>%
    dplyr::select(date,table_title,series,value,unit,series_type)

  dta_emp_state_sex_col_1 <- data.frame( stringr::str_split_fixed( dta_emp_state_sex$table_title,pattern = ",",n = 7 ) )[,2:3] %>%
    dplyr::select(X2) %>%
    dplyr::mutate(state = stringr::str_split(.data$X2, pattern = "-",simplify = TRUE)[1:length(.data$X2)] ) %>%
    dplyr::select(state)

  dta_emp_state_sex_col_2 <- data.frame( stringr::str_split_fixed( dta_emp_state_sex$series,pattern = ";",n = 3 ) )[,1:2] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) )


  dta_emp_state_sex2 <- dta_emp_state_sex %>%
    dplyr::select(date,value,unit,series_type) %>%
    dplyr::bind_cols(dta_emp_state_sex_col_1) %>%
    dplyr::bind_cols(dta_emp_state_sex_col_2) %>%
    dplyr::rename(employment_status = X1, sex = X2)

  dta_emp_state_sex2 %>% dplyr::distinct(state)

  dta_emp_state_sex3 <-
    dta_emp_state_sex2 %>%
    dplyr::mutate(
      employment_status = stringr::str_trim(employment_status, side = "both"),
      state = stringr::str_trim(state, side = "both"),
      series_type = stringr::str_trim(series_type, side = "both"),
      unit = stringr::str_trim(unit, side = "both"),
      sex = stringr::str_trim(sex, side = "both"),
      date = lubridate::as_date(date)
    )

  return_list[[26]] = list("Employment by state and sex 6202.0 tables 4-10a" ,dta_emp_state_sex3 )


  
  
  #############################################################################################################
  # ## hours worked by sex or state
  dta_hours_state_sex<- readabs::read_abs("6202.0", tables = c("19") ) %>%
    dplyr::select(date,series,value,unit,series_type)

  dta_hours_state_sex_col_1 <- data.frame( stringr::str_split_fixed( dta_hours_state_sex$series,pattern = ";",n = 7 ) )[,1:2] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) )


  dta_hours_state_sex2 <- dta_hours_state_sex %>%
    dplyr::select(date,value,unit,series_type) %>%
    dplyr::bind_cols(dta_hours_state_sex_col_1) %>%
    dplyr::rename(employment_status = X1, sex_state = X2)

  dta_hours_state_sex3 <-
    dta_hours_state_sex2 %>%
    dplyr::mutate(
      employment_status = stringr::str_trim(employment_status, side = "both"),
      series_type = stringr::str_trim(series_type, side = "both"),
      unit = stringr::str_trim(unit, side = "both"),
      sex_state = stringr::str_trim(sex_state, side = "both"),
      date = lubridate::as_date(date)
    )

  return_list[[27]] = list("Hours by state or sex 6202.0 tables 19" ,dta_hours_state_sex3 )


  
  
  #############################################################################################################
  ## emp and under emp worked by sex or state
  dta_emp_under_emp_state_sex<- readabs::read_abs("6202.0", tables = c("23") ) %>%
    dplyr::select(date,series,value,unit,series_type)  
  
  dta_emp_under_emp_state_sex_col_1 <- data.frame( stringr::str_split_fixed( dta_emp_under_emp_state_sex$series,pattern = ";",n = 7 ) )[,1:3] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) ) 
  
  
  dta_emp_under_emp_state_sex2 <- dta_emp_under_emp_state_sex %>% 
    dplyr::select(date,value,unit,series_type) %>%
    dplyr::bind_cols(dta_emp_under_emp_state_sex_col_1) %>%
    dplyr::rename(employment_status = X1, sex = X2, state = X3)
  
  dta_emp_under_emp_state_sex3 <- 
    dta_emp_under_emp_state_sex2 %>%
    dplyr::mutate(
      employment_status = stringr::str_trim(employment_status, side = "both"),
      series_type = stringr::str_trim(series_type, side = "both"),
      unit = stringr::str_trim(unit, side = "both"),
      sex = stringr::str_trim(sex, side = "both"),
      state = stringr::str_trim(state, side = "both"),
      date = lubridate::as_date(date)
    ) %>%
    dplyr::filter(
      state != ""
    )%>%
    dplyr::filter(
      sex != ""
    )
  
  return_list[[28]] = list("Emp and under emp by state and sex 6202.0 tables 23" ,dta_emp_under_emp_state_sex3 )

  
  #############################################################################################################
  ## emp and under emp worked by sex or state (FOr economic reasons included)
  dta_emp_under_emp_state_sex_exp<- readabs::read_abs("6202.0", tables = c("24") ) %>%
    dplyr::select(date,series,value,unit,series_type)

  dta_emp_under_emp_state_sex_exp_col_1 <- data.frame( stringr::str_split_fixed(  dta_emp_under_emp_state_sex_exp$series,pattern = ";",n = 7 ) )[,1:3] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) ) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) )



  dta_emp_under_emp_state_sex_exp2 <-   dta_emp_under_emp_state_sex_exp %>%
    dplyr::select(date,value,unit,series_type) %>%
    dplyr::bind_cols(dta_emp_under_emp_state_sex_exp_col_1) %>%
    dplyr::rename(age = X1, employment_status = X2, sex = X3)

  dta_emp_under_emp_state_sex_exp3 <-
    dta_emp_under_emp_state_sex_exp2 %>%
    dplyr::mutate(
      employment_status = stringr::str_trim(employment_status, side = "both"),
      series_type = stringr::str_trim(series_type, side = "both"),
      unit = stringr::str_trim(unit, side = "both"),
      sex = stringr::str_trim(sex, side = "both"),
      age = stringr::str_trim(age, side = "both"),
      date = lubridate::as_date(date)
    ) %>%
    dplyr::filter(
      sex != ""
    )

  return_list[[29]] = list("Emp and under emp by state and sex age expanded 6202.0 tables 24" ,dta_emp_under_emp_state_sex_exp3 )

  
  #############################################################################################################
  # ## emp and under emp worked by sex or state (FOr economic reasons included)
  dta_sex_dur_emp<- readabs::read_abs("6291.0.55.001", tables = c("14b") ) %>%
    dplyr::select(date,series,value,unit,series_type)

  dta_sex_dur_emp_col_1 <- data.frame( stringr::str_split_fixed(  dta_sex_dur_emp$series,pattern = ";",n = 7 ) )[,1:3] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) ) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) )

  dta_sex_dur_emp2 <-   dta_sex_dur_emp %>%
    dplyr::select(date,value,unit,series_type) %>%
    dplyr::bind_cols(dta_sex_dur_emp_col_1) %>%
    dplyr::rename(employment_status = X1, duration = X2, sex = X3)

  dta_emp_under_emp_state_sex_exp3 <-
    dta_emp_under_emp_state_sex_exp2 %>%
    dplyr::mutate(
      employment_status = stringr::str_trim(employment_status, side = "both"),
      series_type = stringr::str_trim(series_type, side = "both"),
      unit = stringr::str_trim(unit, side = "both"),
      sex = stringr::str_trim(sex, side = "both"),
      age = stringr::str_trim(age, side = "both"),
      date = lubridate::as_date(date)
    ) %>%
    dplyr::filter(
      sex != ""
    )

  return_list[[30]] = list("Empy by duration and gender 6291.0 tables 14b" ,dta_emp_under_emp_state_sex_exp3 )



  #---------------------------------LAbour Force Occupation Industry Gender
  indus_occ_gender <- readabs::download_abs_data_cube(catalogue_string = "labour-force-australia-detailed", cube = "EQ09" 
  ) %>%read.xlsx(sheet = 3,startRow = 4,detectDates = TRUE) %>%
    clean_labour_indus_occ_gender()
  
  return_list[[31]] = list("Indus Occ gender TS 6291.0 tables EQ09" ,indus_occ_gender )
  
  #---------------------------------LAbour Force Occupation Industry Gender
  family_gender <- readabs::download_abs_data_cube(catalogue_string = "labour-force-australia-detailed", cube = "FM1"
  ) %>%read.xlsx(sheet = 3,startRow = 4,detectDates = TRUE)

  names(family_gender)[1:4] <- c("date","sex","relationship","state")

  family_gender2 <- family_gender %>%
    pivot_longer(-c(date,sex,relationship,state),names_to = "employment_status", values_to = "value" ) %>%
    mutate(unit = "000") %>%
    mutate(employment_status = gsub(employment_status, pattern = "\\.",replacement = " ")) %>%
    mutate(employment_status = gsub(employment_status, pattern = "(000)",replacement = ""))%>%
    mutate(employment_status = gsub(employment_status, pattern = "\\(",replacement = "")) %>%
    mutate(employment_status = gsub(employment_status, pattern = "\\)",replacement = ""))%>%
    mutate(employment_status = gsub(employment_status, pattern = "\\'",replacement = ""))

  return_list[[32]] = list("Labour Force by relationship" ,family_gender )


  #############################################################################################################
  ## emp and under emp worked by sex or state
  dta_emp_under_emp_indus<- readabs::read_abs("6291.0.55.001", tables = c("19") ) %>%
    dplyr::select(date,series,value,unit,series_type)  
  
  dta_emp_under_emp_indus_col_1 <- data.frame( stringr::str_split_fixed( dta_emp_under_emp_indus$series,pattern = ";",n = 7 ) )[,1:3] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_remove(.x,">")) ) 
  
  dta_emp_under_emp_indus2 <- dta_emp_under_emp_indus %>% 
    dplyr::select(date,value,unit,series_type) %>%
    dplyr::bind_cols(dta_emp_under_emp_indus_col_1) %>%
    dplyr::rename(industry = X1, employment_status = X2, sex = X3)
  
  dta_emp_under_emp_indus3 <- 
    dta_emp_under_emp_indus2 %>%
    dplyr::mutate(
      employment_status = stringr::str_trim(employment_status, side = "both"),
      series_type = stringr::str_trim(series_type, side = "both"),
      unit = stringr::str_trim(unit, side = "both"),
      sex = stringr::str_trim(sex, side = "both"),
      industry = stringr::str_trim(industry, side = "both"),
      date = lubridate::as_date(date)
    ) %>%
    dplyr::filter(
      industry != ""
    )%>%
    dplyr::filter(
      sex != ""
    )
  
  return_list[[33]] = list("Emp and under emp by industry and sex 6202.0 tables 19" ,dta_emp_under_emp_indus3 )
  
  
  #----------------------------------------------------------ALL INDUS OCC
  indus_occ_gender_state <- 
    readabs::download_abs_data_cube(catalogue_string = "labour-force-australia-detailed", cube = "EQ06" 
  ) %>%read.xlsx(sheet = 3,startRow = 4,detectDates = TRUE) %>%
    as_tibble() 
  
  names(indus_occ_gender_state) = c("month_date","sex","state",
                                    "industry","full_time",
                                    "part_time","hours_full_time","hours_part_time")
  
  indus_occ_gender_state2 <- indus_occ_gender_state %>%
    mutate(industry = trimws(industry) ) %>%
    mutate(state = trimws(state))%>%
    mutate(sex = trimws(sex))%>%
    mutate(industry = gsub(x = industry,pattern = "\\&", replacement = "and") )
  
  return_list[[34]]
  
  return(return_list)    
  
}
