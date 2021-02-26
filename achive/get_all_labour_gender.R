
#------------------------------Labour DATA Industry ANZSCO

clean_labour_occupation_gender <- function(.data){
  
  returned_data1 <- .data %>%
    as_tibble() 
  
  names(returned_data1) <- c("date","sex","state","occupation","full_time_people","part_time_people"
                             ,"hours_full_time","hours_part_time")
  
  returned_data <- returned_data1 %>%
    mutate(occupation = stringr::str_remove(string = occupation,pattern = "[[:digit:]]+") ) %>%
    mutate(occupation = stringr::str_remove(string = occupation,pattern = "\\.") ) %>%
    pivot_longer(-c(date,sex,state,occupation), values_to = "value", names_to = "measure") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
}

#------------------------------Labour DATA Industry ANZIC - Longer

clean_labour_industry_GCCSA_gender <- function(.data){
  
  returned_data1 <- .data %>%
    as_tibble() 
  
  names(returned_data1) <- c("date","sex","GCCSA","industry","full_time_people","part_time_people","hours_full_time","hours_part_time")
  
  returned_data <- returned_data1 %>%
    mutate(industry = stringr::str_remove(string = industry,pattern = "[[:digit:]]+") ) %>%
    mutate(industry = stringr::str_remove(string = industry,pattern = "\\.") ) %>%
    pivot_longer(-c(date,sex,GCCSA,industry), values_to = "value", names_to = "measure") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
}

#------------------------------STP DATA Industry ANZIC

clean_labour_indus_occ_gender <- function(.data){
  
  returned_data1 <- .data %>%
    as_tibble() 
  
  names(returned_data1) <- c("date","sex","industry","occupation",
                             "full_time_people","part_time_people","hours_full_time","hours_part_time")
  
  returned_data <- returned_data1 %>%
    mutate(industry = stringr::str_remove(string = industry,pattern = "[[:digit:]]+") ) %>%
    mutate(industry = stringr::str_remove(string = industry,pattern = "\\.") ) %>%
    pivot_longer(-c(date,sex,industry,occupation), values_to = "value", names_to = "measure") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
}

#---------------------------------------------------Get All Data
get_data_local_gender <- function(){
  
  gccsa_indus_gender <- read.xlsx("indus_GCCSA_gender.xlsx",sheet = 3,startRow = 4,detectDates = TRUE)  %>%
    clean_labour_industry_GCCSA_gender()
  
  state_occ_gender <- read.xlsx("industry_state_gender.xlsx",sheet = 3,startRow = 4,detectDates = TRUE)  %>%
    clean_labour_occupation_gender()
  
  occ_ind_gender <- read.xlsx("inuds_occ_gender.xlsx",sheet = 3,startRow = 4,detectDates = TRUE)  %>%
    clean_labour_indus_occ_gender()
  
  return(list(occ_ind_gender))
  
}
