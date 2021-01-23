local_duration <- function(.data){
  
  names(.data) <- c("date","duration","state","num_unemp",
                  "sum_of_weeks","people_looked_for_full_work",
                  "people_looked_for_part_work") 
   dat2 <- .data %>%
     mutate(
      date = as_date(date),
      duration = trimws(duration),
      state = trimws(state),
      num_unemp = as.numeric(num_unemp),
      sum_of_weeks = as.numeric(sum_of_weeks),
      people_looked_for_full_work = as.numeric(people_looked_for_full_work),
      people_looked_for_part_work = as.numeric(people_looked_for_part_work),
      units = "000"
    )
   
   return(dat2)
  
}

local_NILF <- function(.path = .data){
  
  names(.data) <- c("date","sex","age","reason","value") 
  dat2 <- .data %>%
    mutate(
      date = as_date(date),
      sex = trimws(sex),
      age = trimws(age),
      reason = trimws(reason),
      value = as.numeric(value)
    )
  
  return(dat2)
  
}

#-----------------------------------------------------------------------------------------------------

#------------------------------STP DATA SA4

clean_stp_sa4 <- function(.data){
  
  name_data <- names(.data) %>% as.character()
  
  names(.data) = name_data
  
  returned_data <- .data %>%
    as_tibble() %>%
    mutate(State.or.Territory = stringr::str_extract(
      State.or.Territory, 
      "NSW|VIC|QLD|SA|NT|WA"
    )) %>%
    mutate(Statistical.Area.4 = stringr::str_remove(string = Statistical.Area.4,pattern = "[[:digit:]]+") ) %>%
    mutate(Statistical.Area.4 = stringr::str_remove(string = Statistical.Area.4,pattern = "\\.") ) %>%
    mutate(Statistical.Area.3 = stringr::str_remove(string = Statistical.Area.3,pattern = "[[:digit:]]+") ) %>%
    mutate(Statistical.Area.3 = stringr::str_remove(string = Statistical.Area.3,pattern = "\\.") ) %>%
    janitor::clean_names() %>%
    mutate_all( as.character) %>%
    pivot_longer(-c(state_or_territory,statistical_area_4,statistical_area_3),
                 values_to = "value", names_to = "date") %>%
    mutate(date = stringr::str_remove(date,"x")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
}

#------------------------------STP DATA Industry

clean_stp_indus <- function(.data){
  
  returned_data <- .data %>%
    as_tibble() %>%
    mutate(Industry = stringr::str_remove(string = Industry,pattern = "[[:digit:]]+") ) %>%
    mutate(Industry = stringr::str_remove(string = Industry,pattern = "\\.") ) %>%
    mutate(`Subdivision` = stringr::str_remove(string = `Subdivision`,pattern = "[[:digit:]]+") ) %>%
    mutate(`Subdivision` = stringr::str_remove(string = `Subdivision`,pattern = "\\.") ) %>%
    janitor::clean_names() %>%
    mutate_all( as.character) %>%
    pivot_longer(-c(industry,subdivision),
                 values_to = "value", names_to = "date") %>%
    mutate(date = stringr::str_remove(date,"x")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
}
dd
#------------------------------STP DATA NATIONAL

clean_stp_national <- function(.data){
  sdf
  dat <- .data %>%
    as_tibble() %>%
    mutate(Industry.division = stringr::str_remove(string = Industry.division,pattern = "[[:digit:]]+") ) %>%
    mutate(Industry.division = stringr::str_remove(string = Industry.division,pattern = "\\.") ) %>%
    mutate(`State.or.Territory` = stringr::str_remove(string = `State.or.Territory`,pattern = "[[:digit:]]+") ) %>%
    mutate(`State.or.Territory` = stringr::str_remove(string = `State.or.Territory`,pattern = "\\.") ) %>%
    janitor::clean_names() %>%
    mutate_all( as.character) %>%
    pivot_longer(-c(industry_division,state_or_territory,sex,age_group),
                 values_to = "value", names_to = "date") %>%
    mutate(date = stringr::str_remove(date,"x")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(dat)
  
}

#------------------------------STP DATA Industry

clean_stp_indus_subdiv <- function(.data){
  
  returned_data <- .data %>%
    as_tibble() %>%
    mutate(Industry = stringr::str_remove(string = Industry,pattern = "[[:digit:]]+") ) %>%
    mutate(Industry = stringr::str_remove(string = Industry,pattern = "\\.") ) %>%
    mutate(`Subdivision` = stringr::str_remove(string = `Subdivision`,pattern = "[[:digit:]]+") ) %>%
    mutate(`Subdivision` = stringr::str_remove(string = `Subdivision`,pattern = "\\.") ) %>%
    janitor::clean_names() %>%
    mutate_all( as.character) %>%
    pivot_longer(-c(industry,subdivision),
                 values_to = "value", names_to = "date") %>%
    mutate(date = stringr::str_remove(date,"x")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
}


#-------------------------------------------------

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
