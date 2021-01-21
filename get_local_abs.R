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

#------------------------------STP DATA NATIONAL

clean_stp_national <- function(.data){
  
  returned_data <- .data %>%
    as_tibble() %>%
    mutate(Industry.division = stringr::str_remove(string = Industry.division,pattern = "[[:digit:]]+") ) %>%
    mutate(Industry.division = stringr::str_remove(string = Industry.division,pattern = "\\.") ) %>%
    mutate(`State.or.Territory` = stringr::str_remove(string = `State.or.Territory`,pattern = "[[:digit:]]+") ) %>%
    mutate(`State.or.Territory` = stringr::str_remove(string = `State.or.Territory`,pattern = "\\.") ) %>%
    janitor::clean_names() %>%
    mutate_all( as.character) %>%
    pivot_longer(-c(industry_division,state_or_territory),
                 values_to = "value", names_to = "date") %>%
    mutate(date = stringr::str_remove(date,"x")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
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

