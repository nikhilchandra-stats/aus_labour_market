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
    mutate(`Sub-division` = stringr::str_remove(string = `Sub-division`,pattern = "[[:digit:]]+") ) %>%
    mutate(`Sub-division` = stringr::str_remove(string = `Sub-division`,pattern = "\\.") ) %>%
    janitor::clean_names() %>%
    mutate_all( as.character) %>%
    pivot_longer(-c(industry,sub_division),
                 values_to = "value", names_to = "date") %>%
    mutate(date = stringr::str_remove(date,"x")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date(date))
  
  return(returned_data)
  
}

