library(tidyverse)
library(cah)
library(covidpmc)
library(readxl)

# phase_2 ---- 

get_jobkeeper_phase_2 <- function(folder,
                                  unit_of_analysis # "industries" or "persons"
) {
  jk_2 <- list.files(path = folder,
                     pattern = "xlsx",
                     full.names = TRUE)
  jk_2 %>% 
    map_dfr(read_jobkeeper_phase_2,
            unit_of_analysis = unit_of_analysis)
  
}


read_jobkeeper_phase_2 <- function(filepath = jk_2[1],
                                   unit_of_analysis) {
  
  # Read metadata
  file_date <- read_excel(filepath,
                          sheet = "t1") %>% 
    filter(Title == "Timeframe") %>% 
    pluck("JobKeeper Payment extension statistics") %>% 
    str_extract("[0-9].*[0-9]") %>% 
    lubridate::dmy()
  
  # Extract industry breakdown
  jk_industry <- read_excel(filepath,
                            sheet = "t2",
                            skip = 2) %>% 
    select(-Total) %>% 
    filter(Industry != "Total") %>% 
    rename(industry = Industry) %>% 
    pivot_longer(ACT:last_col(),
                 names_to = "state",
                 values_to = "businesses") %>% 
    mutate(date = file_date)
  
  
  # Extract persons breakdown
  jk_persons <- read_excel(filepath,
                           sheet = "t8",
                           skip = 2,
                           col_names = c("state",
                                         "persons_tier_1",
                                         "share_tier_1",
                                         "persons_tier_2",
                                         "share_tier_2",
                                         "persons_total",
                                         "share_total")) %>% 
    select(state,
           persons_tier_1,
           persons_tier_2) %>% 
    drop_na(state) %>% 
    filter(state != "Total") %>% 
    mutate(date = file_date,
           across(starts_with("persons"),
                  as.numeric))
  
  # Return unit of analysis
  df <- switch (unit_of_analysis,
                "industries" = jk_industry,
                "persons" = jk_persons)
  return(df)
  
}



# phase 1 ----

path_jk_p1 <- covidpmc::onedrive("data/raw/ato/jobkeeper")

get_jobkeeper_phase_1 <- function(folder = covidpmc::onedrive("data/raw/ato/jobkeeper")) {
  jk_1 <- list.files(path = path,
                     pattern = "xlsx",
                     full.names = TRUE) %>% 
    purrr::keep(stringr::str_detect,
                pattern = "September 2020",
                negate = TRUE)
  
  
  jk <- jk_1 %>% 
    map_dfr(read_jobkeeper_phase_1)
  
  return(jk)
}


read_jobkeeper_phase_1 <- function(filepath) {
  
  message(filepath)
  # Table 1, industry breakdown
  # August files have data for August
  if (stringr::str_detect(filepath,
                          "August 2020")) {
    kj_1_industry <- read_excel(filepath,
                                sheet = "Table 1") %>% 
      janitor::clean_names() %>% 
      rename(state = starts_with("state"),
             businesses = starts_with("no_of_employers"),
             employees = starts_with("no_of_employees")) %>% 
      mutate(month = lubridate::ym("2020 August")) %>% 
      pivot_longer(matches("employees|businesses"),
                   names_to = "units") %>% 
      relocate(month, units, value, state) %>% 
      drop_na(state)
  } else {
    kj_1_industry <- read_excel(filepath,
                                sheet = "Table 1") %>% 
      janitor::clean_names() %>% 
      # select(-starts_with("industry"),
      #        -matches("id|code")) %>%
      rename(state = matches("x1|state"),
             businesses_april = matches("(?=.*business)(?=.*april)", perl = TRUE),
             employees_april = matches("(?=.*employees)(?=.*april)", perl = TRUE),
             businesses_may = matches("(?=.*business)(?=.*may)", perl = TRUE),
             employees_may = matches("(?=.*employees)(?=.*may)", perl = TRUE),
             businesses_june = matches("(?=.*business)(?=.*june)", perl = TRUE),
             employees_june = matches("(?=.*employees)(?=.*june)", perl = TRUE),
             businesses_july = matches("(?=.*business)(?=.*july)", perl = TRUE),
             employees_july = matches("(?=.*employees)(?=.*july)", perl = TRUE),
             businesses_september = matches("(?=.*business)(?=.*september)", perl = TRUE),
             employees_september = matches("(?=.*employees)(?=.*september)", perl = TRUE)
      ) %>% 
      pivot_longer(matches("employees|businesses"),
                   names_to = "units") %>% 
      separate(units,
               into = c("units", "month")) %>% 
      mutate(month = lubridate::ym(paste0(2020, month))) %>% 
      relocate(month, units, value, state) %>% 
      drop_na(state)
    
  }
  
  # industry
  return(kj_1_industry)
  
  
}



get_jobkeeper_phase_2 <- function(folder = covidpmc::onedrive("data/raw/treasury/jobkeeper_phase_2")){
  
  jk_2 <- list.files(path = folder,
                     pattern = "xlsx",
                     full.names = TRUE) %>% 
    purrr::keep(stringr::str_detect,
                pattern = "September 2020",
                negate = TRUE)
  
  
  jk <- jk_2 %>% 
    map_dfr(clean_jk_p2 )
  
  return(jk)
  
  
}

clean_jk_p2 <- function(file_name = jk_2[1]){
  
  get_file_name_only <- str_split(file_name,
                          pattern = "/")[[1]][2]
  
  get_date_from_file <- str_extract(get_file_name_only,"[[:digit:]]+")
  
  convert_to_date <- as_date(get_date_from_file)
  
  
  dta <- read_excel(file_name,
                          sheet = "t8") %>%
    janitor::clean_names("snake") %>%
    mutate(
      
      total_tier_1 = str_split(total_tier_1_tier_2,pattern = " ",simplify = TRUE,n = 2)[,1],
      total_tier_2 = str_split(total_tier_1_tier_2,pattern = " ",simplify = TRUE,n = 2)[,2],
      
    ) %>%
    dplyr::select(-total_tier_1_tier_2) %>%
    mutate(across(!starts_with("industry"),~str_remove(.x,","))) %>%
    mutate(across(!starts_with("industry"),~str_remove(.x,","))) %>%
    mutate(across(!starts_with("industry"),as.numeric)) %>%
    dplyr::select(-other_tier_1_tier_2) 

  state_vars <- 
    DBI::dbGetQuery(conn = db_con_r,
                    statement = "SELECT * FROM brief_v2.jurisdiction") %>%
    dplyr::select(jurisdiction_id) 
 
  state_vars = paste( state_vars[,1],collapse  = "|")
  
  dta_long <- dta %>%
    pivot_longer(-industry, 
                 names_to = "column_names", values_to = "value" ) %>%
    mutate(date = convert_to_date) %>%
    mutate(
      jurisdiction_id = 
        ifelse( str_detect(column_names,pattern = "total") == TRUE,
                "au",
                str_extract(column_names, pattern = state_vars) )
    ) %>%
    mutate(
      tier = str_extract(column_names,pattern ="tier_1|tier_2" )
    ) %>%
    dplyr::select(-column_names)
  
return(dta_long)

}
