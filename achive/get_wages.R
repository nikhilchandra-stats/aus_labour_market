
read_in_wages_local <- function(path_file = "wages.xls"){
  
  dat <- xlsx::read.xlsx(file = path_file,sheetIndex = 2,startRow = 5 ) %>%
    filter( !is.na(Victoria) )
  names(dat)[1] <- c("date")
  
  dat2 <- dat %>%
    pivot_longer(-c(date,sex,units,employment_status), names_to = "state", values_to = "value") %>%
    mutate(state = str_replace(string = state,pattern = "\\.",replacement = " ") )%>%
    mutate(state = str_replace(string = state,pattern = "\\.",replacement = " ") ) %>%
    mutate(state = str_replace(string = state,pattern = "\\.",replacement = " ") ) %>%
    mutate(state = str_replace(string = state,pattern = "\\.",replacement = " ") )
  
}

read_datalab_output <- function(){
  
  coefs_logit <- read_csv("model_coeffecients.csv")
  coefs_duration <- read_csv("duration_model.csv")
  lack_of_hours_model <- read_csv("lack_of_hours_model.csv")
  
  return(list(coefs_logit,coefs_duration,lack_of_hours_model))
  
}

read_educational_sex <- function(){
  
  dat_test <- read.xlsx("edu_attain_sex.xlsx",sheet = 7,startRow = 1 ) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(date = as_date( paste0(date,"-01-01") ) )
  
  return(dat_test)
  
}
