get_abs_and_saveRDS <- function(){
  
  source("data cleaning/ABS_eco_data2.R")
  source("data cleaning/clean_data.R")
  
  abs_dat <- eco_tables_abs()
  current_date <- as_date(now())
  saveRDS(abs_dat,file = paste0("new_abs_data_",current_date,".Rdata") )
  
}



# board_register_kaggle(token = "data cleaning/kaggle.json")
# 
# abs_dat %>%  
#   pin(board = "kaggle" )
# 
# test <- pin
