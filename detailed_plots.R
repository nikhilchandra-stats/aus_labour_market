# load("abs_data.RData")
# 
#.data <- abs_dat[[31]][[2]]

detailed_plot_forecast <- function(.data){
 
  dat_full_time <- .data %>%
    filter(measure %in% c("full_time_people","part_time_people") ) %>% 
    group_by(
      sex, industry, occupation, measure
    ) %>%
    mutate(diff = value - lag(value,4 ,order_by = date))
  
  value_lost_during_covid <- dat_full_time %>%
    filter( date > "2020-02-01")
  
  value_lost_during_covid %>%
    group_by(occupation,industry ,measure) %>%
    mutate(value = cumsum(diff) ) %>%
    mutate(occupation = str_wrap(occupation,10)) %>%
    cah_plot(aes(x = date, 
                 fill = occupation, 
                 y = value)) +
    geom_col() + 
    # scale_fill_gradient(low = "red",high = "white" ) +
    facet_wrap(.~industry,scales = "free") + 
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 6)
    )w
    
  
  dat_full_time %>%    
    group_by(date,occupation ,measure) %>%
    filter(!is.na(diff)) %>%
    summarise(value = sum(value,na.rm = TRUE)) %>%
    rename(value = value) %>%
    mutate(occupation = str_wrap(occupation,10)) %>%
    cah_plot(aes(x = date, 
                 color = occupation, 
                 y = value)) +
    geom_line(se = FALSE) + 
    # scale_fill_gradient(low = "red",high = "white" ) +
    facet_wrap(measure~occupation,scales = "free") + 
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 6)
    )
    
    
  return(list(risk_matrix_plot,bar_plot))
  
}  
