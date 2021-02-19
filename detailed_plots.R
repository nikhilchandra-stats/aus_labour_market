# load("abs_data.RData")
# 
.data <- abs_dat[[31]][[2]]

detailed_plot_forecast <- function(.data){
 
  dat_full_time <- .data %>%
    filter(measure %in% c("full_time_people","part_time_people") ) %>% 
    group_by(
      sex, industry, occupation, measure
    ) %>%
    mutate(diff = (value - lag(value,1 ,order_by = date))/lag(value,1 ,order_by = date) )
  
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
    )
    
  
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
    
  
  dat <- dat_full_time %>%    
    group_by(date,occupation ,measure) %>%
    filter(!is.na(diff)) %>%
    summarise(value = sum(value,na.rm = TRUE)) %>%
    group_by(occupation ,measure) %>%
    mutate(
      qtr_qtr = value/lag(value) -1 
    )
  
  
  
  
  dat %>%
    # filter(value <10) %>%
    # filter(value > -10) %>%
    cah_plot(aes( x = qtr_qtr, fill = measure, linetype = measure)) +
    geom_density(alpha = 0.25) +
    facet_wrap(.~occupation, scales = "free")
  
  dat %>%
    # filter(value <10) %>%
    # filter(value > -10) %>%
    cah_plot(aes( y = qtr_qtr,x = date, fill = measure, linetype = measure)) +
    geom_line() +
    facet_wrap(.~occupation, scales = "free")
  
    
  return(list(risk_matrix_plot,bar_plot))
  
}  


get_qtr_qtr_growth_dat <- function(.data, indus_or_occ = "industry" ){
  
  
  dat <- .data %>% 
    group_by(
      sex, industry, occupation, measure
    ) %>%
    mutate(diff = (value - lag(value,1 ,order_by = date))/lag(value,1 ,order_by = date) ) %>%
    ungroup()
  
  expression_get_dat <- 
    glue::glue("dat %>% 
                 select(date,{indus_or_occ},value,measure)")
  
  return <- eval( parse(text = expression_get_dat) )
  
  
}

.data <- abs_dat[[31]][[2]] %>%
  get_qtr_qtr_growth_dat()

bayesian_eval_qtr_grth <- 
  function(.data, indus_or_occ = "industry", measure_chosen = "full_time_people"){
  
    dat <- .data %>%
      filter(eval(parse(text = glue::glue("measure == '{measure_chosen}'") ) ) ) %>%
      split(eval(parse(text = glue::glue(".${indus_or_occ}") ))) %>%
      purrr::map(~gibbs_normal(.,"value",20000) %>%
                   sample_from_bayesian_normal(., samples_drawn = 20000) %>%
                   mutate(industry = 
                          eval( parse(text = glue::glue(".x${indus_or_occ}[1]") ) ) 
                          ) %>%
                   mutate(
                     measure = measure_chosen )
                   )
                  
return(dat)
    
}
