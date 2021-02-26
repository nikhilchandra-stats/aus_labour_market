# Plot Summary
plot_summary_1 <- function(.data){
  
  
  emp_dta_underemp_emp <- .data %>%
    filter(
      series_type == "Seasonally Adjusted"
    ) %>%
    filter(unit == "000") %>%
    filter(
      date >= (max(date) - years(1))
    ) %>%
    pivot_wider(c(date,unit,series_type,employment_status,state), names_from = sex, values_from = value) %>%
    mutate(
      perc = Females/Persons
    )
  
  
  
  cols_sex <- c("Males" = "#003366", "Females" = "#ff6600")
  
  emp_dta_underemp_emp %>%
    filter(date == max(date)) %>%
    filter(state != "Australia") %>%
    select(-Males,-Females) %>%
    mutate(Males = 1 - perc) %>%
    mutate(Females = perc) %>%
    select(-perc) %>%
    pivot_longer(-c(date,unit,series_type,employment_status,state), names_to = "sex", values_to = "value") %>%
    filter(sex != "Persons") %>%
    mutate(value = round(value,digits = 2) ) %>%
    ggplot(aes(x = employment_status, y = value, fill = sex, 
               label = paste0(100*value,"%") )) + 
    theme_minimal() + 
    geom_col( color = "black") + 
    geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    theme(
      
      strip.text = element_text(face = "bold",color = "#003366", size = 11)
      
    )+
    coord_flip() +
    facet_wrap(~state,scales = "free",) +
    theme(axis.title  = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold",color = "#003366"),
          axis.text.y = element_text(size = 10)
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) 
  
  
}
 #------Plot Industry
plot_summary_indus <- function(.data, 
                               people_hours = "people"){
  
  
  if(people_hours == "people"){
    full_part_time <- c("full_time_people","part_time_people")
  }else{
    full_part_time <- c("hours_full_time","hours_part_time")
  }
  
  indus_labour_analysis <-  .data%>%
    group_by(industry,date,sex,measure) %>%
    summarise(value = sum(value,na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(date = as_date(date)) %>%
    filter(date == max(date))  %>%
    pivot_wider(c(date,industry,measure), names_from = sex, values_from = value) %>%
    mutate(perc = Females/(Females + Males)) %>%
    mutate(perc_male = 1 - perc) %>%
    mutate(Males = perc_male, Females = perc) %>%
    dplyr::select(-perc,-perc_male) %>%
    filter(measure %in% full_part_time) %>%
    pivot_longer(-c(date,industry,measure), values_to = "value", names_to = "sex") %>%
    mutate(value = round(value,2)) %>%
    mutate(
      measure = case_when(
        measure == "full_time_people" ~ "Full Time Employed",
        measure == "part_time_people" ~ "Part Time Employed",
        measure == "hours_full_time" ~ "Full Time Employed (Hours Worked)",
        measure == "hours_part_time" ~ "Part Time Employed (Hours Worked)",
        
      )
    )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  indus_labour_analysis %>%
    ggplot(aes(x = industry, y = value, fill = sex, 
               label = paste0(100*value,"%") )) + 
    theme_minimal() + 
    geom_col( color = "black") + 
    geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    theme(
      
      strip.text = element_text(face = "bold",color = "#003366", size = 11)
      
    )+
    coord_flip() +
    facet_wrap(~measure,scales = "free",nrow = 2,ncol = 1) +
    theme(axis.title  = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold",color = "#003366"),
          axis.text.y = element_text(size = 10)
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) + 
    labs(
      title = "Gender ratios of people employed by industry"
    ) + 
    theme(
      title = element_text(face = "bold",colour = "#003366",)
    )
  
  
  
}

#---------------------------------------------------
downloaded_data_national <- function(.data){
  emp_dta_underemp_emp <- .data %>%
    filter(
      series_type == "Seasonally Adjusted"
    ) %>%
    filter(unit == "000") %>%
    pivot_wider(c(date,unit,series_type,employment_status,state), names_from = sex, values_from = value) %>%
    mutate(
      perc = Females/Persons
    )
  
  
  
  cols_sex <- c("Males" = "#003366", "Females" = "#ff6600")
  
  emp_dta_underemp_emp %>%
    filter(state != "Australia") %>%
    select(-Males,-Females) %>%
    mutate(Males = 1 - perc) %>%
    mutate(Females = perc) %>%
    select(-perc) 
  
  return(emp_dta_underemp_emp)
  
}

#---------------------------------------------------------------
plot_summary_indus_download <- function(.data, 
                                        people_hours = "people"){
  
  
  if(people_hours == "people"){
    full_part_time <- c("full_time_people","part_time_people")
  }else{
    full_part_time <- c("hours_full_time","hours_part_time")
  }
  
  indus_labour_analysis <-  .data%>%
    group_by(industry,date,sex,measure) %>%
    summarise(value = sum(value,na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(date = as_date(date)) %>%
    pivot_wider(c(date,industry,measure), names_from = sex, values_from = value) %>%
    mutate(perc = Females/(Females + Males)) %>%
    mutate(perc_male = 1 - perc) %>%
    mutate(Males = perc_male, Females = perc) %>%
    dplyr::select(-perc,-perc_male) %>%
    filter(measure %in% full_part_time) %>%
    pivot_longer(-c(date,industry,measure), values_to = "value", names_to = "sex") %>%
    mutate(value = round(value,2)) %>%
    mutate(
      measure = case_when(
        measure == "full_time_people" ~ "Full Time Employed",
        measure == "part_time_people" ~ "Part Time Employed",
        measure == "hours_full_time" ~ "Full Time Employed (Hours Worked)",
        measure == "hours_part_time" ~ "Part Time Employed (Hours Worked)",
        
      )
    )
  
  return(indus_labour_analysis)
  
}
