
detailed_plot_forecast <- function(.data){
  
dat <- .data
#---------------------hours plots
  
  #-----------------------scatter_plot
  dat2 <- .data %>%
    group_by(sex, state,industry,employment_or_hours) %>%
    mutate(
      diff_value = value - lag(value, 1, order_by = month_date)
    ) %>%
    mutate(
      pre_covid = mean(ifelse(month_date < "2020-03-01",diff_value,NA), na.rm = TRUE),
      pre_covid_date = ifelse(
       value == max( ifelse(month_date <= "2020-03-01",value,NA), na.rm = TRUE ),
       as.character(month_date), NA
      )
    ) %>%
    ungroup() %>%
    fill(pre_covid_date,.direction = "down") %>%
    group_by(sex, state,industry,employment_or_hours) %>%
    mutate(
      peak_value_pre = max(
        ifelse(month_date >= "2019-11-01",value,NA), na.rm = TRUE
      )
    ) %>%
    mutate(
      deviation = (value -  peak_value_pre)
    ) %>%
    filter(
      month_date > "2020-02-01"
    ) %>%
    mutate(
      value_at_max = ifelse(month_date == max(month_date), deviation,NA)
    ) 

  dat3 <- 
    dat2 %>%
    ungroup() %>%
    mutate(
      full_part = ifelse(str_detect(employment_or_hours,"full")== TRUE,"full","part")
    ) %>%
    mutate(
      hours_or_people = ifelse(str_detect(employment_or_hours,"hours")== TRUE,"hours","people")
    ) %>%
    select(-employment_or_hours) %>%
    select(month_date, industry, sex,state,full_part, hours_or_people,deviation) %>%
    pivot_wider(c(month_date, industry, sex,state,full_part),
                values_from = deviation, names_from = hours_or_people)
  
  plot_1_dat <- dat3 %>%
    mutate(
      industry = str_remove(industry,pattern = "[[:digit:]]+")
    ) %>%
    mutate(industry = trimws(industry))%>%
    group_by(state,month_date,sex,full_part,industry) %>%
    summarise(hours = sum(hours,na.rm = TRUE),
              people = sum(people,na.rm = TRUE) ) %>%
    ungroup() %>%
    mutate(
      max_value_hours = ifelse(month_date == max(month_date), hours, NA),
      max_value_people = ifelse(month_date == max(month_date), people, NA)
    ) %>%
    mutate(
      label_x =
        ifelse(max_value_people < -5, 
               paste0(industry,"\nJobs Unrecovered:",round(people),"\nHours Unrecovered:",round(hours)),
               NA)
    ) %>%
    mutate(
      full_part = ifelse(full_part == "full", "Full Time Employed","Part Time Employed" )
    )
  
  total_jobs_still_lost <- plot_1_dat %>%
    ungroup() %>%
    filter(month_date == max(month_date)) %>%
    filter(max_value_people < -5) %>%
    summarise(
      max_value_people = sum(max_value_people,na.rm = TRUE)
    )
      
  
  
  plot_1_dat %>%
    ggplot() + theme_minimal() + 
    geom_point(aes(x = max_value_people, y = max_value_hours ),
               show.legend = FALSE, alpha = 0.5, color = "#003366") +
    geom_hline(yintercept = -100, linetype = "dashed",size = 0.7) +
    geom_vline(xintercept = -5, linetype = "dashed",size = 0.7) +
    geom_text(aes(y = -250, x = -17,label = "At risk cohorts"),show.legend = FALSE,color = "black" ) +
    facet_wrap(sex~full_part) +
    xlab("Remaining Unrecovered Jobs") +
    ylab("Remaining Unrecovered Hours") 

  
  plot_2_dat <- plot_1_dat %>%
    filter(!is.na(label_x))
  
}  
