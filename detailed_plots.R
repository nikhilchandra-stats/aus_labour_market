
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
    group_by(sex,full_part) %>%
    filter(month_date == max(month_date)) %>%
    summarise(
      people_lost = sum(max_value_people,na.rm = TRUE)
    ) %>%
    mutate(
      people_lost = abs(people_lost)
    )
  
  at_risk_cohorts <- plot_1_dat %>%
    ungroup() %>%
    group_by(sex,full_part) %>%
    filter(month_date == max(month_date)) %>%
    summarise(
      risk_cohorts = sum( ifelse(max_value_people < -5,max_value_people,NA), na.rm = TRUE  )/
        sum(max_value_people,na.rm = TRUE)
    ) %>%
    mutate(
      risk_cohorts = round( abs(risk_cohorts),2)*100
    )
  
  plot_2_dat <-
    plot_1_dat %>%
    left_join(total_jobs_still_lost,by = c("sex","full_part")) %>%
    left_join(at_risk_cohorts,by = c("sex","full_part") ) %>%
    mutate(
      label_main = paste0(risk_cohorts,"% cohorts are\nstill unrecovered\n",round(people_lost)," thousand"," jobs still\nnot recovered")
    ) %>%
    mutate(
      label_main = ifelse(month_date == max(month_date),label_main,NA)
    )
  
  plot_2_dat %>%
    ggplot() + 
    geom_point(aes(x = max_value_people, y = max_value_hours ),
               show.legend = FALSE, alpha = 0.5, color = "#003366") +
    geom_hline(yintercept = -100, linetype = "dashed",size = 0.7) +
    geom_vline(xintercept = -5, linetype = "dashed",size = 0.7) +
    geom_text(aes(y = -350, x = -20,
                  label = label_main ),
              show.legend = FALSE,color = "darkblue",size = 3 ) +
    theme_minimal() + 
    facet_wrap(sex~full_part) +
    xlab("Remaining Unrecovered Jobs (000)") +
    ylab("Remaining Unrecovered Hours (000)") 

  
  plot_3_dat <- plot_2_dat %>%
    filter(max_value_people < -6)
  
  cols_sex <- c("Males" = "#003366", "Females" = "#ff6600")
  
  
  plot_3_dat %>%
    filter(full_part == "Full Time Employed") %>%
    mutate(state = str_wrap(state,10)) %>%
    ggplot( aes(y = industry, x = round(abs(max_value_people),1), fill = sex  )  ) +
    theme_minimal() + 
    geom_col( )+
    geom_text(aes(label = round(abs(max_value_people),1) ),
              size = 4, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(axis.text.y = element_text(size = 7), axis.title.y = element_blank())+
    facet_grid(vars(state),scales = "free",space = "free") +
    xlab("Remaining Unrecovered Jobs (000)")
  
}  
