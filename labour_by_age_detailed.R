#------Not working for economic reasons

plot_by_age <- function(.data){

  plot_dat <- 
    .data %>% 
    filter(unit == "000") %>%
    pivot_wider(c(date,unit,series_type,age,employment_status), values_from = value, names_from = sex) %>%
    mutate(
      Males = round(Males/Persons,2),
      Females = round(Females/Persons,2)
    ) %>%
    select(
      -Persons
    ) %>%
    pivot_longer(
      -c(date,unit,series_type,age,employment_status), values_to = "value", names_to = "sex"
    ) %>%
    filter(
      date == max(date)
    ) %>%
    filter(age != "15-64 years") %>%
    filter(age != "55 years and over") 
  
  cols_sex <- c("Males" = "#003366", "Females" = "#ff6600")
  
  
  plot_dat %>% 
    ggplot(aes(x = employment_status, y = value, fill = sex, 
             label = paste0(100*value,"%") )) + 
    theme_minimal() + 
    geom_col( color = "black") + 
    geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    theme(
      
      strip.text = element_text(face = "bold",color = "#003366", size = 12)
      
    )+
    coord_flip() +
    facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    theme(axis.title  = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold",color = "#003366")
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) + 
    labs(
      title = "Gender ratios of various employment status by age"
    ) + 
    theme(
      title = element_text(face = "bold",colour = "#003366"),
      axis.text.y = element_text(size = 11)
    )
  
}  
