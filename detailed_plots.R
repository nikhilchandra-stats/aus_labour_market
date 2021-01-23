.data <- returned_data[[37]] %>%
  pivot_longer(-c(month_date, sex, state,industry),values_to = "value", names_to = "employment_or_hours")

detailed_plot_forecast <- function(.data){
  
dat <- .data
#---------------------hours plots
  
  #-----------------------scatter_plot
  dat2 <- .data %>%
    filter( grepl(employment_or_hours,pattern = "hours") == TRUE  ) %>%
    group_by(sex, state,industry,employment_or_hours) %>%
    mutate(
      diff_value = value - lag(value, 3, order_by = month_date)
    ) %>%
    mutate(
      pre_covid = mean(ifelse(month_date < "2020-03-01",diff_value,NA), na.rm = TRUE)
    ) %>%
    mutate(
      hours_lost_at_peak = min(
        ifelse(month_date > "2020-02-01",diff_value,NA), na.rm = TRUE
      )
    )

  plot_dat <- dat2 %>%
    filter(
      month_date > "2020-02-01"
    ) %>%
    mutate(
      max_point = ifelse(month_date == max(month_date),diff_value,NA)
    )
  
  
  plot_dat %>%
    ungroup() %>%
    filter(state == "New South Wales" ) %>%
    ggplot(aes(x = month_date, y = max_point, color = industry)) +
    geom_point(show.legend = FALSE) + theme_minimal()
  
    

}  