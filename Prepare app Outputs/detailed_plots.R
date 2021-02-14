# load("abs_data.RData")
# 
.data <- abs_dat[[31]][[2]]

detailed_plot_forecast <- function(.data){
 
  dat_full_time <- .data %>%
    filter(measure %in% c("full_time_people","part_time_people") ) %>% 
    group_by(
      sex, industry, occupation, measure
    ) %>%
    mutate(diff = value - lag(value,1 ,order_by = date) )
  
  value_lost_during_covid <- dat_full_time %>%
    filter( date > "2020-02-01")%>%
    group_by(occupation,industry ,measure) %>%
    mutate(value2 = cumsum(diff) ) %>%
    mutate(covid = "covid") %>%
    mutate(quarter_date = as_date(paste0("2020-",month(date),"-01")) ) %>%
    mutate(
      index = month(date)
    )
  
  value_lost_pre_covid <- dat_full_time %>%
    filter( between(x = date,left = ymd("2019-03-01"),right = ymd("2020-01-01") ) )%>%
    group_by(occupation,industry ,measure) %>%
    mutate(value2 = cumsum(diff) ) %>%
    mutate(covid = "pre-covid")%>%
    mutate(quarter_date = as_date(paste0("2020-",month(date),"-01"))) %>%
    mutate(index = month(date))
  
  plotting_lost_comparison <- value_lost_during_covid %>%
    bind_rows(value_lost_pre_covid) %>%
    group_by(.data$quarter_date,.data$covid,.data$occupation,.data$industry) %>%
    summarise(value2 = sum(.data$value2,na.rm = TRUE),
              value = sum(.data$value,na.rm = TRUE) ) %>%
    select(-value2) %>%
    mutate(occupation = str_trim(.data$occupation)) %>%
    pivot_wider(c(.data$quarter_date,.data$occupation,.data$industry),
                names_from = .data$covid,
                values_from = .data$value) %>%
    ungroup() %>%
    mutate(
      difference_value = ifelse(quarter_date == max(quarter_date), covid - `pre-covid`, NA),
      label_value = ifelse(quarter_date == max(quarter_date),
                           paste0( round(.data$difference_value),"000\nhours" ), 
                           NA)
      
    ) %>%
    mutate(
      label_y = 
        ifelse(difference_value < 0,
               (covid + `pre-covid`)/2, 
               (covid + `pre-covid`)/2 )
    ) 
  max_date <- max(plotting_lost_comparison$quarter_date)
  
  get_extra_points1 <- plotting_lost_comparison %>%
    mutate(quarter_date = quarter_date + months(1)) %>%
    mutate(label_value = NA,
           label_y = NA)
  get_extra_points2 <- get_extra_points1 %>%
    mutate(quarter_date = quarter_date + months(1)) %>%
    bind_rows(get_extra_points1) %>%
    mutate(label_value = NA,
           label_y = NA)
  
  plotting_lost_comparison2 <- plotting_lost_comparison %>%
    bind_rows(get_extra_points2)
  
  
plot_occ <- function(.data){
  
   plot_dat <- 
  .data %>%
    #filter(industry == "Arts and Recreation Services") %>%
    cah_plot(aes(x = .data$quarter_date)) +
    geom_line(aes(y = .data$covid)) +
    geom_line(aes(y = .data$`pre-covid`), linetype = "dashed") +
    facet_wrap(. ~ .data$occupation, scales = "free",nrow = 1) +
    geom_ribbon(
      data = filter(.data, covid < `pre-covid`),
      aes(ymax = `pre-covid`,
          ymin = covid
          #fill = `pre-covid` > covid
          ),
      fill = "darkred",
      alpha = 0.20
     # show.legend = FALSE
    ) +
    #scale_fill_manual(values=c("darkgreen", "darkred"), name="fill" )+
    geom_text(aes(y = .data$label_y, label = label_value ),
                    nudge_x = 0,
              size = 1.5) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 5),
      axis.text.x = element_text(size = 5),
      legend.title = element_blank(),
      strip.text = element_blank()
    )
  
  return(plot_dat)
}


plots_by_indus <- 
  plotting_lost_comparison2 %>%
  mutate(occupation = str_wrap(.data$occupation,15)) %>%
  split(.$industry) %>%
  map(plot_occ)

for (i in 1:length(plots_by_indus) ) {
  
  plot_name <- str_trim( as.character(names(plots_by_indus)[i]) )
  plots_by_indus[[i]] %>%
    ggsave(filename = glue::glue("plots/",plot_name,".png"),dpi = 400,device = "png",scale = 1,
           width = 12.58,height = 1.15  )
  
}
  

# value_lost_dat%>%
  #   mutate(occupation = str_wrap(occupation,10)) %>%
  #   cah_plot(aes(x = date, 
  #                fill = occupation, 
  #                y = value)) +
  #   geom_col() + 
  #   # scale_fill_gradient(low = "red",high = "white" ) +
  #   facet_wrap(.~industry,scales = "free") + 
  #   theme(
  #     axis.title = element_blank(),
  #     axis.text = element_text(size = 6)
  #   )
  #   
  
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
