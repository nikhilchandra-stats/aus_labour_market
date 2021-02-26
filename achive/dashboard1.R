source("run_all_v2.R")
source("get_all_labour_gender.R")
source("plot_summaries.R")
source("ABS_eco_data2.R")
source("labour_by_age_detailed.R")
source("get_wages.R")
library(cah)
library(covidpmc)

labour_data <- eco_tables_abs()

.data <- labour_data[[28]][[2]] 
wages_dat <- read_in_wages()

#------------------------------------------------------------------------

plot_employed_status_trend <- function(.data,
                                 .state_int = "Australia",
                                 .status_int = "Employed total",
                                 .width_chosen ,
                                 .height = 2){
  
  G_timestamp_declaration <- now()
  
  dat <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status == .status_int) %>%
    mutate(perc_female = round(Females/Persons,2) ) %>%
    mutate(perc_male = round(Males/Persons,2) ) %>%
    mutate(Males = perc_male,
           Females = perc_female) %>%
    select(date,state,Females,Males) %>%
    mutate(gender_diff = Males - Females) %>%
    mutate(value = gender_diff) %>%
    mutate(index = 1:length(date)) %>%
    mutate(value2 = 
             c(
               predict(
                 loess(value ~ index ) 
                 ) 
               ) 
           )
    # pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    # group_by(sex) %>%

  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat %>%
    cah_plot(aes(x = date, y = value2*100 ))+
    geom_line(show.legend = FALSE,
                color = "#44546c", size = 0.9, fill = "#44546c") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin = 0, ymax = 100*value2 ), 
                alpha = 0.25, fill = "#44546c") +
    geom_text( aes(y = -100*0.01, 
                   x = ymd("2017-01-01"), 
                   label = "Gender Parity"  ),
                    color = "#44546c",
               size = 4)+
    geom_text( aes(y = 100*0.04, 
                   x = ymd("2010-01-01"), 
                   label = "Gender Employment\nGap"  ),
               color = "#44546c",
               size = 4.0)+
    ylim(100*-0.03,100*0.12) +
    # scale_colour_manual(
    #   values = cols_sex,
    #   aesthetics = c("colour", "fill")
    # ) +
    # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
    #                    limits = c(-0.05,0.12) ) + 
    ylab("Gender Split of Employed Persons\n(percentage points)") +
    theme(panel.grid.minor = element_blank(),panel.grid.major  = element_blank(),
          axis.title.y = element_text(size = 9,colour = "#44546c"),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 8,colour = "#44546c") ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_line.png") ),
             height = .height ,width = .width_chosen )
  
}

#-------------------------------------------------------------------

plot_employed_status_trend_2 <- function(.data,
                                       .state_int = "Australia",
                                       .status_int = "Employed total",
                                       .width_chosen ,
                                       .height = 2){
  
  G_timestamp_declaration <- now()
  
  dat <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status == .status_int) %>%
    mutate(perc_female = round(Females/Persons,2) ) %>%
    mutate(perc_male = round(Males/Persons,2) ) %>%
    mutate(Males = perc_male,
           Females = perc_female) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    mutate(value = ( value + lag(value,1) + lag(value,2) +
                     lag(value,3) + lag(value,5) +
                     lag(value,6) + lag(value,7)
                      + lag(value,8))/9 )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat %>%
    cah_plot(aes(x = date, y = value, color = sex ))+
    geom_smooth(show.legend = FALSE,se = FALSE , size = 0.9) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    ylab("Percentage of Labour Market Employed\nby Gender") +
    theme(panel.grid.minor = element_blank(),panel.grid.major  = element_blank(),
          axis.title = element_text(size = 7,colour = "#44546c"),
          axis.text = element_text(size = 7,colour = "#44546c") ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_line2.png") ),
             height = .height ,width = .width_chosen )
  
}

#-------------------------------------------------------------------

plot_employed_status_now<- function(.data,
                                       .state_int = "Australia",
                                       .status_int = "Employed total",
                                    .width_chosen,
                                    .height = 2  ){
  G_timestamp_declaration <- now()
  
  dat <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status == .status_int) %>%
    mutate(perc_female = round(Females/Persons,2) ) %>%
    mutate(perc_male = round(Males/Persons,2) ) %>%
    mutate(Males = perc_male,
           Females = perc_female) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    filter(date == max(date))%>%
    mutate(dummy = "test")
  
  dat_num <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status == .status_int) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    filter(date == max(date)) %>%
    rename(value_num = value) %>%
    mutate(value_num = round(value_num,2))
    
  dat <- left_join(dat,dat_num,by = c("date","state","sex"))
  #mutate(value = ( value + lag(value,1) + lag(value,2) + lag(value,3) + lag(value,5) +  lag(value,6) )/7 )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat %>%
    cah_plot(aes(x = dummy, y = value, fill = sex, 
               label = paste0(100*value,"%") )) + 
    geom_col( show.legend = FALSE,position = "fill") +
    geom_text(size = 5.5, position = position_stack(vjust = 0.5), colour = "white" ) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+
    ylab("")+
    xlab("")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) )+
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(text = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major  = element_blank()  ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                           "gend_bar.png") ),
             height = .height,width = .width_chosen )
  
}
 

plot_employed_status_now_numeric<- function(.data,
                                    .state_int = "Australia",
                                    .status_int = "Labour force total",
                                    .width_chosen,
                                    .height = 2 ){
  G_timestamp_declaration <- now()
  
  dat_numeric <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status == .status_int) %>%
    mutate(perc_female = round(Females/Persons,2) ) %>%
    mutate(perc_male = round(Males/Persons,2) ) %>%
    mutate(Males = perc_male,
           Females = perc_female) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    filter(date == max(date))%>%
    mutate(dummy = "test") %>%
    mutate(value = round(value,2) )
  #mutate(value = ( value + lag(value,1) + lag(value,2) + lag(value,3) + lag(value,5) +  lag(value,6) )/7 )
  
  ylimits <- dat %>% ungroup() %>% summarise(value = sum(value))
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat_numeric %>%
    cah_plot(aes(x = dummy, y = value, fill = sex, 
                 label = paste0(100*value," %") )) + 
    geom_col( show.legend = FALSE) +
    geom_text(size = 5.5, 
              position = position_stack(vjust = 0.5), 
              colour = "white" ) +
    ylim(0,ylimits$value[1]) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+
    ylab("")+
    xlab("")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) )+
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(text = element_blank(),
          panel.grid.minor = element_blank() ,
          panel.grid.major  = element_blank() ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_bar_partic_rate.png") ),
             height = .height ,width = .width_chosen )
  
}

#-------------------------------------------------------------------------
plot_employed_status_rate<- function(.data,
                                    .state_int = "Australia",
                                    .status_int = "Unemployed total",
                                    .width_chosen ,
                                    .height = 2  ){
  G_timestamp_declaration <- now()
  
  lf_size <- .data %>% 
    filter(employment_status %in% "Labour force total") %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    rename(Females_lf = Females,
           Males_lf = Males,
           Persons_lf = Persons ) %>%
    select(date,state,Females_lf ,Males_lf,Persons_lf)
  
  dat <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    left_join(lf_size, by = c("date","state" ) ) %>%
    filter(employment_status %in% c(.status_int) ) %>%
    mutate(perc_female = round(Females/Females_lf,4) ) %>%
    mutate(perc_male = round(Males/Males_lf,4) ) %>%
    mutate(Males = perc_male,
           Females = perc_female) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    filter(date == max(date))%>%
    mutate(dummy = "test")

  #mutate(value = ( value + lag(value,1) + lag(value,2) + lag(value,3) + lag(value,5) +  lag(value,6) )/7 )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat %>%
    cah_plot(aes(x = state, y = value, fill = sex, 
                 label = paste0(100*value,"%") )) + 
    geom_col( show.legend = FALSE) +
    geom_text(size = 5.5, 
              position = position_stack(vjust = .5), 
              colour = "white" ) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+
    ylab("")+
    xlab("")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) )+
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(text = element_blank(),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_rate.png") ),
             height = .height,width = .width_chosen )
  
}

#----------------------------NUM And Perc LABEL

plot_employed_status_num_perc<- function(.data,
                                    .state_int = "Australia",
                                    .status_int = "Employed total",
                                    .width_chosen = 5,
                                    .height = 2  ){
  G_timestamp_declaration <- now()
  
  dat <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status == .status_int) %>%
    mutate(perc_female = round(Females/Persons,2) ) %>%
    mutate(perc_male = round(Males/Persons,2) ) %>%
    mutate(Males = perc_male,
           Females = perc_female) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    filter(date == max(date))%>%
    mutate(dummy = "test")
  
  dat_num <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status == .status_int) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    filter(date == max(date)) %>%
    rename(value_num = value) %>%
    mutate(value_num = round(value_num/1000,2))
  
  dat <- left_join(dat,dat_num,by = c("date","state","sex"))
  #mutate(value = ( value + lag(value,1) + lag(value,2) + lag(value,3) + lag(value,5) +  lag(value,6) )/7 )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat %>%
    cah_plot(aes(x = dummy, y = value, fill = sex, 
                 label = paste0(100*value,"%","\n",value_num," Million") )) + 
    geom_col( show.legend = FALSE,position = "fill") +
    geom_text(size = 4.7, position = position_stack(vjust = 0.5), colour = "white" ) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+
    ylab("")+
    xlab("")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) )+
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(text = element_blank(),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_bar.png") ),
             height = .height,width = .width_chosen )
  
}


wage_plot_1 <- function(.data ,
                        .state_int = "Australia",
                        .status_int = "Total",
                        .width_chosen ,
                        .height = 2 ){
  
  dat_plot <- .data %>% 
    filter(state == .state_int) %>%
    filter(sex != "Persons") %>%
    filter(employment_status == .status_int) %>%
    #mutate(date_paste = rep( c("-01-01","-06-01"),round(length(state)/2) ) ) %>%
    mutate(date = paste0(date,"01-01") ) %>%
    mutate(date = as_date(date)) %>%
    mutate(value = as.numeric(value)) %>%
    pivot_wider(c(date,units,employment_status,state),
                values_from = value, 
                names_from = sex) %>%
    mutate(
      median_value = (Males + Females)/2
    )

  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat_plot %>% 
    cah_plot(aes(x = date)) +
    geom_line(aes(y = Females), color ="#6474b5", size = 1 ) +
    geom_line(aes(y = Males), color = "#ec7c24" , size = 1) +
    geom_ribbon(aes(ymin = Females, ymax = Males), fill = "#44546c", alpha = 0.25)+
    theme(
      axis.title.x = element_blank()
    )+
    theme(panel.grid.minor = element_blank(),panel.grid.major  = element_blank(),
          axis.title.y = element_text(size = 9,colour = "#44546c"),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 7,colour = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() )+ 
    ylab("Median Income ($)") +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_wages.png") ),
             height = .height,width = .width_chosen )
    
}

#-----------------------------------------------------------Plot Indus
plot_summary_indus <- function(.data, 
                              people_hours = "people",
                              .width_chosen = 5,
                              .height = 2,
                              .industries = 
                                c("Professional, Scientific and Technical Services",
                                  "Construction",
                                  "Education and Training",
                                  "Health Care and Social Assistance",
                                  "Financial and Insurance Services") ){
  
  
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
    ) %>%
    filter(
      measure == "Full Time Employed"
    ) %>%
    filter(
      industry %in% .industries
    ) %>%
    mutate(
      industry = str_wrap(industry,15)
    )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  indus_labour_analysis %>%
    cah_plot(aes(x = industry, y = value, fill = sex, 
               label = paste0(100*value,"%") )
             ) + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    theme(
      
      strip.text = element_blank()
      
    )+
    coord_flip() +
    facet_wrap(~measure,scales = "free",nrow = 2,ncol = 1) +
    theme(axis.title  = element_blank(),
          # legend.title = element_blank(),
          # legend.text = element_text(face = "bold",color = "#003366"),
          axis.text.y = element_text(size = 9, color = "#44546c"),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) + 
    # labs(
    #   title = "Gender ratios of people employed by industry"
    # ) + 
    theme(
      text = element_text(colour = "#44546c", size = 3.5)
    ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "gend_indus.png") ),
             height = .height,width = .width_chosen )
  
  
  
}

#-----------------------------------------------------------Plot Age

plot_by_age <- function(.data                             
                        , .employment_chosen = "Employed total",
                        .width_chosen = 5,
                        .height = 2){
  
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
    filter(age != "55 years and over") %>%
    filter(
      employment_status == .employment_chosen
    )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  
  plot_dat %>% 
    cah_plot(aes(x = age, y = value, fill = sex, 
               label = paste0(100*value,"%") )) + 
    theme_minimal() + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 4.5, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    # theme(
    #   
    #   strip.text = element_text(color = "#44546c", size = 11)
    #   
    # )+
    coord_flip() +
    # facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    theme(axis.title  = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y =  element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", color = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "gend_age.png") ),
             height = .height,width = .width_chosen )
  
}  

#------------------------------------------OCCUPATION
plot_occ <- function(.data                             
                     , .employment_chosen = "full_time_people",
                     .width_chosen = 5,
                     .height = 2){
  max_date <- dat %>% distinct(date) %>%
    ungroup() %>%
    summarise(date = max(date))
  
  dat <- .data %>%
    group_by(date,sex,occupation,measure) %>%
    summarise(value = sum(value,na.rm = TRUE)) %>%
    filter(measure == .employment_chosen) %>%
    pivot_wider(c(date,measure,occupation), values_from = value, names_from = sex) %>%
    mutate(
      Males_perc = Males/(Males + Females),
      Females_perc = Females/(Males + Females)
    )  %>%
    ungroup()
  
  dat_1 <- dat %>%
    select(-Males,-Females) %>%
    pivot_longer(-c(date,occupation,measure),
                 values_to = "value_perc", names_to = "sex") %>%
    mutate(
      sex = case_when(
        sex == "Males_perc" ~ "Males",
        sex == "Females_perc" ~ "Females"
      )
    )
  
  dat_2 <- dat %>%
    select(-Males_perc,-Females_perc) %>%
    pivot_longer(-c(date,occupation,measure),
                 values_to = "values", names_to = "sex")
  
  dat <- left_join(dat_1,dat_2, by = c("date","occupation","sex","measure")) %>%
    mutate(date = as_date(date)) 
  
  dat_plot <- dat %>% 
    filter( as_date(date) == as_date(max(date,na.rm = TRUE))) %>%
    mutate(value_perc = round(value_perc,2)) %>%
    mutate(
      occupation = str_wrap(occupation,35)
    )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  
  dat_plot %>%
    cah_plot(aes(x = occupation, y = value_perc, fill = sex, 
                 label = paste0(100*value_perc,"%") )) + 
    theme_minimal() + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 3.25, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    # theme(
    #   
    #   strip.text = element_text(color = "#44546c", size = 11)
    #   
    # )+
    coord_flip() +
    # facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    theme(axis.title  = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y =  element_text(size = 8),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", color = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    )+
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "gend_occ.png") ),
             height = .height,width = .width_chosen )
  
  
}

#---------------------------------------WOMEN IN MANAGEMENT
management_line <- function(.data                             
                            , .employment_chosen = "full_time_people",
                            .width_chosen = 5,
                            .height = 2){
                              
      dat <- .data %>%
        filter(occupation == "Managers") %>%
        group_by(date,sex) %>%
        summarise(value = sum(value)) %>%
        mutate(date = as_date(date))
      
      dat2 <- dat %>%
        pivot_wider(c(date), names_from = sex, values_from = value) %>%
        mutate(
          Males_perc = Males/(Females + Males),
          Females_perc = Females/(Females + Males)
        ) %>%
        mutate(diff_value = Males_perc - Females_perc)
      
      cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
      
      dat2 %>% 
        cah_plot(aes(x = date))+
        geom_line(aes(y = Males), color = "#ec7c24" )+
        geom_line(aes(y = Females), color = "#6474b5" )+
        geom_ribbon(aes(ymin = Females, ymax = Males), 
                    fill = "#44546c" ,
                    alpha = 0.25)+
        ylab("Managers (000')")+
        theme(
          axis.title.x = element_blank(),
          axis.text.x =  element_text(size = 6),
          axis.text.y =  element_text(size = 6),
          axis.title.y =  element_text(size = 8),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
        ) +
        cah_save(filename = file.path(G_output_path,
                                      paste0(as_date(G_timestamp_declaration),.employment_chosen,
                                             "gend_managers.png") ),
                 height = .height,width = .width_chosen )


}


management_line2 <- function(.data                             
                            , .employment_chosen = "full_time_people",
                            .width_chosen = 5,
                            .height = 2){
  
  dat <- .data %>%
    filter(occupation == "Managers") %>%
    group_by(date,sex) %>%
    summarise(value = sum(value)) %>%
    mutate(date = as_date(date))
  
  dat2 <- dat %>%
    pivot_wider(c(date), names_from = sex, values_from = value) %>%
    mutate(
      Males_perc = Males/(Females + Males),
      Females_perc = Females/(Females + Males)
    ) %>%
    mutate(diff_value = Males_perc - Females_perc) %>%
    mutate(diff_raw = Males - Females)
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")

  dat2 %>% 
    cah_plot(aes(x = date, y = diff_value))+
    geom_smooth(color = "#44546c", se = FALSE)+
    ylab("% Difference between number of\nMale and Female Managers")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x =  element_text(size = 6),
      axis.text.y =  element_text(size = 6),
      axis.title.y =  element_text(size = 8),
      panel.grid.minor = element_blank()
      ,panel.grid.major  = element_blank() 
      
    )+
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.employment_chosen,
                                         "gend_managers2.png") ),
             height = .height,width = .width_chosen )
  
}

#-----------------------------------------------------------

managers_industry <- function(.data                             
                             , .employment_chosen = "full_time_people",
                             .width_chosen = 5,
                             .height = 2,
                             .industries = 
                               c("Professional, Scientific and Technical Services",
                                 "Construction",
                                 "Education and Training",
                                 "Health Care and Social Assistance",
                                 "Financial and Insurance Services") ){
  
  dat <- .data %>%
    filter(occupation == "Managers") %>%
    group_by(date,sex,industry) %>%
    summarise(value = sum(value)) %>%
    mutate(date = as_date(date))
  
  dat2 <- dat %>%
    pivot_wider(c(date,industry), names_from = sex, values_from = value) %>%
    mutate(
      Males_perc = Males/(Females + Males),
      Females_perc = Females/(Females + Males)
    ) %>%
    mutate(diff_value = Males_perc - Females_perc) %>%
    mutate(diff_raw = Males - Females) %>%
    filter(
      industry %in% .industries
    ) %>%
    ungroup() %>%
    filter(date == max(date)) %>%
    select(-Males,-Females,-diff_value,-diff_raw) %>%
    pivot_longer(-c(date,industry), values_to = "value", names_to = "sex") %>%
    mutate(
      sex = case_when(
        sex == "Males_perc" ~ "Males",
        sex == "Females_perc" ~ "Females"
        
      )
    ) %>%
    mutate(
      value = round(value,2)
    ) %>%
    mutate(
      industry = str_wrap(industry,15)
    )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat2 %>% 
    cah_plot(aes(x = industry, y = value, fill = sex, 
                 label = paste0(100*value,"%") )) + 
    theme_minimal() + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 3.5, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    # theme(
    #   
    #   strip.text = element_text(color = "#44546c", size = 11)
    #   
    # )+
    coord_flip() +
    # facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    theme(axis.title  = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y =  element_text(size = 7),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", color = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "managers_indus.png") ),
             height = .height,width = .width_chosen )
  
}

#--------------------------------------------------------------------------------
plot_employed_part_full_ratio<- function(.data,
                                     .state_int = "Australia",
                                     .status_int = c("Employed full-time","Employed part-time"),
                                     .sex_chosen = "Males",
                                     .width_chosen ,
                                     .height = 2  ){
  G_timestamp_declaration <- now()
  
  lf_size <- .data %>% 
    filter(employment_status %in% "Labour force total") %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    rename(Females_lf = Females,
           Males_lf = Males,
           Persons_lf = Persons ) %>%
    select(date,state,Females_lf ,Males_lf,Persons_lf)
  
  dat <- .data %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    left_join(lf_size, by = c("date","state" ) ) %>%
    filter(employment_status %in% c(.status_int) ) %>%
    select(date,state,employment_status,Females,Males) %>%
    pivot_longer(c(-date,-state,-employment_status), values_to = "value", names_to = "sex") %>%
    group_by(sex,date) %>%
    mutate(part_time_perc = sum(
                                ifelse(employment_status == "Employed part-time",value,NA),
                                na.rm = TRUE
                                )/sum(value) ) %>%
    mutate(full_time_perc = 1 - part_time_perc) %>%
    select(date,sex,part_time_perc,full_time_perc) %>%
    ungroup() %>%
    filter(!duplicated(.))
  #mutate(value = ( value + lag(value,1) + lag(value,2) + lag(value,3) + lag(value,5) +  lag(value,6) )/7 )
  
  cols_male <- c("Part Time" = "#ec7c24", "Full Time" = "#ec7c24")
  cols_female <- c("Part Time" = "#6474b5", "Full Time" = "#6474b5")
  
  
  dat_males <- dat %>%
    filter(sex == "Males") %>%
    pivot_longer(-c(sex,date), names_to = "emp_status", values_to = "value") %>%
    filter(date == max(date)) %>%
    mutate(value = round(value,2)) %>%
    mutate(emp_status = ifelse(emp_status == "part_time_perc","Part Time","Full Time")) %>%
    mutate(label_x = paste0(100*value,"%") ) %>%
    ungroup()
  
  dat_females <- dat %>%
    filter(sex == "Females") %>%
    pivot_longer(-c(sex,date), names_to = "emp_status", values_to = "value") %>%
    filter(date == max(date)) %>%
    mutate(value = round(value,2)) %>%
    mutate(emp_status = ifelse(emp_status == "part_time_perc","Part Time","Full Time")) %>%
    mutate(label_x = paste0(100*value,"%") ) %>%
    ungroup()
  
  dat_males %>%
    cah_plot(aes(x = sex, y = value, fill = emp_status, 
                 label = label_x) ) + 
    geom_col( show.legend = FALSE, color = "white") +
    geom_text(size = 4.3, position = position_stack(vjust = 0.5), colour = "white" ) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+
    ylab("")+
    xlab("")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) )+
    scale_colour_manual(
      values = cols_male,
      aesthetics = c("colour", "fill")
    ) +
    theme(text = element_blank(),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() )+
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "part_full_male.png") ),
             height = .height,width = .width_chosen )
  
  dat_females %>%
    cah_plot(aes(x = sex, y = value, fill = emp_status, 
                 label = label_x) ) + 
    geom_col( show.legend = FALSE, color = "white") +
    geom_text(size = 4.3, position = position_stack(vjust = 0.5), colour = "white" ) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+
    ylab("")+
    xlab("")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) )+
    scale_colour_manual(
      values = cols_female,
      aesthetics = c("colour", "fill")
    ) +
    theme(text = element_blank(),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() )+
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "part_full_female.png") ),
             height = .height,width = .width_chosen )

}

#------------------------------------------------------------------------------------------UNDEREMP RATE
plot_employed_status_rate_under<- function(.data,
                                     .state_int = "Australia",
                                     .status_int = "Underemployed total",
                                     .width_chosen ,
                                     .height = 2  ){
  G_timestamp_declaration <- now()
  
  dat_aus <- .data %>%
    downloaded_data_national() %>%
    filter(state != "Australia") %>%
    group_by(date,unit,series_type,employment_status) %>%
    summarise(
      
      state = "Australia",
      Persons = sum(Persons,na.rm = TRUE),
      Males = sum(Males,na.rm = TRUE),
      Females = sum(Females,na.rm = TRUE),
      perc  = mean(perc,na.rm = TRUE)
    ) %>%
    ungroup()
  
  dat_start <- .data %>%
    downloaded_data_national() %>%
    filter(state == "Australia") %>%
    mutate(date = as_date(date))
  
  dat_use <- bind_rows(dat_start,dat_aus)
  
  lf_size <- .data %>% 
    filter(employment_status %in% "Labour force total") %>%
    downloaded_data_national() %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    rename(Females_lf = Females,
           Males_lf = Males,
           Persons_lf = Persons ) %>%
    select(date,state,Females_lf ,Males_lf,Persons_lf)
  
  dat <- dat_use %>%
    filter(date > "2000-01-01") %>%
    filter(state == .state_int) %>%
    filter(employment_status %in% c(.status_int) ) %>%
    filter(date == max(date)) %>%
    left_join(lf_size, by = c("date","state" ) ) %>%
    mutate(perc_female = round(Females/Females_lf,4) ) %>%
    mutate(perc_male = round(Males/Males_lf,4) ) %>%
    mutate(Males = perc_male,
           Females = perc_female) %>%
    select(date,state,Females,Males) %>%
    pivot_longer(c(-date,-state), values_to = "value", names_to = "sex") %>%
    group_by(sex) %>%
    filter(date == max(date))%>%
    mutate(dummy = "test")
  
  #mutate(value = ( value + lag(value,1) + lag(value,2) + lag(value,3) + lag(value,5) +  lag(value,6) )/7 )
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat %>%
    cah_plot(aes(x = state, y = value, fill = sex, 
                 label = paste0(100*value,"%") )) + 
    geom_col( show.legend = FALSE) +
    geom_text(size = 5.5, 
              position = position_stack(vjust = .5), 
              colour = "white" ) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+
    ylab("")+
    xlab("")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) )+
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(text = element_blank(),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_rate.png") ),
             height = .height,width = .width_chosen )
  
}

#----------------------------AGE PERC

plot_by_age_perc <- function(.data                             
                        , .employment_chosen = "Unemployed total",
                        .width_chosen = 5,
                        .height = 2){
  
  total_peoples <- .data %>%
    filter(employment_status == "Labour force total") %>%
    pivot_wider(c(date,unit,series_type,employment_status,age), values_from = value,
                names_from = sex) %>%
    select(date,age,unit ,Males, Females) %>%
    rename(lf_Males = Males, lf_Females = Females)
  
  plot_dat <- .data %>%
    filter(employment_status == .employment_chosen) %>%
    pivot_wider(c(date,unit,series_type,employment_status,age), values_from = value,
                names_from = sex)%>%
    select(date,age,unit ,Males, Females) %>%
    rename(unemp_Males = Males, unemp_Females = Females) %>%
    left_join(total_peoples, by = c("date","age","unit")) %>%
    mutate(lf_Males = as.numeric(lf_Males),
           lf_Females = as.numeric(lf_Females),
           unemp_Males = as.numeric(unemp_Males),
           unemp_Females = as.numeric(unemp_Females) ) %>%
    mutate(Males = unemp_Males/lf_Males, Females = unemp_Females/lf_Females) %>%
    select(
      date, age, Males, Females
    ) %>%
    pivot_longer(-c(date,age), names_to = "sex", values_to = "value") %>%
    filter(date == max(date)) %>%
    mutate(value = round(value,2)) %>%
    filter(
      age != "15-64 years" & age != "65 years and over" &
        age != "15-19 years", age != "15-24 years",age != "20-24 years",
      age != "55-64 years", age != "25-34 years", age != "35-44 years",
      age != "45-54 years"
    )
  
  
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  
  plot_dat %>% 
    cah_plot(aes(x = age, y = value, fill = sex, 
                 label = paste0(100*value,"%") )) + 
    theme_minimal() + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 4.25, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    # theme(
    #   
    #   strip.text = element_text(color = "#44546c", size = 11)
    #   
    # )+
    coord_flip() +
    # facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    theme(axis.title  = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y =  element_text(size = 9),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", color = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "gend_age.png") ),
             height = .height,width = .width_chosen )
  
}  


#------------------------------------------------  wage plot 2
wage_plot_avg_wages <- function(.data ,
                        .state_int = "Australia",
                        .status_int = c("Full-time in main job"),
                        .width_chosen ,
                        .height = 2 ){
  
  dat_plot <- .data %>% 
    filter(earning_type == "Ordinary time earnings") %>%
    select(date,value,sex,employment_status) %>%
    mutate(value = as.numeric(value)) %>%
    pivot_wider(c(date,employment_status), values_from = value, names_from = sex) %>%
    mutate(perc = 1 - Females/Males) %>%
    select(date,perc, Males, Females) 
  
  scaling_value = (max(dat_plot$Males)/min(dat_plot$perc))*0.75
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  dat_plot %>% 
    cah_plot(aes(x = date)) +
    geom_line(aes(y = Females), color ="#6474b5", size = 1 ) +
    geom_line(aes(y = Males), color = "#ec7c24" , size = 1) +
    geom_line(aes(y = perc*scaling_value),se = FALSE, linetype = "dashed")+
    #geom_ribbon(aes(ymin = Females, ymax = Males), fill = "#44546c", alpha = 0.25)+
    ylab("Average Weekly Income ($)") +
    scale_y_continuous(sec.axis = sec_axis(~ .*100*(1/scaling_value) ,name = "Gender Pay Gap (%)"))+
    theme(
      axis.title.x = element_blank()
    )+
    theme(panel.grid.minor = element_blank(),panel.grid.major  = element_blank(),
          axis.title.y = element_text(size = 9,colour = "#44546c"),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 7,colour = "#44546c")
          )  +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_wages.png") ),
             height = .height,width = .width_chosen )

}

#----------------------------------------------------- 
job_liklihood <- function(.data){
  
  dat <- .data %>% 
    filter(term == "sexFemale") %>%
    filter(model_type == "Full-time") %>%
    filter(occ_or_ind == "industry") %>%
    mutate(start_date = as_date(start_date)) %>%
    mutate(
      date_flag = ifelse(start_date > "2018-01-01","before","after")
    ) %>%
    mutate(
      estimate_12month = slider::slide(.x = estimate,.f = mean,.before = 24) 
    ) %>%
    mutate(
      estimate_12month = as.numeric(estimate_12month)
    ) %>%
    mutate(
      estimate_12month_exp = exp(estimate_12month)
    )
  
  dat %>%
    filter(start_date > "2008-01-01") %>%
    cah_plot( aes( x = start_date) ) +
    #geom_line(aes( y = estimate) ) +
    #geom_line(aes(y = 1 - exp(estimate) ), linetype = "dashed")+
    geom_smooth(aes(y = 1 - exp(estimate) ),method = "glm",formula = y ~ poly(x,1) )
    #  +
    # geom_smooth(aes(y = conf.low), linetype = "dashed", se = FALSE) +
    # geom_smooth(aes(y = conf.high), linetype = "dashed", se = FALSE)
  
  dat %>% 
    mutate(flag = ifelse(start_date > "2018-01-01","new","old") ) %>%
    cah_plot() + 
    geom_density(aes(x = exp(estimate), fill = flag ), alpha = 0.3 ) 
  
}

#----------------------------------------------------- Educational Attainment

education_sex <- function(.data , 
                          .width_chosen ,
                          .height = 2 ){
  
  plot_dat <- .data %>%
    mutate(age = trimws(age)) %>%
    filter(str_detect(age,"18") == FALSE ) %>%
    filter(date == max(date)) %>%
    mutate(value = value/100)
  
  plot_dat %>% 
    cah_plot(aes(x = age, y = value, fill = sex, 
                 label = paste0(100*value,"%") )) + 
    theme_minimal() + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 4.25, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    # theme(
    #   
    #   strip.text = element_text(color = "#44546c", size = 11)
    #   
    # )+
    coord_flip() +
    # facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    theme(axis.title  = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y =  element_text(size = 9),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", color = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "gend_age_edu.png") ),
             height = .height,width = .width_chosen )
  
  
  plot_dat <- .data %>%
    mutate(age = trimws(age)) %>%
    mutate(sex = trimws(sex)) %>%
    filter(str_detect(age,"18") == FALSE ) %>%
    pivot_wider(c(date,age), values_from = value, names_from = sex) %>%
    mutate(perc = Females - Males)
  
  scaling_value = (max(plot_dat$Males)/min(plot_dat$perc))*0.75
  
  plot_dat %>%
  cah_plot(aes(x = date)) +
    geom_line(aes(y = Females), color ="#6474b5", size = 1 ) +
    geom_line(aes(y = Males), color = "#ec7c24" , size = 1) +
    geom_line(aes(y = perc*3),se = FALSE, linetype = "dashed")+
    facet_wrap(~age) +
    #geom_ribbon(aes(ymin = Females, ymax = Males), fill = "#44546c", alpha = 0.25)+
    ylab("Average Weekly Income ($)") +
    scale_y_continuous(sec.axis = sec_axis(~ .*100*(1/scaling_value) ,name = "Gender Pay Gap (%)"))+
    theme(
      axis.title.x = element_blank()
    )+
    theme(panel.grid.minor = element_blank(),panel.grid.major  = element_blank(),
          axis.title.y = element_text(size = 9,colour = "#44546c"),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 7,colour = "#44546c")
    )  +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),.status_int,
                                         "gend_edu_timeseries.png") ),
             height = .height,width = .width_chosen )
}


#---------------------------------------------------duration of unemp
duration_sex <- function(.data ,
                         .state_int = "Australia",
                         .status_int = c("Long-term unemployed"),
                         .width_chosen ,
                         .height = 2 ){
  
  total_unemp <- .data %>%
    filter(series_type == "Original") %>%
    filter(date == max(date) ) %>%
    select(date,sex,value) %>%
    group_by(date,sex) %>%
    summarise(value = sum(value)) %>%
    pivot_wider(c(date), values_from = value, names_from = sex) %>%
    rename(
      Female_tot = Females,
      Male_tot = Males,
      Persons_tot = Persons
    )
  
  
  dat <- .data %>%
    filter(series_type == "Original") %>%
    filter(employment_status %in% .status_int) %>%
    filter(date == max(date) ) %>%
    select(date,sex,value) %>%
    group_by(date,sex) %>%
    summarise(value = sum(value)) %>%
    pivot_wider(c(date), values_from = value, names_from = sex) %>%
    left_join(total_unemp, by = c("date")) %>%
    mutate(Females_use = Females/Female_tot, Males_use = Males/Male_tot) %>%
    select(date,Females_use, Males_use) %>%
    rename(
       Females = Females_use,
       Males = Males_use 
    ) %>%
    pivot_longer(-date, values_to = "value", names_to = "sex")
    
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  
  dat %>% 
    cah_plot(aes(x = date, y = value, fill = sex, 
                 label = paste0(100*round(value,2),"%") )) + 
    theme_minimal() + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 4.25, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    # theme(
    #   
    #   strip.text = element_text(color = "#44546c", size = 11)
    #   
    # )+
    coord_flip() +
    # facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(axis.title  = element_blank(),
          axis.text =  element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", color = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "gend_longterm.png") ),
             height = .height,width = .width_chosen ) 
  
}

#-------------------------------Federal Government
government_rep <- function(.data,
                           .width_chosen ,
                           .height = 2){
  dat <- .data %>%
    filter(!is.na(Males)) %>%
    filter(date == max(date)) %>%
    filter(perc_num == "num") %>%
    mutate(perc_female = Females/(Males + Females),
           perc_males = 1 - perc_female) %>%
    select(date, House_reps, perc_males, perc_female) %>%
    rename(Males = perc_males, Females = perc_female) %>%
    pivot_longer(-c(date,House_reps), values_to = "value", names_to = "sex" ) %>%
    mutate(House_reps = str_wrap(House_reps,10))
  
  
  
  cols_sex <- c("Males" = "#ec7c24", "Females" = "#6474b5")
  
  
  dat %>% 
    cah_plot(aes(x = House_reps, y = value, fill = sex, 
                 label = paste0(100*round(value,2),"%") )) + 
    theme_minimal() + 
    geom_col(  show.legend = FALSE) + 
    geom_text(size = 3.75, position = position_stack(vjust = 0.5), colour = "white" ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ) +
    # theme(
    #   
    #   strip.text = element_text(color = "#44546c", size = 11)
    #   
    # )+
    coord_flip() +
    # facet_wrap(~age,scales = "free", nrow = 4, ncol = 2) +
    scale_colour_manual(
      values = cols_sex,
      aesthetics = c("colour", "fill")
    ) +
    theme(axis.title  = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", color = "#44546c"),
          panel.grid.minor = element_blank()
          ,panel.grid.major  = element_blank() 
          # axis.text = element_text(family = "TT Arial"), 
    ) +
    cah_save(filename = file.path(G_output_path,
                                  paste0(as_date(G_timestamp_declaration),
                                         "gend_government.png") ),
             height = .height,width = .width_chosen ) 
}
