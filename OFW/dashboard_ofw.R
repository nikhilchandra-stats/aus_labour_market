source("run_all_v2.R")
#source("plot_summaries.R")
source("data cleaning/ABS_eco_data2.R")
source("OFW/labour_by_age_detailed.R")
source("data cleaning/clean_data.R")

# library(cah)
# library(covidpmc)

# labour_data <- eco_tables_abs()
# saveRDS(labour_data,file = "new_abs_data.Rdata")

labour_data[[28]][[2]] %>%
  plot_employed_status_now_numeric(
    .state_int = "Australia",
    .status_int = "Labour force total",
    .width_chosen = 5.5,.height = 2.0
  )

labour_data[[28]][[2]] %>%
  plot_employed_status_num_perc(
    .state_int = "Australia",
    .status_int = "Employed total",
    .width_chosen = 5.5 - 0.25,.height = 2.0 - 0.125
  )

labour_data[[28]][[2]] %>%
plot_employed_status_rate(
  .state_int = "Australia",
  .status_int = "Employed total",
  .width_chosen = 5.5 - 0.25,.height = 2.0 - 0.125
)

labour_data[[28]][[2]] %>%
  plot_employed_status_now(
    .state_int = "Australia",
    .status_int = "Employed full-time",
    .width_chosen = 5.5 - 2,.height = 2.0 - 1
  )

labour_data[[28]][[2]] %>%
  plot_employed_status_now(
    .state_int = "Australia",
    .status_int = "Employed part-time",
    .width_chosen = 5.5 - 2,.height = 2.0 - 1
  )

labour_data[[28]][[2]] %>%
  plot_employed_status_num_perc(
    .state_int = "Australia",
    .status_int = "Unemployed total",
    .width_chosen = 5.5 ,.height = 2.0 
  )

labour_data[[28]][[2]] %>%
  plot_employed_status_trend(
    .state_int = "Australia",
    .status_int = "Employed total",
    .width_chosen = 5 ,.height = 4 
  )

labour_data[[28]][[2]] %>%
  plot_employed_status_trend_2(
    .state_int = "Australia",
    .status_int = "Employed total",
    .width_chosen = 5 ,.height = 4 
  )

wages_dat %>%
  wage_plot_1(
    .state_int = "Australia",
    .status_int = "Total",
    .width_chosen = 4 ,.height = 3 
  )


labour_data[[31]][[2]] %>%
  plot_summary_indus(
    people_hours = "people",
    .width_chosen = 4.5,
    .height = 5  
  )

labour_data[[29]][[2]]  %>%
  plot_by_age(
    .employment_chosen = "Employed total",
    .width_chosen = 5,
    .height = 2.5 
  )

labour_data[[31]][[2]]  %>%
  plot_occ(
    .employment_chosen = "full_time_people",
    .width_chosen = 5.35,
    .height = 3.5
  )

labour_data[[31]][[2]]  %>%
  management_line(
    .employment_chosen = "full_time_people",
    .width_chosen = 3.5,
    .height = 3.5
  )

labour_data[[31]][[2]]  %>%
  management_line2(
    .employment_chosen = "full_time_people",
    .width_chosen = 3.5,
    .height = 3.5
  )

labour_data[[31]][[2]]  %>%
  managers_industry(
    .employment_chosen = "full_time_people",
    .width_chosen = 4.5,
    .height = 4,
    .industries = 
      c("Professional, Scientific and Technical Services",
        "Construction",
        "Education and Training",
        "Health Care and Social Assistance",
        "Financial and Insurance Services")
  )


labour_data[[28]][[2]] %>%
 plot_employed_part_full_ratio(.state_int = "Australia",
                              .status_int = c("Employed full-time","Employed part-time"),
                              .sex_chosen = "Males",
                              .width_chosen = 5.5 - 2,.height = 2.0 - 1)

labour_data[[28]][[2]] %>%
  plot_employed_part_full_ratio(.state_int = "Australia",
                                .status_int = c("Employed full-time","Employed part-time"),
                                .sex_chosen = "Females",
                                .width_chosen = 5.5 - 2,.height = 2.0 - 1)

labour_data[[28]][[2]] %>%
  plot_employed_status_rate(.state_int = "Australia",
                            .status_int = "Unemployed total",
                            .width_chosen = 5.5 - 2,.height = 2.0 - 1)

labour_data[[28]][[2]] %>%
  plot_employed_status_rate_under(.state_int = "Australia",
                            .status_int = "Underemployed total",
                            .width_chosen = 5.5 - 2,.height = 2.0 - 1)

labour_data[[33]][[2]] %>%
  plot_by_age_perc(
    .employment_chosen = "Unemployed total",
    .width_chosen = 5.5 - 1,.height = 2.5
  )

labour_data[[33]][[2]] %>%
  plot_by_age_perc(
    .employment_chosen = "Unemployed total",
    .width_chosen = 5.5 - 1,.height = 2.5
  )


labour_data[[34]][[2]] %>%
  wage_plot_avg_wages(
    .width_chosen = 4 ,
    .height = 3 
  )


labour_data[[30]][[2]] %>%
  duration_sex(
    .width_chosen = 5.5 - 2,.height = 2.0 - 1,
    .status_int = c("Long-term unemployed")
  )

read_educational_sex() %>%
  education_sex( .width_chosen = 4 ,
                 .height = 2)

read.xlsx("gov_sex_leaderhship.xlsx", sheet = 5, startRow =  1)%>%
  government_rep(
    .width_chosen = 4 ,
    .height = 3
  )

datalab_dat <- read_datalab_output()
.data <- read_educational_sex()

.data <-read.xlsx("gov_sex_leaderhship.xlsx", sheet = 5, startRow =  1)

