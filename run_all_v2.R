
library("anytime")
library("DBI")
library("directlabels")
library("extrafont")
library("formattable")
library("gghighlight")
library("ggrepel")
library("ggthemes")
library("httr")
library("isoband")
library("janitor")
library("jsonlite")
library("keyring")
library("lemon")
library("logging")
library("lubridate")
library("magrittr")
library("odbc")
library("openxlsx")
library("readxl")
library("slider")
library("scales")
library("snakecase")
library("svDialogs")
library("tidyverse")

#-------------------- global variables --------------------

G_timestamp_declaration <- now()
# G_output_path <- onedrive("outputs")
# G_input_path  <- onedrive("data\\raw")

# # For testing purposes:
# G_timestamp_declaration <- parse_date_time("2020-12-09 06:30:00", orders = "ymd HMS", tz = "Australia/Sydney")
# 
