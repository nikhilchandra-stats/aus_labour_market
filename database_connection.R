library(DBI)
library(odbc)

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "localhost\\SQLEXPRESS", 
                 Database = "ABS", 
                 Trusted_Connection = "True")

