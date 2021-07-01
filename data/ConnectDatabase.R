library(tidyverse)
library(RPostgres)

co2_table <- read.csv("data/co2_tidy.csv", sep = ";")
gdp_table <- read.csv("data/gdp_tidy.csv", sep = ";")
hdi_table <- read.csv("data/", sep = ";")
country_table <- read.csv("data/country_tidy.csv", sep = ";")
population_table <- read.csv("data/population_tidy.csv", sep = ";")

con <- dbConnect(RPostgres::Postgres(),
                 host = "localhost",
                 port = 5432,
                 dbname = "dbs_project", 
                 user = "postgres",
                 password = "passwort"
)

#Writes the csv files to the database
dbWriteTable(con,'co2_emissions', co2_table, row.names=FALSE)
dbWriteTable(con,'gdp', gdp_table, row.names=FALSE)
dbWriteTable(con,'hdi', hdi_table, row.names=FALSE)
dbWriteTable(con,'country', country_table, row.names=FALSE)
dbWriteTable(con,'population', population_table, row.names=FALSE)


#The respective tables are pulled from the database
data_co2 <- dbGetQuery(con, "select * from co2_emissions")
data_gdp <- dbGetQuery(con, "select * from gdp")
data_hdi <- dbGetQuery(con, "select * from hdi")
data_country <- dbGetQuery(con, "select * from country")
data_population <- dbGetQuery(con, "select * from population")


#Table that is worked with is created
data <- left_join(data_country, data_gdp, by = "country_code", "year")
data <- left_join(data, data_hdi, by = "country_code", "year")
data <- left_join(data, data_co2, by = "country_code", "year")
data <- left_join(data, data_population, by = "country_code", "year")

#Connection to the database is terminated
dbDisconnect(con)
