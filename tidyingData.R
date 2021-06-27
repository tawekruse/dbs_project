library(janitor)
#setwd("/Users/tawekruse/Desktop/dataProject")
library(relations)
library(magrittr)

#co2-table sauber machen
co2_dirty <- read.csv("co2_emission.csv", na.strings=c("","NA")) %>% 
  filter(Entity != "World") %>% 
  na.omit()

co2_tidy <- co2_dirty %>% 
  filter(Annual.CO..emissions..tonnes.. != 0) %>% 
  rename(country_name = "Entity", country_code = "Code", year = "Year",
         co2_emissions = "Annual.CO..emissions..tonnes..")

co2_country <-  as.data.frame(unique(co2_tidy$country_code))
co2_rel <- as.relation(co2_country)

write_csv(co2_tidy, "co2_tidy.csv")

#population_total sauber machen
population_total_drt <- read_csv("population_total.csv")
pop_total_country <-unique(population_total_drt$"Country Name") 

#population_growth sauber machen
population_growth <- read.csv("population_growth.csv", sep = ";")

names(population_growth) <-  c("country_name", "country_code", "1961":"2019")

population_growth_tidy <- population_growth %>% 
  gather(key = "year", value = "population_growth", "1961":"2019")


write_csv(population_growth_tidy, "population_growth_tidy.csv")
 
#gdp sauber machen

gdp <- read.csv("gdp.csv", sep = ";") %>% 
  clean_names()

temp <- as.data.frame(unique(population_growth_tidy$country_code))

names(temp) <- c("country_code")

gdp_tidy_wide <- left_join(temp, gdp, by = c("country_code"))
names(gdp_tidy_wide) <-  c("country_name", "country_code", "1960":"2019")

gdp_tidy_long <- gdp_tidy_wide %>% 
  gather(key = "year", value = "gdp_value", "1960":"2019")

gdp_tidy_long %<>% select("country_name", "country_code", "year", "gdp_value")

gdp_tidy_long %<>% na.omit()

write_csv(gdp_tidy_long, "gdp_tidy.csv")
## __________________________________________

population_total_drt <- read.csv("population_total.csv", sep = ";")
temp_country <-  as.data.frame(unique(gdp_tidy_long$country_code))
temp_country2 <-  as.data.frame(unique(population_total_drt$`Country Name`))
pop_rel <- as.relation(temp_country)
gdp_rel <- as.relation(temp_country)

relation_table(gdp_rel - pop_rel)

gdp_new <- read.csv("gdp_tidy.csv", sep = ";") %>% 
  select(country_code, country_name)

gdp_new2 <- as.data.frame(unique(gdp_new))

population_total <- left_join(population_total_drt, gdp_new2)

write_csv(population_total, "population_total_tidy.csv")
##__________________________


hdi <- read.csv("hdi.csv", sep = ";", na.strings=c("..","NA"))

temp_country2 <-  as.data.frame(unique(hdi$`Country`))
hdi_rel <- as.relation(temp_country2)
gdp_rel <- as.relation(temp_country)

relation_table(hdi_rel - gdp_rel)

names(hdi) <-  c("country_name","1990":"2019")

hdi_long <- hdi %>% 
  gather(key = "year", value = "hdi", "1990":"2019") %>% 
  na.omit()

hdi_tidy <- left_join(hdi_long, gdp_new2)  

write_csv(hdi_tidy, "hdi_tidy.csv")
