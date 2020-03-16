if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("magrittr")) install.packages("magrittr"); library("magrittr")
if(!require("countrycode")) install.packages("countrycode"); library("countrycode")



# JHCSSE data import
data_confirmed <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv', stringsAsFactors = FALSE)
data_deaths <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv', stringsAsFactors = FALSE)
data_recovered <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv', stringsAsFactors = FALSE)

# function cleaning data 
cleaning_data <- function(data) {
        data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
        data %<>% gather(key=date, value=count, -country)
        data %<>% mutate(date=date %>%substr(2,8) %>% mdy())
        data %<>% group_by(country, date) %>% summarise(count=sum(count)) %>% as.data.frame()
        
}

# applying function to clean data
data_confirmed %<>% cleaning_data()%>% rename(confirmed=count)
data_deaths %<>% cleaning_data() %>% rename(deaths=count)
data_recovered %<>% cleaning_data( )%>% rename(recovered=count)

# assembling all date in one file
# calcuclating active cases
data_world <- data_confirmed %>% 
        merge(data_deaths)%>% 
        merge(data_recovered)%>%
        mutate(active=confirmed-recovered-deaths) %>%
        gather(key=var, value=value, -c(country, date)) %>%
        mutate(var=factor(var, levels=c("confirmed", "active", "recovered", "deaths"))) %>%
        mutate(valstybe=countrycode(country, origin = "country.name", destination =  "cldr.short.lt",nomatch = NULL ))

# saving data in subfolder for further usage
write.csv(data_world, "./data/data_world.csv", row.names = FALSE)
