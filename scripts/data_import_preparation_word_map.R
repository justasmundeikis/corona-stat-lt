# JHCSSE data import
data_confirmed <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv', stringsAsFactors = FALSE)
data_deaths <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv', stringsAsFactors = FALSE)
data_recovered <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv', stringsAsFactors = FALSE)

# function mapping WHO country names to shape (map) country names
change_country_names <- function(data) {
        data$Country.Region[data$Country.Region=="Akrotiri and Dhekelia"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Anguilla"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Bermuda"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="British Antarctic Territory"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="British Indian Ocean Territory"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="British Virgin Islands"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Cayman Islands"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Falkland Islands"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Gibraltar"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Montserrat"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Pitcairn, Henderson, Ducie and Oeno Islands"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Saint Helena, Ascension and Tristan da Cunha, including:"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Saint Helena"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Ascension Island"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Tristan da Cunha"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="South Georgia and the South Sandwich Islands"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Turks and Caicos Islands"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Guernsey"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Jersey"] <-"United Kingdom"
        data$Country.Region[data$Country.Region=="Aruba"] <- "Netherlands"
        data$Country.Region[data$Country.Region=="Curacao"] <- "Netherlands"
        data$Country.Region[data$Country.Region=="French Guiana"] <- "France"
        data$Country.Region[data$Country.Region=="Martinique"] <- "France"
        data$Country.Region[data$Country.Region=="Reunion"] <- "France"
        data$Country.Region[data$Country.Region=="Guadeloupe"] <- "France"
        data$Country.Region[data$Country.Region=="Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
        data$Country.Region[data$Country.Region=="Congo (Brazzaville)"] <-"Republic of the Congo"
        data$Country.Region[data$Country.Region=="Cote d'Ivoire"] <- "Ivory Coast"
        data$Country.Region[data$Country.Region=="Holy See"] <- "Vatican"
        data$Country.Region[data$Country.Region=="Korea, South"] <- "South Korea"
        data$Country.Region[data$Country.Region=="North Macedonia"] <- "Macedonia"
        data$Country.Region[data$Country.Region=="Serbia"] <- "Republic of Serbia"
        data$Country.Region[data$Country.Region=="US"] <- "United States of America"
        as.data.frame(data)
}

# function cleaning data 
cleaning_data <- function(data) {
        data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
        data %<>% gather(key=date, value=count, -country)
        data %<>% mutate(date=date %>%substr(2,8) %>% mdy())
        data %<>% group_by(country, date) %>% summarise(count=sum(count)) %>% as.data.frame()
        
}


# applying function to clean data
data_confirmed %<>% change_country_names() %>% cleaning_data()%>% rename(confirmed=count)
data_deaths %<>% change_country_names() %>% cleaning_data() %>% rename(deaths=count)
data_recovered %<>% change_country_names() %>%cleaning_data( )%>% rename(recovered=count)

# assembling all date in one file
# calcuclating active cases
data_world_map <- data_confirmed %>% 
        merge(data_deaths)%>% 
        merge(data_recovered)%>%
        mutate(active=confirmed-recovered-deaths) %>%
        gather(key=var, value=value, -c(country, date)) %>%
        mutate(var=factor(var, levels=c("confirmed", "active", "recovered", "deaths")))%>%
        filter(country!="Cruise Ship")%>% # here change only for map
        mutate(valstybe=countrycode(country, origin = "country.name", destination =  "cldr.short.lt",nomatch = NULL ))%>%
        mutate(CNTR_CODE=countrycode(country, origin = "country.name", destination =  "eurostat",nomatch = NULL )) 

# saving data in subfolder for further usage
write.csv(data_world_map, "./data/data_world_map.csv", row.names = FALSE)

rm(list = ls())
