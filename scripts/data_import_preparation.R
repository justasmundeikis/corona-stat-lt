# data import and cleaning
data_confirmed <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv', stringsAsFactors = FALSE)
data_deaths <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv', stringsAsFactors = FALSE)
data_recovered <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv', stringsAsFactors = FALSE)




cleaning_data <- function(data) {
        data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
        data %<>% gather(key=date, value=count, -country)
        data %<>% mutate(date=date %>%substr(2,8) %>% mdy())
        data %<>% group_by(country, date) %>% summarise(count=sum(count)) %>% as.data.frame()
}



data_confirmed %<>% cleaning_data() %>% rename(confirmed=count)
data_deaths %<>% cleaning_data() %>% rename(deaths=count)
data_recovered %<>% cleaning_data( )%>% rename(recovered=count)

data <- data_confirmed %>% 
        merge(data_deaths)%>% 
        merge(data_recovered)%>%
        mutate(confirmed_rest=confirmed-deaths-recovered) %>%
        gather(key=var, value=value, -c(country, date)) %>%
        mutate(var=factor(var, levels=c("confirmed", "confirmed_rest", "recovered", "deaths")))

# todo
# šalies pakeitimai prieš summary, arba reikia daryti ant pavienių datasetų po importo,
# arba pirma apjungti DS ir tada
# reikia papildomo žingsnio verifikavimui (vidiniam) ar  šalys iš  Hopkinso ir žemėlapių atitinka, nes atsiradus naujoms šalims reikės rankutėmis koreguot... hopkins naudoja WHO šlaių pavadinimus, žemėlapiai ne...


# data$country[data$country=="Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
# data$country[data$country=="Cote d'Ivoire"] <- "Ivory Coast"
# data$country[data$country=="French Guiana"] <- "France"
# data$country[data$country=="Martinique"] <- "France"
# data$country[data$country=="Reunion"] <- "France"
# data$country[data$country=="Holy See"] <- "Vatican"
# data$country[data$country=="Korea, South"] <- "South Korea"
# data$country[data$country=="North Macedonia"] <- "Macedonia"
# data$country[data$country=="Serbia"] <- "Republic of Serbia"
# data$country[data$country=="US"] <- "United States of America"




# Duomenys apie Corona atvejus iš spaudos pranešimų
# corona_gs_sheet <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1l9HM2hIjLFA-SZy_kO8b8B5Ddr0hfVZrmx2Yb1n1feg/edit#gid=0")

# Agreguojame atvejus vienos dienos lygmeniu
# corona_gs_sheet %>% group_by(date, var) %>% summarize(value = sum(Value))
# corona_gs_sheet %>% group_by(date, var, county_2) %>% summarize(value = sum(Value))
