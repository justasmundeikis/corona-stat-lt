if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("magrittr")) install.packages("magrittr"); library("magrittr")
if(!require("gsheet")) install.packages("gsheet"); library("gsheet")

# data import from google sheets
URL <- "https://docs.google.com/spreadsheets/d/1l9HM2hIjLFA-SZy_kO8b8B5Ddr0hfVZrmx2Yb1n1feg/edit#gid=0"
data_lt <- read.csv(text=gsheet2text(URL, format = "csv"),
                    stringsAsFactors=FALSE,
                    na.strings = "")%>%
        select(1:10)

# transforming date to date format (needed for join opperation)
data_lt %<>% mutate(date=as.Date(date)) 
# creating a vectopr of date from first observation to today
date <- data.frame(date=seq(as.Date("2020-02-28"), today(), by="1 day"))
# joining 
data_lt <- left_join(date, data_lt, by="date")


# grouping by var, then summarising  (summing up) for each cell (county/date)
# spreading with fill=0 for not creating NAs
# na.omit() omits NA, that are created for date+ no entry in GSheet
data_lt %<>% 
        group_by(var, NUTS_ID, date)%>%
        summarise(values=sum(values))%>%
        spread(date, values, fill = 0)%>%
        na.omit()

# this opperation SUCKS, any ideas how to add this to pype above???
data_lt <- cbind(data.frame(data_lt[,1:2]), data.frame(t(apply(data_lt[,3:ncol(data_lt)], 1, cumsum)))) 

# creating county daily change df
data_lt_county <- data_lt %>%
        gather(date, values, 3:ncol(data_lt))%>%
        mutate(date=date %>%substr(2,11) %>% ymd())
write.csv(data_lt_county, "./data/data_lt_county.csv", row.names = FALSE)

# creating country daily change
data_lt_country <- data_lt %>%
        gather(date, values, 3:ncol(.))%>%
        group_by(var,date)%>%
        summarise(values=sum(values))

write.csv(data_lt_country, "./data/data_lt_country.csv", row.names = FALSE)


# crating long format cumulative dataset for counties
# adjustement needed, when first deaths will arrive
data_lt_county_cum <- data_lt %>% 
        gather(date, values, 3:ncol(data_lt))%>% 
        spread(var, values, fill = 0)%>%
        mutate(active=confirmed-recovered)%>%
        #mutate(active=confirmed-recovered-deaths)%>%
        mutate(date=date %>%substr(2,11) %>% ymd())%>%
        gather(var, values, 3:ncol(.))
write.csv(data_lt_county_cum, "./data/data_lt_county_cum.csv", row.names = FALSE)


# crating long format cumulative dataset for whole country
# adjustement needed, when first deaths will arrive
data_lt_country_cum <- data_lt_county_cum %>% 
        group_by(var,date) %>% 
        summarise(values=sum(values))

write.csv(data_lt_country_cum, "./data/data_lt_country_cum.csv", row.names = FALSE)
