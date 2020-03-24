library(zoo)
library(tidyverse)
library(lubridate)

load2020 <- read.csv("data/entso/Total Load - Day Ahead _ Actual_202001010000-202101010000.csv",
                     stringsAsFactors = FALSE,
                     header = TRUE,
                     na.strings = "-")%>%
        mutate(date_time=seq(from = as.POSIXct("2020-01-01 00:00", format="%Y-%m-%d %H:%M"),
                             to = as.POSIXct("2020-12-31 24:00", format="%Y-%m-%d %H:%M"),
                             by = "hour"),
               week_day=wday(date_time, week_start = 1),
               week_num=isoweek(date_time),
               time=rep_along(date_time,sprintf("%02d", 0:23)))%>%
        select(date_time, week_num,week_day, time, Actual.Total.Load..MW....BZN.LT)%>%
        rename(MW=Actual.Total.Load..MW....BZN.LT)%>%
        na.omit()

df <- load2020 %>% filter(date(date_time)>="2020-02-24",
                          date(date_time)<="2020-03-21")%>%
        mutate(week_num=factor(week_num, levels=c(seq(1:12))))

ggplot(df, aes(as.character(interaction(week_day,time)), MW, group=week_num , col=week_num))+
        geom_line(aes(y=rollmean(MW, 6, na.pad=TRUE))) +
        scale_colour_manual(values=c("#848484","#848484","#848484","#FF0000"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


        
