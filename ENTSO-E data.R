# Request your API security token at https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_authentication_and_authorisation
# securityToken <- <YOUR_TOKEN>
install.packages("remotes")
remotes::install_github("krose/entsoeAPI")
library(entsoeapi)
library(tidyverse)

en_eic <- entsoeapi::en_eic() # codes
# Lithuania average weekly load
en_load_actual_total_load(eic = "10YLT-1001A0008Q", period_start = lubridate::ymd(as.character(Sys.Date() - 30), tz = "UTC"),
                          period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC"), security_token = securityToken) %>%
        mutate(dt = lubridate::floor_date(dt, "hours")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(dt = lubridate::floor_date(dt, "days")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(wday = lubridate::wday(dt, week_start = 1), isoweek = as.factor(lubridate::isoweek(dt))) %>%
        ggplot(., aes(wday, quantity, col = isoweek)) + geom_line()


en_load_actual_total_load(eic = "10YLT-1001A0008Q", period_start = lubridate::ymd(as.character(Sys.Date() - 30), tz = "UTC"),
                          period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC"), security_token = securityToken) %>%
        mutate(dt = lubridate::floor_date(dt, "hours")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(dt = lubridate::floor_date(dt, "days")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(wday = lubridate::wday(dt, week_start = 1), isoweek = as.factor(lubridate::isoweek(dt))) %>%
        ggplot(., aes(wday, quantity, col = isoweek)) +
        geom_line() + xlab("savaitės diena") + ylab("Kiekis, MW") +
        ggtitle("Elektros tinklų apkrova Lietuvoje, pask. 30 dienų savaitiniai duomenys (vidurkiai), MW", subtitle = "Duomenys: transparency.entsoe.eu, skaičiavimai: corona-stat.lt")

en_load_actual_total_load(eic = "10YLT-1001A0008Q", period_start = lubridate::ymd(as.character(Sys.Date() - 30), tz = "UTC"),
                          period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC"), security_token = securityToken) %>%
        mutate(dt = lubridate::floor_date(dt, "hours")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(dt = lubridate::floor_date(dt, "days")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(wday = lubridate::wday(dt, week_start = 1), isoweek = as.factor(lubridate::isoweek(dt))) %>%
        ggplot(., aes(wday, quantity, col = isoweek)) +
        geom_bar() + xlab("savaitės diena") + ylab("Kiekis, MW") +
        ggtitle("Elektros tinklų apkrova Lietuvoje, pask. 30 dienų savaitiniai duomenys (vidurkiai), MW", subtitle = "Duomenys: transparency.entsoe.eu, skaičiavimai: corona-stat.lt")


ts <- en_load_actual_total_load(eic = "10YLT-1001A0008Q", period_start = lubridate::ymd(as.character(Sys.Date() - 22), tz = "UTC"),
                          period_end = lubridate::ymd_hm(paste0(Sys.Date() - 15," 23:00"), tz = "UTC"), security_token = securityToken)

plot.ts(ts$quantity)


en_load_actual_total_load(eic = "10YLT-1001A0008Q", period_start = lubridate::ymd(as.character(Sys.Date() - 30), tz = "UTC"),
                          period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC"), security_token = securityToken) %>%
        mutate(dt = lubridate::floor_date(dt, "hours")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(dt = lubridate::floor_date(dt, "days")) %>%
        group_by(dt) %>%
        summarise(quantity = mean(quantity)) %>%
        ungroup() %>%
        mutate(wday = lubridate::wday(dt, week_start = 1), isoweek = as.factor(lubridate::isoweek(dt))) %>%
        ggplot(., aes(wday, quantity, col = isoweek)) +
        geom_bar() + xlab("savaitės diena") + ylab("Kiekis, MW") +
        ggtitle("Elektros tinklų apkrova Lietuvoje, pask. 30 dienų savaitiniai duomenys (vidurkiai), MW", subtitle = "Duomenys: transparency.entsoe.eu, skaičiavimai: corona-stat.lt")

en_load_day_ahead_total_load_forecast(eic = "10YLT-1001A0008Q", period_start = lubridate::ymd(as.character(Sys.Date() - 30), tz = "UTC"),
                                                 period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC"), security_token = securityToken) %>%
                mutate(dt = lubridate::floor_date(dt, "hours")) %>%
                group_by(dt) %>%
                summarise(quantity = mean(quantity)) %>%
                ungroup() %>%
                mutate(dt = lubridate::floor_date(dt, "days")) %>%
                group_by(dt) %>%
                summarise(quantity = mean(quantity)) %>%
                ungroup() %>%
                mutate(wday = lubridate::wday(dt, week_start = 1), isoweek = as.factor(lubridate::isoweek(dt))) %>%
                ggplot(., aes(wday, quantity, col = isoweek)) +
                geom_line() + xlab("savaitės diena") + ylab("Kiekis, MW") +
                ggtitle("Elektros tinklų apkrovos prognozė, MW", subtitle = "Duomenys: transparency.entsoe.eu, skaičiavimai: corona-stat.lt")


