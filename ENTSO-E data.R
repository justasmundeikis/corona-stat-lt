# https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_authentication_and_authorisation
# https://github.com/krose/entsoeR/


install.packages("remotes")
remotes::install_github("krose/entsoeR")
library(entsoeR)

ENTSOE_PAT = "<SECURITY_TOKEN>"

data <- entsoeR::entsoe_all_params()

entsoeR::load_get(documentType = "A65", 
                  processType = "A16", 
                  periodStart = "201702012300", 
                  periodEnd = "201702172300", 
                  outBiddingZone_Domain = "10YCZ-CEPS-----N")
?load_get

entsoeR::entsoe_all_params()


total_load_2019 <- read.csv("data/Total Load - Day Ahead _ Actual_201901010000-202001010000.csv")
total_load_2020 <- read.csv("data/Total Load - Day Ahead _ Actual_202001010000-202101010000.csv")

plot(total_load_2019$Actual.Total.Load..MW....BZN.LT)
plot(total_load_2019$Day.ahead.Total.Load.Forecast..MW....BZN.LT)
plot.ts(total_load_2019$Actual.Total.Load..MW....BZN.LT)
plot.ts(total_load_2019$Day.ahead.Total.Load.Forecast..MW....BZN.LT)

ts.plot(total_load_2020$Actual.Total.Load..MW....BZN.LT[24])

library(forecast)

load <- msts(total_load_2020$Actual.Total.Load..MW....BZN.LT, start = c(2020,1), seasonal.periods = c(8760, 52))
fit <- tbats(load)
plot(forecast(fit))
plot(load)

?tbats
?ts
?msts


ts.plot(load[1:168])
ts.plot(load[168:336])
ts.plot(load[336:504])
ts.plot(load[504:672])
ts.plot(load[672:840])
ts.plot(load[840:1008])
ts.plot(load[1008:1176])
ts.plot(load[1176:1344])
ts.plot(load[1344:1512])
ts.plot(load[1512:1680])
ts.plot(load[1680:1848])
ts.plot(load[1848:2016])

