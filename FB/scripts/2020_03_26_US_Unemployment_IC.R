# Autorius: Justas Mundeikis
# Data: 2020-03-26
# Atnaujinimas: 
# Aprašymas: JAV nedarbo draudimo išmokų pramšymų skaičius
# Skirta: FB

# Paketai reikalingi kodo veikimui
if(!require("quantmod")) install.packages("quantmod"); library("quantmod")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("zoo")) install.packages("zoo"); library("zoo")

# Duomenų parsiuntimas
getSymbols(c("ICSA", "ICNSA"), src = "FRED", warnings = FALSE)

# Duomenų sujungimas
df <- merge(`ICSA`, `ICNSA`, all = TRUE) 
# DF objekto sukūrimas ir laiko eilutės filtravimas
df <- data.frame(date=as.Date(index(df)), coredata(df))%>%
        gather(var, values, -date)%>%
        filter(year(date)>=2006)


# Grafikas
png("../figures/ICSA_ICNSA.png", width = 16, height = 9, units = 'in', res = 200)
ggplot(df, aes(date,values, col=var))+
                       geom_line(size=1.1)+
        scale_color_brewer(palette = "Set1",name="Variables")+
        scale_x_date(breaks="1 year")+
        geom_hline(yintercept = max(df%>%filter(year(date)<=2010)%>%
                                             select(values)), linetype=2, size=1.1)+
        labs(title="Savaitiniai nedarbo pašalpos prašymai JAV ",
             subtitle="Šaltinis: FRED, skaičiavimai: Corona-Stat.lt",
             x="Data",
             y="Skaičius")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
