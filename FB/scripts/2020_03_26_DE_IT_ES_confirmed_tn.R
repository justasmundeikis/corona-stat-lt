# Autorius: Justas Mundeikis
# Data: 2020-03-26
# Atnaujinimas: 
# Aprašymas: COVID atvejų raida po 10 atvejo
# Skirta: FB

# Paketai reikalingi kodo veikimui
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")

# duomenų importas
data_world <- read.csv("../../data/data_world.csv",header = TRUE,stringsAsFactors = FALSE)%>%
        mutate(date=as.Date(date),
               var=factor(var, levels=c("confirmed", "deaths")))

# duomenų apdorojimas
df <- data_world%>%
        filter(var=="confirmed",
               value>=10)%>%
        group_by(valstybe)%>%
        arrange(date)%>%
        mutate(index = value/value[1],
               time_0=seq_along(along.with = date))%>%
        filter(
                CNTR_CODE %in% c("IT", "DE", "ES", "GB"),
                time_0<=90)
# grafikas
png("../figures/confirmed_IT_ES_DE_tn.png", width = 16, height = 9, units = 'in', res = 200)
ggplot(df, aes(time_0, value,  group=valstybe, col=valstybe))+
        geom_line(size=1.1)+
        scale_x_continuous(breaks=seq(0,100,7))+
        scale_y_continuous(trans = log2_trans())+
        scale_color_brewer(palette="Set1",type = "qualitative", name="Valstybės")+
        labs(title="Registruotų atvejų skaičiaus raida nuo momento, kai šalyje pasiekta ~10 atvejų",
             subtitle = "Šaltinis: JHCSSE, skaičiavimai: Corona-Stat.lt",
             x="Dienos 0+t",
             y="Skaičius (log2)")+
        theme(legend.title=element_blank(),
              legend.position="left",
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
