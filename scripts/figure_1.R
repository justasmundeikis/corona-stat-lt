data_world <- read.csv("./data/data_world.csv",
                       stringsAsFactors = FALSE)%>%
        mutate(date=as.Date(date))%>%
        mutate(var=factor(var, levels=c("active", "recovered", "deaths")))%>%
        filter(var!="confirmed")%>%
        group_by(date, var)%>%
        summarise(value=sum(value))

max.date <- max(data_world$date)

jpeg("./figures/figure_1.jpeg")
ggplot(data_world, aes(x=date, y=value, fill=var))+
        geom_area(alpha=0.5)+
        scale_fill_manual(values=c("red", "green", "black"))+ 
        scale_x_date(breaks="1 week")+
        scale_y_continuous(breaks =pretty_breaks())+
        labs(title=paste0("COVID-19 (last update ", max.date,")"),
             subtitle = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE",
             x="Date",
             y="Count")+
        theme(legend.title=element_blank(),
              legend.position='bottom',
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()