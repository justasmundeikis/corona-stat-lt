### Bendras naujų susirgimų kitimas
```{r, echo=FALSE}
df <- data_world_map %>%
        mutate(var=factor(var, levels=c("active", "recovered", "deaths", "confirmed")))

df <- df%>%filter(valstybe%in%c("Airija", "Austrija", "Belgija", "Bulgarija", "Čekija", "Danija", 
                                "JK", "Estija", "Graikija", "Ispanija", "Islandija", "Italija", "Kipras", "Kroatija", "Latvija", "Lenkija", "Lichtenšteinas", "Lietuva", "Liuksemburgas", 
                                "Malta", "Nyderlandai", "Norvegija", "Portugalija", "Prancūzija", "Rumunija", "Slovakija", "Slovėnija", "Suomija", "Švedija", "Vengrija", "Vokietija"), var=="confirmed" )

dff <- df%>%
        group_by(valstybe) %>%
        mutate(diff = value - lag(value, default = first(value))) %>%
        select (valstybe,date, diff)%>%
        group_by(date)%>%
        summarise(diff=sum(diff))

max.date<-max(df$date)

ggplot(dff, aes(x=date, y=diff)) +
        geom_point(size=2,color="black")+
        scale_x_date(breaks="1 week")+
        scale_y_continuous(breaks = seq(0, max(dff$diff), by = 1000))+
        labs(x="Data", y="Susirgimų skaičius", title=paste0('Covid-19 nauji susirgimai Europoje ',max.date+1," duomenimis"),
             subtitle="Šaltinis: www.corona-stat.lt")+
        geom_smooth(fill="red")

```


### Pasveikimai
```{r, echo=FALSE}
df <- data_world_map %>%
        mutate(var=factor(var, levels=c("active", "recovered", "deaths", "confirmed")))%>%
        filter(valstybe%in%c("Airija", "Austrija", "Belgija", "Bulgarija", "Čekija", "Danija", 
                             "JK", "Estija", "Graikija", "Ispanija", "Islandija", "Italija", "Kipras", "Kroatija", "Latvija", "Lenkija", "Lichtenšteinas", "Lietuva", "Liuksemburgas", 
                             "Malta", "Nyderlandai", "Norvegija", "Portugalija", "Prancūzija", "Rumunija", "Slovakija", "Slovėnija", "Suomija", "Švedija", "Vengrija", "Vokietija"), var=="recovered") %>%
        spread(date, value)%>%
        arrange(desc(.[,ncol(.)]))%>%
        .[1:6,]%>%
        gather(date, value, 5:ncol(.))%>%
        mutate(date=as.Date(date))%>%
        select(valstybe, date, value, var)

max.date<-max(df$date)

ggplot(df, aes(x=date, y=value, fill=var)) +
        geom_area(alpha=0.5, color="black", fill="green") +
        labs(title=paste0('Pasveikusių žmonių skaičius TOP 6 Europos valstybėse ', max.date+1), subtitle="Šaltinis: corona-stat.lt",
             x="Data", y="Skaičius") +
        theme(legend.position='bottom',
              axis.text.x=element_text(angle=45, hjust=1)) +
        facet_wrap(~valstybe, ncol=3, scales='free_y', nrow=2)   
```


### Top 5 Valstybės
```{r, echo=FALSE}

df <- data_world_map %>%
        filter(var=="confirmed", valstybe%in%c("Airija", "Austrija", "Belgija", "Bulgarija", "Čekija", "Danija", 
                                               "JK", "Estija", "Graikija", "Ispanija", "Islandija", "Italija", "Kipras", "Kroatija", "Latvija", "Lenkija", "Lichtenšteinas", "Lietuva", "Liuksemburgas", 
                                               "Malta", "Nyderlandai", "Norvegija", "Portugalija", "Prancūzija", "Rumunija", "Slovakija", "Slovėnija", "Suomija", "Švedija", "Vengrija", "Vokietija"))%>%
        spread(date, value)%>%
        arrange(desc(.[,ncol(.)]))%>%
        .[1:5,]%>%
        gather(date, value, 5:ncol(.))%>%
        mutate(date=as.Date(date))

max.date<-max(df$date)

ggplot(df, aes(x=date, y=value, color=valstybe, group=valstybe)) +
        geom_line(size=1.1)+
        geom_point(size=2)+
        scale_color_brewer(palette="Set1",type = "qualitative", name="Valstybės")+
        labs(
                title="Susirgimų skaičius 5-iose daugiausia patvirtintų atvejų turinčiose Europos šalyse ",max.date+1,
                subtitle="Šaltinis:corona-stat.lt",
                x="Laikotarpis",
                y="Susirgimų skaičius"
        )+
        scale_x_date(breaks = "1 week")+
        theme(axis.text=element_text(size=8),
              axis.text.x=element_text(angle=45, hjust=1))

```

### Susirgimų/pasveikimų/mirčių santykis

```{r, echo=FALSE}
df <- df <- data_world_map %>%
        mutate(var=factor(var, levels=c("active", "recovered", "deaths", "confirmed")))%>%
        filter(valstybe%in%c("Airija", "Austrija", "Belgija", "Bulgarija", "Čekija", "Danija", 
                             "JK", "Estija", "Graikija", "Ispanija", "Islandija", "Italija", "Kipras", "Kroatija", "Latvija", "Lenkija", "Lichtenšteinas", "Lietuva", "Liuksemburgas", 
                             "Malta", "Nyderlandai", "Norvegija", "Portugalija", "Prancūzija", "Rumunija", "Slovakija", "Slovėnija", "Suomija", "Švedija", "Vengrija", "Vokietija"), var=="confirmed") %>%
        spread(date, value)%>%
        arrange(desc(.[,ncol(.)]))%>%
        .[1:6,]%>%
        gather(date, value, 5:ncol(.))%>%
        mutate(date=as.Date(date))%>%
        select(valstybe, date, value, var)

names<-as.vector(unique(df$valstybe))
max.date<-max(df$date)

df <- read.csv("./data/data_world.csv",
               stringsAsFactors = FALSE)%>%
        mutate(date=as.Date(date))%>%
        mutate(var=factor(var, levels=c("active", "recovered", "deaths", "confirmed"))) %>% 
        filter(valstybe%in%names, var!="confirmed")%>%
        select(valstybe, date, value, var)%>%
        group_by(valstybe)

ggplot(df, aes(x=date, y=value, fill=var)) +
        geom_area(alpha=0.5) +
        labs(title=paste0("Covid-19 atvejai TOP6 Europos šalyse ( ",max.date,") duomenimis"), 
             subtitle="Šaltinis: JHCSSE, skaičiavimai: Corona-Stat.lt", x="Data", y="Susirgimų skaičius") +
        scale_fill_manual(values=c('red', 'green', 'black'), 
                          labels=c("Aktyvūs","Pasveikę","Mirtys")) +
        theme(legend.title = element_blank(),
              legend.position='bottom',
              axis.text.x=element_text(angle=45, hjust=1)) +
        facet_wrap(~valstybe, ncol=3, scales='free_y', nrow=2)

```



```{r}
data_lt %>%
        na.omit()%>%
        mutate(tested_perc=round(tested_total/observed*100,1))%>%
        select(date,tested_total, observed, tested_perc)%>%
        rename(Data=date,
               "Atlitka testų"=tested_total,
               "Stebimų asm. skaičius"=observed,
               "Ištyrimų proc."=tested_perc)%>%
        kable(escape = F,row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                      full_width = F, 
                      position = "center")

```
