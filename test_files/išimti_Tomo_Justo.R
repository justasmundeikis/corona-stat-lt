Experimentiniai grafikai:
        
        ```{r}

df <- data_world %>%
        filter(var=="confirmed",
               value>=100)%>%
        group_by(valstybe)%>%
        arrange(date)%>%
        mutate(index = value/value[1],
               time_0=seq_along(along.with = date))%>%
        filter(
                # CNTR_CODE %in% c("CN", "IT", "DE", "KR", "ES", "IR", "JP", "EE"),
                time_0<=40)


ggplot(df, aes(time_0, value,  group=valstybe))+
        geom_line()+
        scale_y_continuous(trans = log2_trans())+
        scale_color_brewer(palette="Set1",type = "qualitative", name="Valstybės")+
        labs(title="Registruotų atvejų skaičiaus raida nuo momento, kai šalyje pasiekta 100 atvejų",
             subtitle = "Šaltinis: JHCSSE, skaičiavimai: Corona-Stat.lt",
             x="Dienos 0+t",
             y="Skaičius (log2)")+
        theme(legend.title=element_blank(),
              legend.position="left",
              axis.text.x = element_text(angle = 45, hjust = 1))


```


Užsikrėtimų pokytis paskelbus karantiną

```{r, echo=FALSE}
df <- data_world %>%
        filter(var=="confirmed",
               CNTR_CODE %in% c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO"))%>%
        spread(date, value)%>%
        arrange(desc(.[,ncol(.)]))%>%
        filter(valstybe %in% c(.$valstybe[1:5], "Estija", "Latvija", "Lenkija", "Lietuva"))%>%
        gather(date, value, 5:ncol(.))%>%
        mutate(date=as.Date(date)) %>%
        group_by(valstybe) %>%
        mutate(diff = value - lag(value, default = first(value))) %>%
        select (valstybe,date, diff)

# čia reikia sukurti atskirą DF, geriausia google sheete ir importuot, nes reikia visoms šalims

dff <- data.frame(valstybe=c("Airija", "Austrija", "Belgija", "Čekija", "Danija", 
                             "Estija", "Graikija", "Ispanija", "Islandija", "Italija", "Kipras", "Kroatija", "Latvija", "Lenkija", "Lietuva", "Liuksemburgas", "Nyderlandai", "Norvegija",
                             "Portugalija", "Prancūzija", "Rumunija", "Slovakija", "Slovėnija", "Suomija", "Vengrija", "Vokietija"),
                  diena=c("2020-03-13", "2020-03-11","2020-03-18", "2020-03-16","2020-03-13", "2020-03-12","2020-03-16", "2020-03-14","2020-03-16", "2020-03-11","2020-03-11",
                          "2020-03-13","2020-03-17", "2020-03-11","2020-03-16", "2020-03-13","2020-03-11", "2020-03-12","2020-03-12", "2020-03-16","2020-03-16", "2020-03-16","2020-03-20", "2020-03-16"
                          ,"2020-03-13", "2020-03-15"))%>%
        mutate(diena=as.Date(as.character(diena)))

df <- left_join(df,dff)

max.date<-max(df$date)

ggplot(df, aes(x=date, y=diff)) +
        geom_area(alpha=0.5, color="darkred", fill="red")+
        scale_x_date()+
        labs(title=paste0('Covid-19 Užsikrėtimų pokytis TOP 5 ir Baltijos valstybėse įvedus karantiną ',
                          max.date), 
             subtitle="Šaltinis: JHCSSE, skaičiavimai: Corona-Stat.lt", 
             x="Data", 
             y="Užsikrėtimų skaičius")+
        facet_wrap(~valstybe, ncol=3, scales='free_y')+
        geom_vline(data = df, mapping = aes(xintercept = diena), color="blue" , size=1.1)
```


## Lietuva{.tabset .tabset-fade .tabset-pills}
### BVP
### Pramonės produkcija
### Užsienio prekyba
### Mažmeninė preyba
### Infliacija
### Kreditai
### Užimtumas / nedarbas

## Pagrindinės ES šalys{.tabset .tabset-fade .tabset-pills}
### BVP
### Pramonės produkcija
### Užsienio prekyba
### Mažmeninė preyba
### Infliacija
### Kreditai
### Užimtumas / nedarbas