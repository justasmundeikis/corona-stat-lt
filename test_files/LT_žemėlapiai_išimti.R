### Užsikrėtimai (abs.)

```{r, echo=FALSE}
df <- data_lt_county_cum%>%
        filter(var=="confirmed",
               date==max(date))%>%
        left_join(.,demo_r_pjangrp, by="NUTS_ID") %>%
        mutate(values_pop=values/pop*100000)

df <- left_join(EU_shape, df, by="NUTS_ID")

min <- round(min(df$values, na.rm = T),1)
max <- round(max(df$values, na.rm = T),1)

my_breaks <- c(min,2,5, 12 ,max)

ggplot(df, aes(fill=values))+
        geom_sf()+
        scale_fill_gradientn(colours=brewer.pal(name ="YlOrRd", n=9),
                             name="Skaičius",
                             trans="log",
                             na.value = "grey100",
                             breaks=my_breaks,
                             labels=my_breaks)+
        geom_sf_label(aes(label =round(values,0)), size=5, fill="white" )+
        labs(title=paste0("Visi patvirinti atvejai (duomenys ",max.date,")"),
             subtitle = "Šaltinis ir skaičiavimai: Corona-Stat.lt",
             x="",
             y="")

```

### Užsikrėtimai (/pop.)
```{r, echo=FALSE}
df <- data_lt_county_cum%>%
        filter(var=="confirmed",
               date==max(date))%>%
        left_join(.,demo_r_pjangrp, by="NUTS_ID") %>%
        mutate(values_pop=values/pop*100000)


df <- left_join(EU_shape, df, by="NUTS_ID")

min <- round(min(df$values_pop, na.rm = T),1)
max <- round(max(df$values_pop, na.rm = T),1)

my_breaks <- c(min, 1.5, 2.5 ,max)

ggplot(df, aes(fill=values_pop))+
        geom_sf()+
        scale_fill_gradientn(colours=brewer.pal(name ="YlOrRd", n=9),
                             name="Skaičius",
                             trans="log",
                             na.value = "grey100",
                             breaks=my_breaks,
                             labels=my_breaks)+
        geom_sf_label(aes(label =round(values_pop,0)), size=5, fill="white" )+
        labs(title=paste0("Visi patvirinti atvejai (duomenys ",max.date,")"),
             subtitle = "Šaltinis ir skaičiavimai: Corona-Stat.lt",
             x="",
             y="")

```


### Aktyvūs (abs.)
```{r, echo=FALSE}
df <- data_lt_county_cum%>%
        filter(var=="active",
               date==max(date))%>%
        left_join(.,demo_r_pjangrp, by="NUTS_ID") %>%
        mutate(values_pop=values/pop*100000)


df <- left_join(EU_shape, df, by="NUTS_ID")

min <- round(min(df$values, na.rm = T),1)
max <- round(max(df$values, na.rm = T),1)

my_breaks <- c(min,2,5, 12 ,max)

ggplot(df, aes(fill=values))+
        geom_sf()+
        scale_fill_gradientn(colours=brewer.pal(name ="YlOrRd", n=9),
                             name="Skaičius",
                             trans="log",
                             na.value = "grey100",
                             breaks=my_breaks,
                             labels=my_breaks)+
        geom_sf_label(aes(label =round(values,0)), size=5, fill="white" )+
        labs(title=paste0("Visi patvirinti atvejai (duomenys ",max.date,")"),
             subtitle = "Šaltinis ir skaičiavimai: Corona-Stat.lt",
             x="",
             y="")

```


### Aktyvūs (/pop.)
```{r, echo=FALSE}
df <- data_lt_county_cum%>%
        filter(var=="active",
               date==max(date))%>%
        left_join(.,demo_r_pjangrp, by="NUTS_ID") %>%
        mutate(values_pop=values/pop*100000)

df <- left_join(EU_shape, df, by="NUTS_ID")

min <- round(min(df$values_pop, na.rm = T),1)
max <- round(max(df$values_pop, na.rm = T),1)

my_breaks <- c(min,1,2 ,max)

ggplot(df, aes(fill=values_pop))+
        geom_sf()+
        scale_fill_gradientn(colours=brewer.pal(name ="YlOrRd", n=9),
                             name="Skaičius",
                             trans="log",
                             na.value = "grey100",
                             breaks=my_breaks,
                             labels=my_breaks)+
        geom_sf_label(aes(label =round(values_pop,0)), size=5, fill="white" )+
        labs(title=paste0("Visi patvirinti atvejai (duomenys ",max.date,")"),
             subtitle = "Šaltinis ir skaičiavimai: Corona-Stat.lt",
             x="",
             y="")

```