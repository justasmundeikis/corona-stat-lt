library(gsheet)
library(tidyverse)

URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQWGlrkMV1SH6Eiurc4LBy1fProHbqf3yHfztaIdVMfZqfQkljGZcHj482LS40jrexhQHjnKRrQADG2/pub?gid=1906964200&single=true&output=csv"
data_lt <- read.csv(text=gsheet2text(URL, format = "csv", sheetid = "1906964200"),
                    stringsAsFactors=FALSE,
                    na.strings = "")%>%
        select(1:10)

                


df <- read.csv("./data/data_world.csv",
               stringsAsFactors = FALSE)%>%
        filter(var%in%c("deaths","confirmed"))%>%
        mutate(date=as.Date(date))%>%
        spread(var,value)%>%
        mutate(deathrate=deaths/confirmed*100)%>%
        mutate_at(vars(deathrate),funs(round(.,2)))

maxdate <- max(df$date)

        dfx <- filter(df,date==maxdate)%>%
                arrange(.,desc(confirmed))
        
                %>%
                top_n(.,10, confirmed)


        
df <- data_lt_add%>%
                mutate(checked=round(tested_total/observed*100,1))%>%
                .[!is.na(.$checked),] %>% as.data.frame()
        
        kable(df)       

