if(!require("rsdmx")) install.packages("rsdmx"); library("rsdmx") #for work with shapes

# importing data
fatality_age_group <- read.csv("./data/osp/h_fatality_age_group.csv", stringsAsFactors = FALSE, header=TRUE)
county_matrix <- read.csv("./data/osp/h_county_matrix.csv", stringsAsFactors = FALSE)

#importing OSP data
S3R167_M3010203<- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R167_M3010203", dsd = TRUE)
S3R167_M3010203 <- as.data.frame(S3R167_M3010203 , labels = TRUE)

# transforming
df <- S3R167_M3010203%>%
        filter(!(Demogr_amziaus_grM1412 %in% c("g000g999", "gxxx")),
               Lytis==0,
               LAIKOTARPIS=="2020")%>%
        select(1,3,7,9,12)%>%
        rename(age_group=Demogr_amziaus_grM1412_label.lt)%>%
        left_join(., fatality_age_group, by="age_group")%>%
        mutate(value=obsValue*fatality_risk)

df <- left_join(county_matrix, df, by=c("osp_c_n"="savivaldybesRegdb_label.lt"))

df <- df %>% mutate(proc_010=round(0.1*value,0),
                    proc_020=round(0.2*value,0),
                    proc_030=round(0.3*value,0),
                    proc_040=round(0.4*value,0),
                    proc_050=round(0.5*value,0),
                    proc_060=round(0.6*value,0),
                    proc_070=round(0.7*value,0),
                    proc_080=round(0.8*value,0),
                    proc_090=round(0.9*value,0),
                    proc_100=round(1.0*value,0))

        # creating  .csv files (for tables)
table_risk_agg <- df %>%
        select(5,6,8,11:20)%>%
        gather(var, values, 4:13)%>%
        group_by(var)%>%
        summarise(values=sum(values),
                  sum_p=sum(obsValue))%>%
        mutate(Mirtingumas=round(values/sum_p*100,2))%>%
        rename(Scenarijus=var,
               Mirtys=values,
               LT_pop=sum_p)

write.csv(table_risk_agg, "./data/osp/table_risk_agg.csv", row.names = FALSE)


table_risk_age_group <- df %>% select(5,6,11:20)%>%
        gather(var, values, 3:12)%>%
        group_by(Demogr_amziaus_grM1412, age_group, var)%>%
        summarise(values=sum(values))%>%
        spread(var, values)%>%
        rename(Amžiaus_grupė=age_group)%>%
        as.data.frame()%>%
        select(2:12)

write.csv(table_risk_age_group, "./data/osp/table_risk_age_group.csv", row.names = FALSE)


table_risk_county <- df %>% select(1,7,8,11:20)%>%
        gather(var, value,3:12)%>%
        group_by(osp_c_n, var)%>%
        summarize(value=sum(value))%>%
        spread(var, value)%>%
        rename(Savivaldybė=osp_c_n)

write.csv(table_risk_county, "./data/osp/table_risk_county.csv", row.names = FALSE)
