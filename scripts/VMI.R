if(!require("tidyverse")) install.packages("tidyverse", dep = T); library("tidyverse")
if(!require("rvest")) install.packages("rvest", dep = T); library("rvest")

VMI <- data.frame(comp_name=as.character(),
                  comp_id=as.character(),
                  comp_vmi_info=as.character(),
                  stringsAsFactors = FALSE)

for (i in 1:3229) {
# vreating html url
url <-paste0("https://www.vmi.lt/cms/informacija-apie-mokesciu-moketojus?p_auth=uJY5nkic&p_p_id=taxespayersportlet_WAR_eskisliferayportlet&p_p_lifecycle=1&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_pos=1&p_p_col_count=5&_taxespayersportlet_WAR_eskisliferayportlet_fingerprint=81881CDBEBDF9EF51DF47B27E42A82E4A34BABAF&_taxespayersportlet_WAR_eskisliferayportlet_searchfield=groups&_taxespayersportlet_WAR_eskisliferayportlet_group_codes=75&_taxespayersportlet_WAR_eskisliferayportlet_page=",
i,
"&_taxespayersportlet_WAR_eskisliferayportlet_javax.portlet.action=search")

# reading url
html <- read_html(url, encoding = "utf-8")

# creating a dataframe of ads
W <- html %>%{
        data.frame(
                comp_name = html_nodes(., xpath ='//*[(@id = "_taxespayersportlet_WAR_eskisliferayportlet_results2")]//td[1]')%>% html_text(),
                comp_id = html_nodes(., xpath ='//*[(@id = "_taxespayersportlet_WAR_eskisliferayportlet_results2")]//td[2]')%>% html_text(),
                comp_vmi_info = html_nodes(., xpath ='//*[(@id = "_taxespayersportlet_WAR_eskisliferayportlet_results2")]//td[1]//a')%>%html_attr("href")
         
        )
}

VMI <- rbind(VMI,W)
}



                # transforming date of ad
VMI <- VMI %>%
        mutate(comp_name=as.character(comp_name),
               comp_id=as.character(comp_id),
               comp_vmi_info=as.character(comp_vmi_info))

write.csv(VMI, "VMI.csv")