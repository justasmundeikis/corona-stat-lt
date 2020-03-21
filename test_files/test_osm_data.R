library(sf)
library(osmdata)
library(tidyverse)


osm_lithuania <- opq(bbox = c( 20, 53, 27, 57)) %>%
        add_osm_feature(opq =.,
                        key = "addr:country",
                        value = "LT")%>%
        osmdata_sf()

     unname_osmdata_sf()

osm_lithuania_multipoly <- osm_lithuania$osm_multipolygons
ggplot(osm_lithuania_multipoly)+
        geom_sf()


osm_latvia <- opq(bbox = c( 20, 53, 27, 57)) %>%
        add_osm_feature(opq =.,
                        key = 'admin level',
                        value = "4")%>%
        osmdata_sf()

osm_estonia <- opq(bbox = c( 21, 57, 29, 60)) %>%
        add_osm_feature(opq =.,
                        key = 'admin_level',
                        value = "6")%>%
        osmdata_sf()

osm_lithuania_multipoly <- osm_lithuania$osm_multipolygons

osm_lithuania_multipoly$geometry%>% str()

rpp <- osm_lithuania_multipoly
names(rpp$geometry[[1]][[1]]) <- NULL
ggplot(rpp)+
        geom_sf()









# extract Quartiers
q0 <- opq(bbox = c( 20, 53, 27, 57))
q5 <- add_osm_feature(opq = q0, key = 'admin_level', value = "5")
res5 <- osmdata_sf(q5)
quartier <- res5$osm_multipolygons

res6 <- unname_osmdata_sf(quartier)


res5$osm_multipolygons


ggplot(quartier$geometry)+
        geom_sf()

row.names(res5$osm_multipolygons)


plot(quartier$geometry)







```{r, include=FALSE}
# todo
# reikia importą zipo ir failo paėmimą iš zipo padaryt, nes dabar iš working directory ima

# INFO:
# https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2

# URL <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_sovereignty.zip"
# download.file(url=URL, destfile ="./data/ne_50m_admin_0_sovereignty.zip", method = "auto")
# unzip("./data/ne_50m_admin_0_sovereignty.zip", exdir = "./data/")
# countries <- readOGR(dsn="./data/ne_50m_admin_0_sovereignty.shp",
#                              encoding = "utf-8",
#                              use_iconv = T,
#                              verbose = FALSE,
#                      stringsAsFactors = FALSE)
#  countries <- st_as_sf(countries)
#  ggplot(countries)+ geom_sf()

```