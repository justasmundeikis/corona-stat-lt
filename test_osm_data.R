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

