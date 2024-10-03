library(sf)
library(mapview)
#library(geojsonio)
library(sp)
library(sf)

agro_shops <- st_layers("FWL and AGORA Map.kml")

agro_shops <- st_read("FWL and AGORA Map.kml", layer = "FWL Coordinates-Central Region")

mapviewOptions(fgb = FALSE)
agro_shops <- data.frame(agro_shops)
sel_shops <- agro_shops[c("Address","Latitude",  "Longitude") ]

###issue with naming (weird character)
sel_shops[56,1] <- "Suza (Kasungu)"
### also there are 3 in Kasungu that have the same name

write.csv(sel_shops, file = "shops_gps.csv", row.names = FALSE)