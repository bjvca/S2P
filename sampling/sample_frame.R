### script to draw sampling frame as input of ODK for baseline data collection and treatments
### Bjorn Van Campenout August 12th 2024

#run in /RCT/sampling

# load libraries
library(haven)
library(randomizr)
library(dplyr)
library(leaflet)

# work with relative paths
path <- getwd()

#use sampling frame from our market participation study
data <- read.csv(paste(path,"data/full_data/baseline_raw.csv", sep="/"))

data$treatment <- NULL

data$q54a <- as.numeric(data$q54a) 
data$q54a[data$q54a >=100] <- NA

data$q58a <- as.numeric(data$q58a) 
data$q58a[data$q58a >=100] <- NA

data$q62a <- as.numeric(data$q62a) 
data$q62a[data$q62a >=100] <- NA

data$land_size <- rowSums(cbind(as.numeric(data$q54a), as.numeric(data$q58a), as.numeric(data$q62a)), na.rm=TRUE)
data$land_size[data$land_size>50] <- NA
data$land_size_ha <- data$land_size/2.471

data$q19[data$q19>15] <- NA

data$land_per_capita <-  data$land_size_ha/data$q19

data$smallholder <-data$land_per_capita < 0.25
###this is for stratification - 31 percent are large farmers
prop.table(table(data$smallholder))

set.seed(120824)
#how many villages do we have? 113
length(table(data$q3))

### treatment assignment - 30 percent in control and the rest equally split into T1 and T2.
### create unique village ID
data$village <- paste(paste(data$q1, data$q2, sep="_"), data$q3, sep="_")

# Get the unique villages
unique_villages <- unique(data$village)
n_villages <- length(unique_villages)

# Calculate the number of villages in each group
n_control_villages <- round(0.30 * n_villages)
n_treatment1_villages <- round(0.35 * n_villages)
n_treatment2_villages <- n_villages - n_control_villages - n_treatment1_villages

# Assign treatment at the village level
village_assignments <- c(rep("C", n_control_villages), 
                         rep("T1", n_treatment1_villages), 
                         rep("T2", n_treatment2_villages))

# Shuffle the assignments randomly
set.seed(15082024) # Set seed for reproducibility
village_assignments <- sample(village_assignments)

# Create a data frame to store village assignments
village_treatment <- data.frame(village = unique_villages, treatment = village_assignments)

# Merge treatment assignments back into the original household data
data <- merge(data, village_treatment, by = "village")

# Set the number of households to select per village
n_households_per_village <- 16

sampling_frame <- data %>%
  group_by(village) %>%
  sample_n(n_households_per_village, replace = FALSE)

sampling_frame <- data.frame(sampling_frame )

sampling_frame <- sampling_frame[c("q1","q2","q3", "farmername", "farmer_ID" ,  "q11", "treatment", "gps_latitude", "gps_longitude")]

##create a map

pal <- colorFactor(c("black","red", "green"),sampling_frame$treats)

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(sampling_frame$gps_latitude)),na.rm=T), lng = mean(as.numeric(as.character(sampling_frame$gps_longitude)),na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=sampling_frame, lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),radius= 2,   color = ~pal(treatment), popup = ~as.character(farmer_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="S2P_sampling.html",selfcontained = TRUE) #traders and farmers 
write.csv(sampling_frame, file = "sample.csv", row.names = FALSE)


### create a map for meridian by aggregating 

gps <- aggregate(cbind(as.numeric(sampling_frame$gps_longitude),as.numeric(sampling_frame$gps_latitude)),by=list(sampling_frame$q1,sampling_frame$q2, sampling_frame$q3), FUN=mean, na.rm=TRUE)

names(gps) <- c("dist","sub","village","long","lat")

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(gps$lat)),na.rm=T), lng = mean(as.numeric(as.character(gps$long)),na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=gps, lng=~as.numeric(as.character(long)), lat=~as.numeric(as.character(lat)),radius= 2,  , popup = ~as.character(village))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="map_meridian.html",selfcontained = TRUE)
