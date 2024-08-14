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
### keep only villages that have more than 30 observations
village_counts <- table(data$q3)
data <- data[data$q3 %in% names(village_counts[village_counts >= 30]), ]
#and sample only 30 households per village
df_sampled <- data %>%
  group_by(q3) %>%
  sample_n(size = 30) %>%
  ungroup()
data <- data.frame(df_sampled)


#select 17 villages as pure control and remove them from the sample
pure_ctrl <- sample(names(table(data$q3)), size=22)
pure_ctrl_data <- subset(data, q3 %in% pure_ctrl)

#from remainder, select 62 villages
sampling_frame <-  subset(data, !(q3 %in% pure_ctrl))
sampling_frame_names <- sample(names(table(sampling_frame$q3)), size=62)
sampling_frame <- subset(sampling_frame, q3 %in% sampling_frame_names)

### check if there are duplicates
sum(duplicated(sampling_frame))

### treatment assignment

subset_strat_1 <- subset(sampling_frame,smallholder == TRUE & sampling_frame$q13 == "Male")
treats <- complete_ra(N=dim(subset_strat_1)[1],prob_each = c(0.26,0.26,0.065,0.065, 1-2*0.065-2*0.26))
subset_strat_1 <-data.frame(subset_strat_1,treats)

subset_strat_2 <- subset(sampling_frame,smallholder == FALSE & sampling_frame$q13 == "Male")
treats <- complete_ra(N=dim(subset_strat_2)[1],prob_each = c(0.26,0.26,0.065,0.065, 1-2*0.065-2*0.26))
subset_strat_2 <-data.frame(subset_strat_2,treats)

subset_strat_3 <- subset(sampling_frame,smallholder == TRUE & sampling_frame$q13 == "Female")
treats <- complete_ra(N=dim(subset_strat_3)[1],prob_each = c(0.26,0.26,0.065,0.065, 1-2*0.065-2*0.26))
subset_strat_3 <-data.frame(subset_strat_3,treats)

subset_strat_4 <- subset(sampling_frame,smallholder == FALSE & sampling_frame$q13 == "Female")
treats <- complete_ra(N=dim(subset_strat_4)[1],prob_each = c(0.26,0.26,0.065,0.065, 1-2*0.065-2*0.26))
subset_strat_4 <-data.frame(subset_strat_4,treats)

sampling_frame <- rbind(subset_strat_1, subset_strat_2, subset_strat_3, subset_strat_4)
levels(sampling_frame$treats)[levels(sampling_frame$treats) == "T5"] <- "C1"

pure_ctrl_data$treats <- "C2"

sampling_frame <- rbind(sampling_frame, pure_ctrl_data)


## OK, this seems reasonable - export only the relevant 
sampling_frame <- sampling_frame[c("q1","q2","q3", "farmername", "farmer_ID" ,  "q11", "treats", "gps_latitude", "gps_longitude")]

##create a map

pal <- colorFactor(c("red", "orange","blue","green","grey","black"),sampling_frame$treats)

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(sampling_frame$gps_latitude)),na.rm=T), lng = mean(as.numeric(as.character(sampling_frame$gps_longitude)),na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=sampling_frame, lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),radius= 2,   color = ~pal(treats), popup = ~as.character(farmer_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="S2P_sampling.html",selfcontained = TRUE) #traders and farmers 
write.csv(sampling_frame, file = "sample.csv", row.names = FALSE)
