### script to draw sampling frame as input of ODK for baseline data collection and treatments
### Bjorn Van Campenhout August 12th 2024

#run in /RCT/sampling

# load libraries
library(haven)
library(randomizr)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(clubSandwich)

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
set.seed(27082024) # Set seed for reproducibility
village_assignments <- sample(village_assignments)

# Create a data frame to store village assignments
village_treatment <- data.frame(village = unique_villages, treatment = village_assignments)

# Merge treatment assignments back into the original household data
data <- merge(data, village_treatment, by = "village")

write.csv(data[c("village","farmer_ID","land_size_ha")], file="sample.csv")
# Set the number of households to select per village - make sure this can be divided by 2 for stratification
n_households_per_village <- 18

###we randomly sampled 16 hosueholds per village
sampling_frame <- data %>%
  group_by(village) %>%
  sample_n(n_households_per_village, replace = FALSE)

summary(sampling_frame$q19)
## alternative: if we want to make sure we have sufficient large and small households in our sample, why not take 8 largest and then rest random?

# library(dplyr)
 
# Select 8 largest households by land size within each village
# selected_households <- data %>%
#   group_by(village) %>%
#   arrange(desc(land_size_ha)) %>%
#   slice(1:(n_households_per_village/2)) %>%
#   ungroup()
# 
# # Select 8 random households from the remaining ones in each village
# remaining_households <- data %>%
#   group_by(village) %>%
#   arrange(desc(land_size_ha)) %>%
#   slice(-(1:(n_households_per_village/2))) %>%
#   sample_n((n_households_per_village/2), replace = FALSE) %>%
#   ungroup()
# 
# # Combine the two sets of households
# final_selected_households <- bind_rows(selected_households, remaining_households)

 sampling_frame <- data.frame(sampling_frame)
 
 
###prepare for baseline table
 sampling_frame$age_head <- NA
 sampling_frame$q17 <- as.numeric(sampling_frame$q17)
 sampling_frame$q17[sampling_frame$q17 == 999] <- NA
 sampling_frame$q14 <- as.numeric(sampling_frame$q14)
 sampling_frame$q14[sampling_frame$q14 == 999] <- NA
 
 # if age of HH head is not filled, than age head is age of respondent,  
 sampling_frame$age_head <- ifelse(is.na(sampling_frame$q17),sampling_frame$q14 , sampling_frame$q17) 
 
 
 sampling_frame$male_head <- NA

 sampling_frame$q16[sampling_frame$q16 == "n/a"] <- NA
 sampling_frame$q16 <- sampling_frame$q16 =="Male"
 sampling_frame$q13[sampling_frame$q13 == "n/a"] <- NA
 sampling_frame$q13 <- sampling_frame$q13 == "Male"
 
 # if sex of HH head is not filled, than sex head is sex of respondent,  
 sampling_frame$male_head <- ifelse(is.na(sampling_frame$q16),sampling_frame$q13 , sampling_frame$q16) 
 ### HH size
 sampling_frame$HH_size <-  sampling_frame$q19 
 ### pov indicator
 sampling_frame$poor <- NA
 sampling_frame$poor <- sampling_frame$q69 == 4 | sampling_frame$q69 == 5

 sampling_frame$gps_latitude <- as.numeric(as.character(sampling_frame$gps_latitude))
 sampling_frame$gps_longitude <- as.numeric(as.character(sampling_frame$gps_longitude))
 
sampling_frame <- sampling_frame[c("q1","q2","q3", "village","name", "farmer_ID" ,  "q11","age_head","male_head","HH_size","land_size_ha", "poor","treatment", "gps_latitude", "gps_longitude")]

sampling_frame$type <- "farmer"
### add agroshops to sample frame
agro_shops <- read.csv("shops_gps.csv")
agro_shops$type <- "agro_shop"
names(agro_shops) <- c("farmer_ID","gps_latitude", "gps_longitude","type")

library(dplyr)
mapper <- full_join(sampling_frame, agro_shops, by = c("farmer_ID","gps_latitude", "gps_longitude","type"))

mapper$treatment[is.na(mapper$treatment)] <- "shop"
##create a map for us

greenLeafIcon <- makeIcon(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "https://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)


pal <- colorFactor(c("green", "red", "blue"),mapper$treatment)

map <-  leaflet() %>% setView(lat =mean(mapper$gps_latitude,na.rm=T), lng = mean(mapper$gps_longitude,na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=mapper[mapper$treatment != "shop",], lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),radius= 2,   color = ~pal(treatment), popup = ~as.character(farmer_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))  %>%   addMarkers(data=mapper[mapper$treatment == "shop",], lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),icon = greenLeafIcon,popup = ~as.character(farmer_ID))



saveWidget(map, file="S2P_sampling_team.html",selfcontained = TRUE) 
write.csv(sampling_frame, file = "sample_team.csv", row.names = FALSE)

### map and sampling frame T1 T2
sample_treats <- subset(sampling_frame, treatment %in% c("T1","T2"))

pal <- colorFactor(c("red", "green"),sample_treats$treatment)

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(sample_treats$gps_latitude)),na.rm=T), lng = mean(as.numeric(as.character(sample_treats$gps_longitude)),na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=sample_treats, lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),radius= 2,   color = ~pal(treatment), popup = ~as.character(farmer_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="S2P_sampling_T1_T2.html",selfcontained = TRUE) #traders and farmers 
write.csv(sample_treats, file = "sample_treats.csv", row.names = FALSE)


### create a map for meridian by aggregating 
sampling_frame$treatment_nr <- NA

sampling_frame$treatment_nr[sampling_frame$treatment == "T1"] <- 1
sampling_frame$treatment_nr[sampling_frame$treatment == "T2"] <- 2
sampling_frame$treatment_nr[sampling_frame$treatment == "C"] <- 3

gps <- aggregate(cbind(sampling_frame$treatment_nr,as.numeric(sampling_frame$gps_longitude),as.numeric(sampling_frame$gps_latitude)),by=list(sampling_frame$q1,sampling_frame$q2, sampling_frame$q3), FUN=mean, na.rm=TRUE)

names(gps) <- c("dist","sub","village","treatment", "long","lat")

pal <- colorFactor(c("black","red", "green"),gps$treatment)

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(gps$lat)),na.rm=T), lng = mean(as.numeric(as.character(gps$long)),na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=gps, lng=~as.numeric(as.character(long)), lat=~as.numeric(as.character(lat)),radius= 2,  color = ~pal(treatment),  , popup = ~as.character(village))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="map_meridian.html",selfcontained = TRUE)

write.csv(gps, file = "village_list_meridian.csv", row.names = FALSE)

### merge in agro-input shops

#### balance tables
##farmer's age (in years), sex of farmer, household size, land area for crop production (acres), and AIP recepient in previous year. We also include five variables from among the outcomes of interest to test balance at baseline, in particular: use of organic fertilizer (yes/no), use of Urea (kg), yield, profit, crop in previous season was legume
### may need to be merged in 

df_balance <- array(NA,dim=c(8,9))
balance_vars <- c("age_head", "male_head","HH_size","land_size_ha","poor")

sampling_frame$treatment1 <- sampling_frame$treatment=="T1" | sampling_frame$treatment=="T2"
sampling_frame$treatment2 <- sampling_frame$treatment=="T2"

#age of HH head
for (i in 1:length(balance_vars)){
  
  df_balance[i,1] <- round(tapply(as.numeric(unlist(sampling_frame[balance_vars[i]])), sampling_frame$treatment, FUN = mean, na.rm=TRUE)[1], digits = 3)
  df_balance[i,2] <- round(tapply(as.numeric(unlist(sampling_frame[balance_vars[i]])), sampling_frame$treatment, FUN = sd, na.rm=TRUE)[1], digits = 3)
  
fit.sim <- lm(as.formula(paste(balance_vars[i],"treatment1+treatment2",sep = "~")), data=sampling_frame)
vcov_cluster <- vcovCR(fit.sim,cluster=sampling_frame$village,type="CR3")
coef_test(fit.sim, vcov_cluster)



df_balance[i,3] <- round(coef_test(fit.sim, vcov_cluster)$beta[2], digits = 3)
df_balance[i,4] <- round(coef_test(fit.sim, vcov_cluster)$SE[2], digits = 3)
df_balance[i,5] <- coef_test(fit.sim, vcov_cluster)$p_Satt[2]


df_balance[i,6] <- round(coef_test(fit.sim, vcov_cluster)$beta[3], digits = 3)
df_balance[i,7] <- round(coef_test(fit.sim, vcov_cluster)$SE[3], digits = 3)
df_balance[i,8] <- coef_test(fit.sim, vcov_cluster)$p_Satt[3]

df_balance[i,9] <-  nobs(fit.sim)
}

##joint orthogonality

### do this with a multinomial model and a Likelihood ration test : -2*L(null model) – (-2*L(fitted model)) 
library(lmtest)
library(car)
library(nnet)

nullMod <- multinom(treatment ~1 , data=na.omit(sampling_frame[ , all.vars(formula(treatment~age_head+male_head+HH_size+land_size_ha+poor))]))
altMod <- multinom(treatment~age_head+male_head+HH_size+land_size_ha+poor , data = sampling_frame)
lrtest(altMod, nullMod)

df_balance[6,1] <-round(lrtest(altMod, nullMod)[2,4],digits=3)
df_balance[6,2] <-round(lrtest(altMod, nullMod)[2,5],digits=3)

##simple F-tests

mod1<- lm((treatment=="T1")~age_head+male_head+HH_size+land_size_ha+poor, data = sampling_frame[sampling_frame$treatment%in%c("T1","C"),])
test_res <- linearHypothesis(mod1, c("age_head=0","male_headTRUE=0","HH_size=0","land_size_ha=0","poorTRUE=0"))


df_balance[7,1] <-round(test_res[2,5],digits=3)
df_balance[7,2] <-round(test_res[2,6],digits=3)

mod1<- lm((treatment=="T2")~age_head+male_head+HH_size+land_size_ha+poor, data = sampling_frame[sampling_frame$treatment%in%c("T1","T2"),])
test_res <- linearHypothesis(mod1, c("age_head=0","male_headTRUE=0","HH_size=0","land_size_ha=0","poorTRUE=0"))


df_balance[8,1] <-round(test_res[2,5],digits=3)
df_balance[8,2] <-round(test_res[2,6],digits=3)


### save this results DF somewhere
saveRDS(df_balance, file=paste(path,"balance_2022.Rdata",sep="/")) 


library(openrouteservice)


#### code to match each village to the nearest agro-dealer using travel by bicycle
### to do this I had to set up open route server instance locally - https://giscience.github.io/openrouteservice/run-instance/
### this was helpful - https://www.youtube.com/watch?v=VQXlbqKArFk&t=291s
### once installed, start the server like this:
###sudo docker compose up -d
###which should return: Container ors-app  Started 

library(httr)
library(jsonlite)

# Assuming you have data frames 'villages' and 'agro_shops' with 'lat' and 'long' columns
villages <- gps
names(agro_shops) <- c("shop", "lat" ,"long","type")
library(geosphere)
options(openrouteservice.url = "http://localhost:8080/ors")

# Function to calculate shortest walking distance using Openrouteservice
get_shortest_walking_distance_ors <- function(lat1, lon1, lat2, lon2) {
  
  coordinates <- list(c(lon1, lat1), c(   lon2 ,lat2 ))
  
  x <- ors_directions(coordinates, radiuses = 1000, ors_profile("bike"))
  
  if (!is.null(x$features[[1]]$properties$summary$distance[1]/1000)) {
    return(x$features[[1]]$properties$summary$distance[1]/1000)
  } else {
    return(NA)  # Handle cases where no route is found
  }
}

distances <- matrix(NA, nrow = nrow(villages), ncol = nrow(agro_shops))



# Calculate distances for each village-agro-shop pair
for (i in 1:nrow(villages)) {
  for (j in 1:nrow(agro_shops)) {
    distances[i, j] <- get_shortest_walking_distance_ors(villages$lat[i], villages$long[i],
                                                         agro_shops$lat[j], agro_shops$long[j])
  }
}



# Find the nearest agro-shop for each village
nearest_shop_index <- apply(distances, 1, which.min)

# Assign each village to its nearest agro-shop
villages$nearest_shop <- agro_shops$shop[unlist(nearest_shop_index)]
villages <- villages[c("dist","sub","village","treatment", "lat", "long", "nearest_shop")]      
### merge in shop gps again
names(agro_shops) <- c("shop", "shop_lat",  "shop_long", "type")
villages <- merge(villages,agro_shops,  by.y = "shop", by.x = "nearest_shop")

villages$treat <- NA
villages$treat[villages$treatment == "1"] <- "T1"
villages$treat[villages$treatment == "2"] <- "T2"
villages$treat[villages$treatment == "3"] <- "C"

villages$treatment <- NULL
villages$type <- NULL

names(villages)[names(villages) == 'long'] <- 'village_long'
names(villages)[names(villages) == 'lat'] <- 'village_lat'
names(villages)[names(villages) == 'sub'] <- 'ta'

write.csv(villages, file = "village_agro_shop_matched.csv", row.names = FALSE)

write.csv(villages[villages$treat!="C",], file = "village_agro_shop_matched_only_T.csv", row.names = FALSE)

###now merge this into the farmer level data to get sampling frame to be uploaded in ODK
sampling_frame$treatment_nr <- NULL
sampling_frame$village <- NULL
sampling_frame$treatment1 <- NULL
sampling_frame$treatment2 <- NULL
sampling_frame$type <- NULL
sampling_frame$poor <- NULL
sampling_frame$age_head <- NULL
sampling_frame$male_head <- NULL
sampling_frame$HH_size <- NULL
sampling_frame$land_size_ha <- NULL
sampling_frame$treatment <- NULL

names(sampling_frame)[names(sampling_frame) == 'q11'] <- 'tel'
names(sampling_frame)[names(sampling_frame) == 'name'] <- 'farmer_name'
names(sampling_frame)[names(sampling_frame) == 'q1'] <- 'dist'
names(sampling_frame)[names(sampling_frame) == 'q2'] <- 'ta'
names(sampling_frame)[names(sampling_frame) == 'q3'] <- 'village'


sampling_ODK  <- merge(sampling_frame,  villages,  by.x = c("dist", "ta","village") , by.y=c("dist","ta",	"village"))
names(sampling_ODK)[names(sampling_ODK) == 'nearest_shop'] <- 'linked_shop'


### merge in shop districts

shops_dist <- read.csv("shops_gps2.csv")

names(shops_dist)[names(shops_dist) == 'dis'] <- 'shop_dist'

sampling_ODK <- merge(sampling_ODK,shops_dist[c("shop","shop_dist","shop_ODK")], by.x="linked_shop",by.y="shop"  )

sampling_ODK <- sampling_ODK[c("shop_dist",  "shop_ODK", "shop_lat", "shop_long", "village", "village_lat",  "village_long","farmer_name","dist","ta","farmer_ID","tel","gps_latitude","gps_longitude","treat") ]
#sorting
sampling_ODK <- sampling_ODK[with(sampling_ODK, order(sampling_ODK[,1], sampling_ODK[,2], sampling_ODK[,5], sampling_ODK[,8])), ]

write.csv(sampling_ODK, file = "sampling_ODK.csv", row.names = FALSE)
write.csv(sampling_ODK[sampling_ODK$treat !="C",], file = "sampling_ODK_only_T.csv", row.names = FALSE)
