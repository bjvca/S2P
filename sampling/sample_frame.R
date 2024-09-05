### script to draw sampling frame as input of ODK for baseline data collection and treatments
### Bjorn Van Campenout August 12th 2024

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


sampling_frame <- sampling_frame[c("q1","q2","q3", "village","farmername", "farmer_ID" ,  "q11","age_head","male_head","HH_size","land_size_ha", "poor","treatment", "gps_latitude", "gps_longitude")]

##create a map for us

pal <- colorFactor(c("black","red", "green"),sampling_frame$treatment)

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(sampling_frame$gps_latitude)),na.rm=T), lng = mean(as.numeric(as.character(sampling_frame$gps_longitude)),na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=sampling_frame, lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),radius= 2,   color = ~pal(treatment), popup = ~as.character(farmer_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="S2P_sampling.html",selfcontained = TRUE) #traders and farmers 
write.csv(sampling_frame, file = "sample.csv", row.names = FALSE)

### map and sampling frame for meridian
sample_treats <- subset(sampling_frame, treatment %in% c("T1","T2"))

pal <- colorFactor(c("red", "green"),sample_treats$treatment)

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(sample_treats$gps_latitude)),na.rm=T), lng = mean(as.numeric(as.character(sample_treats$gps_longitude)),na.rm=T), zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=sample_treats, lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),radius= 2,   color = ~pal(treatment), popup = ~as.character(farmer_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="S2P_sampling.html",selfcontained = TRUE) #traders and farmers 
write.csv(sample_treats, file = "sample_treats.csv", row.names = FALSE)


### create a map for meridian by aggregating 
sample_treats$treatment_nr <- NA

sample_treats$treatment_nr[sample_treats$treatment == "T1"] <- 1
sample_treats$treatment_nr[sample_treats$treatment == "T2"] <- 2


gps <- aggregate(cbind(sample_treats$treatment_nr,as.numeric(sample_treats$gps_longitude),as.numeric(sample_treats$gps_latitude)),by=list(sample_treats$q1,sample_treats$q2, sample_treats$q3), FUN=mean, na.rm=TRUE)

names(gps) <- c("dist","sub","village","treatment", "long","lat")

pal <- colorFactor(c("red", "green"),gps$treatment)

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
