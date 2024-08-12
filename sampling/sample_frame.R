### script to draw sampling frame as input of ODK for baseline data collection and treatments
### Bjorn Van Campenout August 12th 2024

#run in /RCT/sampling

# load libraries
library(haven)
library(randomizr)

# work with relative paths
path <- getwd()

# load in all the AIP data
raw_dowa <-read.csv(paste(path,"data/full_data/dowa.csv", sep="/"))
raw_kasungu <-read.csv(paste(path,"data/full_data/kasungu.csv", sep="/"))
raw_ntchisi <-read.csv(paste(path,"data/full_data/ntchisi.csv", sep="/"))
raw_mchinji <-read.csv(paste(path,"data/full_data/mchinji.csv", sep="/"))
names(raw_mchinji) <- names(raw_kasungu)
names(raw_dowa) <- names(raw_kasungu)


### cleaning data
raw_dowa[c("district","ta","gvh","village")] <- lapply(raw_dowa[c("district","ta","gvh","village")], function(x) toupper(as.character(x) ))
raw_kasungu[c("district","ta","gvh","village")] <- lapply(raw_kasungu[c("district","ta","gvh","village")], function(x) toupper(as.character(x) ))
raw_ntchisi[c("district","ta","gvh","village")] <- lapply(raw_ntchisi[c("district","ta","gvh","village")], function(x) toupper(as.character(x) ))
raw_mchinji[c("district","ta","gvh","village")] <- lapply(raw_mchinji[c("district","ta","gvh","village")], function(x) toupper(as.character(x) ))

raw_dowa[c("district","ta","gvh","village")] <- lapply(raw_dowa[c("district","ta","gvh","village")], function(x) trimws(as.character(x) ))
raw_kasungu[c("district","ta","gvh","village")] <- lapply(raw_kasungu[c("district","ta","gvh","village")], function(x) trimws(as.character(x) ))
raw_ntchisi[c("district","ta","gvh","village")] <- lapply(raw_ntchisi[c("district","ta","gvh","village")], function(x) trimws(as.character(x) ))
raw_mchinji[c("district","ta","gvh","village")] <- lapply(raw_mchinji[c("district","ta","gvh","village")], function(x) trimws(as.character(x) ))

data <- rbind(raw_dowa,raw_kasungu,raw_ntchisi,raw_mchinji )

names(data)
nrow(data)

#just to make sure, we need to remove villages wiht less than 40 households - this can change now that we only implement 
data$ones <- 1
hh_size_per_village <- data.frame(aggregate(ones~district+ta+gvh+village,data=data, FUN=sum))
#get village names of villages with more than 35 hh
 
hh_size_per_village <- subset(hh_size_per_village, ones>35)

data <- merge(hh_size_per_village, data, by = c("district","ta","gvh","village"))
data$ones.x <- NULL
data$ones.y <- NULL


# randomly select 62 villages - let us do 65 for attrition
set.seed(120824)

sampling_frame <- data[sample(1:dim(data)[1], size=65),]
sampling_frame  <- sampling_frame[,c("district","ta","gvh","village")] 

### check if there are duplicates
sum(duplicated(sampling_frame))
#one are duplicated - 
 
#for the non duplicated, randomly select 30 in each village and then allocate 10 to control 8 to T1, 8 to T2, 2 to T3, and 2 to T4.

#not sure why this was there...
#sampling_frame_1 <- subset(sampling_frame, !(district == "DOWA" & ta=="CHIWERE" & gvh=="KALUMA" & village =="CHINKHADZE" ))

#initialize sample_names:
sample_names <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(sample_names) <- c("district","ta","gvh","village","Add","epa","section","asp", "person.type", "person.name", "NID", "sex","treat")  


for (i in (1:nrow(sampling_frame))) {
df <- data[as.character(data$district) == as.character(sampling_frame$district[i])  & as.character(data$ta) == as.character(sampling_frame$ta[i]) & as.character(data$gvh) == as.character(sampling_frame$gvh[i]) & as.character(data$village) == as.character(sampling_frame$village[i]),]

sample_names_int <- cbind(df[sample(nrow(df), 30),],c(rep("C",times=10),rep("T1",times=8),rep("T2",times=8),rep("T3",times=2),rep("T4",times=2)))
colnames(sample_names_int) <- c("district","ta","gvh","village","Add","epa","section","asp", "person.type", "person.name", "NID", "sex","treat")  
sample_names <- rbind(sample_names, sample_names_int)
print(i)
}



#just scramble them to make sure there is not a predictable order in the treatments in the ODK app
all <- sample_names[sample(1:dim(sample_names)[1], size=nrow(sample_names)),]

#now order them again on district ta, gvh and village

all <- all[with(all, order(all$district, all$ta, all$gvh, all$village)),]

#add unique farmer ID dhere

all$farmer_ID <- paste("F",1:nrow(all), sep="_")

all$person.name <-  trimws(as.character(all$person.name))
all$unique_ID <- paste(paste(all$person.name,all$farmer_ID, sep=" ("),")", sep="")

#save csv output
write.csv(all[,c("farmer_ID","district", "ta", "gvh", "village", "treat")], file = paste(path,"sampling_frame_ODK.csv", sep="/"), row.names=F)
write.csv(all[,c("farmer_ID","district", "ta", "gvh", "village", "person.type", "person.name", "NID", "sex","treat","unique_ID")], file = paste(path,"sampling_frame_ODK_names.csv", sep="/"), row.names=F)


### sample list for Meridian:

soil_tests <- subset(all, treat %in% c("T1","T3","T4"))

write.csv(soil_tests[,c("farmer_ID","district", "ta", "gvh", "village", "person.type", "person.name", "NID", "sex","treat","unique_ID")], file = paste(path,"sampling_frame_soil_tests.csv", sep="/"), row.names=F)



