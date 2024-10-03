rm(list=ls())
path <- getwd()
path <-  strsplit(path,"/data/clean")[[1]]

dta <- read.csv(paste(path,"data/raw/latest.csv", sep="/"))
dta_list_old <- read.csv(paste(path,"form/sampling_ODK_only_T_old.csv", sep="/"))
dta_list <- read.csv(paste(path,"form/sampling_ODK_only_T.csv", sep="/"))

#if ID == na then we need to get the names from the list -  this is because we changed the list but not the choices tab in the from, so the program did not pull correct data

dta_nas <- subset(dta, dta$Farmer_ID == "n/a") ## get those with nas
dta <- subset(dta, dta$Farmer_ID != "n/a") ### and only keep those with valid ids
dta_nas <-  merge(dta_nas,dta_list_old, by="farmer_name", all.x=TRUE)
dta_nas$Farmer_ID <- dta_nas$farmer_ID
dta_nas$farmer_ID <- NULL

dta_nas$treat.y <- NULL
dta_nas$treat.x <- NULL
dta_nas$village <- dta_nas$village.x
dta_nas$village.x <- NULL
dta_nas$village.y <- NULL

dta_nas$lat <- dta_nas$gps_latitude
dta_nas$gps_latitude <- NULL
dta_nas$long  <- dta_nas$gps_longitude
dta_nas$gps_longitude <- NULL
dta_nas$phone <- dta_nas$tel
dta_nas$tel <- NULL

dta_nas <- dta_nas[names(dta)]
# stack those with new nas to those with valid ids
dta <- rbind(dta_nas, dta)
 


### fix second problem: we started off with a list with wrong names, which we can fix by importing names from the correct list

dta <- merge(dta, dta_list[c("farmer_ID" ,"farmer_name")], by.x="Farmer_ID" , by.y="farmer_ID", all.x=TRUE)



#and if farmer_name.x differs from farmer_name.y, use farmer_name.y
dta$farmer_name <- ifelse(dta$farmer_name.x != dta$farmer_name.y, 
                          dta$farmer_name.y, 
                          dta$farmer_name.x)

dta$farmer_name.x <- NULL
dta$farmer_name.y <- NULL 


### treat seems to be missing - pull from latest list
dta <- merge(dta, dta_list[c("farmer_ID","treat")], by.x = "Farmer_ID", by.y="farmer_ID")

write.csv(dta,paste(path,"", sep="/data/clean/linkfile.csv"), row.names = FALSE)