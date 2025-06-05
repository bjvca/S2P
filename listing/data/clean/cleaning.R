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
dta_nas$treat <- dta_nas$treat.y
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
dta$treat <- NULL
dta <- merge(dta, dta_list[c("farmer_ID","treat")], by.x = "Farmer_ID", by.y="farmer_ID")

## correcting some duplicates
dta$Farmer_ID[dta$X_uuid  == "faf01a89-e09f-4f8b-af2b-c8a3b42b4e0f"] <- "F_998"
dta$Farmer_ID[dta$X_uuid  == "7fac07b6-a9ae-4b89-b3af-ef70727adfd0"] <- "F_3537"

dta$Farmer_ID[dta$X_uuid  == "a70fc0a5-83ea-4373-afc6-b80646d5a7e1"] <- "F_2702"
dta$farmer_name[dta$X_uuid  == "a70fc0a5-83ea-4373-afc6-b80646d5a7e1"] <- "Florence Banda (F_2702)"

dta$Farmer_ID[dta$X_uuid  == "5088423b-43a2-4cac-91a6-f80e745b7610"] <- "F_3414"
dta$farmer_name[dta$X_uuid  == "5088423b-43a2-4cac-91a6-f80e745b7610"] <- "WATSON KASANTHI (F_3414)"
dta$Farmer_ID[dta$X_uuid  == "9fbe6647-19b3-4db9-987f-6b3f6f6ab0ad"] <- "F_374"

dta <- subset(dta, !(dta$phone_1  == "985769405" & dta$Farmer_ID ==  "F_455"))

dta$Farmer_ID[dta$X_uuid  == "df68dad5-ddf7-4b79-baef-ae6c332c8f09"] <- "F_11"
dta$farmer_name[dta$X_uuid  == "df68dad5-ddf7-4b79-baef-ae6c332c8f09"] <-  "Aubrey Ackison Kuleza"

dta$Farmer_ID[dta$X_uuid  == "97f716a6-150d-4f35-87dc-9de3a5e4fa1c"] <-  "F_686"
dta$Farmer_ID[dta$X_uuid  == "952a21f0-ae17-43a0-ac5a-5b1b7d197f9e"] <-  "F_636"


sum(duplicated(dta$Farmer_ID))  #0 duplicates

## get latest names if farmers were replaced
##if q1 = no
dta$farmer_name[dta$q1=="No"] <- dta$New_farmer_name[dta$q1=="No"] 
dta$New_farmer_name <- NULL

dta$farmer_name[dta$check.dec_maker=="No"] <- dta$check.name_dec_maker[dta$check.dec_maker=="No"] 




column_names <- c(
  "Farmer_ID",
  "farmer_name", "enumerator", "district", "shop_name", 
  "farmer_dist", "TA", "village", "check.maize.mer_farm_ID", 
  "check.maize.phone_1", "check.maize.phone_2", 
  "check.maize._location_latitude", "check.maize._location_longitude", "treat","today"
)


###select variables we want to keep
dta <- dta[column_names]

#and give them sensible names
names(dta) <-  c(  "Farmer_ID",  "farmer_name", "enumerator", "district", "shop_name", 
  "farmer_dist", "TA", "village", "soil_sample_ID", 
  "phone_1", "phone_2", 
  "hh_location_latitude", "hh_location_longitude", "treat","today"
)

##correct some duplicates and problems with soil_sample_IDs

dta$soil_sample_ID[dta$soil_sample_ID == "15066759"] <-  "FWM-ML-52045"
dta$soil_sample_ID[dta$soil_sample_ID == "1672299"] <- "FWM-ML-45381"
dta$soil_sample_ID[dta$soil_sample_ID == "1754087"] <-  "FWM-ML-51189"
dta$soil_sample_ID[dta$soil_sample_ID == "2211619"] <-  "FWM-ML-51859"
dta$soil_sample_ID[dta$soil_sample_ID == "57242686"] <-  "FWM-ML-51329"
dta$soil_sample_ID[dta$soil_sample_ID == "7232617"] <-  "FWM-ML-50947"
dta$soil_sample_ID[dta$soil_sample_ID == "FW&-ML-&0339"] <-  "FWM-ML-51801"

dta$soil_sample_ID[dta$Farmer_ID == "F_692"] <- "FWM-ML-51939"
dta$soil_sample_ID[dta$Farmer_ID == "F_793"] <- "FWM-ML-45560"

### still 2 duplicates
sum(duplicated(dta$soil_sample_ID))
### show duplicates
write.csv(dta[dta$soil_sample_ID %in% dta$soil_sample_ID[duplicated(dta$soil_sample_ID)],],file="dupl_barcodes.csv")

write.csv(dta,paste(path,"", sep="/data/clean/linkfile.csv"), row.names = FALSE)

### now for control
dta_ctrl <- read.csv(paste(path,"data/raw/latest_ctrl.csv", sep="/"))

dta_ctrl$soil_sample_ID <- dta_ctrl$check.maize.mer_farm_ID 

dta_ctrl$treat <- "C"

dta_ctrl <- subset(dta_ctrl, Farmer_ID != "n/a") #drop 6 n/a's
dta_ctrl <- subset(dta_ctrl, Farmer_ID != dta_ctrl$Farmer_ID[duplicated(dta_ctrl$Farmer_ID)])

## get latest names if farmers were replaced
##if q1 = no
dta_ctrl$farmer_name[dta_ctrl$q1=="No"] <- dta_ctrl$New_farmer_name[dta_ctrl$q1=="No"] 
dta_ctrl$New_farmer_name <- NULL

dta_ctrl$farmer_name[dta_ctrl$check.dec_maker=="No"] <- dta_ctrl$check.name_dec_maker[dta_ctrl$check.dec_maker=="No"] 

column_names <- c(
  "Farmer_ID",
  "farmer_name", "enumerator", "district", "shop_name", 
  "farmer_dist", "TA", "village", "soil_sample_ID", 
  "check.maize.phone_1", "check.maize.phone_2", 
  "check.maize._location_latitude", "check.maize._location_longitude", "treat","today"
)


###select variables we want to keep
dta_ctrl <- dta_ctrl[column_names]

#and give them sensible names
names(dta_ctrl) <-  c(  "Farmer_ID",  "farmer_name", "enumerator", "district", "shop_name", 
                   "farmer_dist", "TA", "village", "soil_sample_ID", 
                   "phone_1", "phone_2", 
                   "hh_location_latitude", "hh_location_longitude", "treat","today"
)

dta <- rbind(dta,dta_ctrl)
dta$farmer_name[dta$Farmer_ID =="F_849"] <- "Letala Mathews Phiri"
dta$farmer_name[dta$Farmer_ID =="F_1106"] <- "LIYANA M'BWANA"
dta$farmer_name <- trimws(dta$farmer_name)
dta[] <- lapply(dta, function(x) {
  if (is.character(x)) trimws(x) else x
})
write.csv(dta,paste(path,"", sep="/data/clean/linkfile.csv"), row.names = FALSE)

