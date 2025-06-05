#start from listing






treat <- read.csv("latest.csv")[, c("farmer_dist","TA", "village","farmer_name", "Farmer_ID", "check.maize.phone_1", "check.maize.phone_2","check.maize._location_longitude", "check.maize._location_altitude")]
ctrl <- read.csv("latest_ctrl.csv")[, c("farmer_dist","TA", "village","farmer_name", "Farmer_ID", "check.maize.phone_1", "check.maize.phone_2","check.maize._location_longitude", "check.maize._location_altitude")]
all <- rbind(treat, ctrl)
all$farmer_name[all$New_farmer_name != "n/a"] <- all$New_farmer_name[all$New_farmer_name != "n/a"] 


all <- all[c("farmer_dist","TA", "village","farmer_name", "Farmer_ID", "check.maize.phone_1", "check.maize.phone_2","check.maize._location_longitude")]
write.csv(all, "to_upload.csv")

to_upload <- read.csv("to_upload_edited.csv")
to_upload$X <- NULL
to_upload$check.maize._location_longitude <- NULL
to_upload <- to_upload[!duplicated(to_upload$Farmer_ID), ]
write.csv(to_upload, "to_upload_edited.csv",row.names = FALSE)

linkfile <- read.csv("~/IFPRI Dropbox/Bjorn Van Campenhout/Space2Place/listing/data/clean/linkfile.csv")

to_upload_2 <- merge(to_upload,linkfile[c("Farmer_ID","soil_sample_ID")],by.x="Farmer_ID", by.y="Farmer_ID", all.x=TRUE)

cropnuts <- read.csv("~/IFPRI Dropbox/Bjorn Van Campenhout/Space2Place/listing/data/raw/meridian/cropnuts.csv")

to_upload_3 <-merge(to_upload_2, cropnuts[c("Sample.Code","Farmer.Name","Farmer.Sampler.Phone.Number")],by.x= "soil_sample_ID", by.y="Sample.Code", all.x= TRUE)

to_upload_3$farmer_name[!is.na(to_upload_3$Farmer.Name)] <- to_upload_3$Farmer.Name[!is.na(to_upload_3$Farmer.Name)]
to_upload_3$check.maize.phone_1[!is.na(to_upload_3$Farmer.Name)] <- to_upload_3$Farmer.Sampler.Phone.Number[!is.na(to_upload_3$Farmer.Name)]
to_upload_3 <- to_upload_3[c("soil_sample_ID", "Farmer_ID","farmer_dist", "TA","village", "farmer_name", "check.maize.phone_1", "check.maize.phone_2")] 
names(to_upload_3) <- c("soil_sample_ID", "Farmer_ID","farmer_dist", "TA","village", "farmer_name", "farmer_phone1","farmer_phone2")

write.csv(to_upload_3, "final_list.csv", row.names = FALSE)