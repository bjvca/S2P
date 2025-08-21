
### this file prepares raw data collected during S2P endline data collection for public release
### it also fixes some duplicates

rm(list = ls())
path <- getwd()
dta <- read.csv("latest.csv")
path <- strsplit(path,"raw")[[1]]

names(dta) <- sub("check.maize.", "",names(dta))
names(dta) <- sub("grp1.", "test_plot.",names(dta))
names(dta) <- sub("grp2.", "rnd_plot.",names(dta))

###  we started with a different form and as a result of variable name change, we need to retreive data from randomly selected plot from a dataset that was exported using a different from
dta_first <- read.csv("latest_first_form.csv")
names(dta_first) <- sub("check.maize.", "",names(dta_first))
names(dta_first) <- sub("groupx1.", "",names(dta_first))
names(dta_first) <- sub("grp1.", "test_plot.",names(dta_first))
names(dta_first) <- sub("grp2.", "rnd_plot.",names(dta_first))

##these are the variables that need to be replaced
garden_vector <- c("hh_size",                                                          
                   "hh_age" ,                                                          
                   "hh_educ"  ,                                                        
                   "hh_gender"  ,                                                      
                   "ttl_area" ,                                                        
                   "ttl_area_t_1"  ,                                                   
                   "AIP_rec"   ,                                                       
                   "AIP_rec_t_1"   ,                                                   
                   "feed_diff" ,                                                       
                   "dist_agro",                                                        
                   "is_FW"    ,                                                        
                   "dist_FWS"   )
### replace them here
dta[dta$X_uuid  %in% dta_first$X_uuid, garden_vector] <- dta_first[dta$X_uuid %in% dta_first$X_uuid,garden_vector]
##fix farmer_IDs
dta$farmer_ID[dta$X_uuid  == "fa431e7b-5aa9-4780-aab1-561e61536de1"] <-  "F_366"
dta$farmer_ID[dta$X_uuid  == "0d84cf30-e5b8-4729-b466-da5fb8db28a7"] <-  "F_1165"
dta$farmer_ID[dta$X_uuid  == "34026839-e833-457c-aac9-1e990d015fa0"] <-  "F_1175"
dta$farmer_ID[dta$X_uuid  == "1212cfc9-b514-417c-9c42-22ce4a1711e2"] <-  "F_1152"
dta$farmer_ID[dta$X_uuid  == "f5a3c30e-989c-4178-8b12-068f2cd18c77"] <-  "F_970"
dta$farmer_ID[dta$X_uuid  == "10ad974e-1abc-43bb-a6fe-95c70f101dcb"] <-  "F_1149"
dta$farmer_ID[dta$X_uuid  == "cb4cc542-3a04-444f-a5ff-7df8ba2217ed"] <-  "F_969"
dta$farmer_ID[dta$X_uuid  == "03e3a7bc-31a7-4d0c-a636-5e9e1a7fb6db"] <-  "F_979"
dta$farmer_ID[dta$X_uuid  == "180f434f-06c7-45eb-96f5-0d16f3c226a3"] <-  "F_964"
dta$farmer_ID[dta$X_uuid  == "ac3c4b5f-fd91-4213-99c5-b4fbcb043ec8"] <-  "F_535"
dta$farmer_ID[dta$X_uuid  == "22304e8e-cffb-4bea-8571-d37647444c0c"] <-  "F_1174"
dta$farmer_ID[dta$X_uuid  == "d45fdfc0-8632-4d23-9db6-6b35420efa74"] <-  "F_1172"
dta$farmer_ID[dta$X_uuid  == "8dfd9ca9-81c4-4f62-af65-e01a8b05a18e"] <-  "F_1157"
dta$farmer_ID[dta$X_uuid  == "eaa0fede-c2f5-45b5-bf59-6b275b48798b"] <-  "F_982"
dta$farmer_ID[dta$X_uuid  == "06925b0d-a542-443c-9b84-a5c51eae4078"] <-  "F_991"
dta$farmer_ID[dta$X_uuid  == "41c548e1-07e0-4e01-89f2-1ce918e16b8c"] <-  "F_983"
dta$farmer_ID[dta$X_uuid  == "4859adbb-b46e-4907-b0b8-f4c29ffbc17a"] <-  "F_389"
dta$farmer_ID[dta$X_uuid  == "ae3f9458-97a8-4427-8a5f-47778b0479f1"] <-  "F_1019"
dta$farmer_ID[dta$X_uuid  == "4e54ae02-34a4-4997-bc78-e90d4964f8ad"] <-  "F_3547"
dta$farmer_ID[dta$X_uuid  == "d48ab590-b22e-449d-8187-e0c996cd5b6e"] <-  "F_3536"
dta$farmer_ID[dta$X_uuid  == "d3365107-2d6b-4ad3-a3d8-e1a62c8a80f4"] <-  "F_528"
dta$farmer_ID[dta$X_uuid  == "4b535dae-2319-4964-970f-6f8345ea767e"] <-  "F_533"
dta$farmer_ID[dta$X_uuid  == "a33cc925-2691-4ebd-b5a2-1eef69e240a0"] <-  "F_974"
dta$farmer_ID[dta$X_uuid  == "36be82a1-aa80-4a02-bb0d-0409ffbd138f"] <-  "F_547"
dta$farmer_ID[dta$X_uuid  == "2de803f6-4bfa-4efd-8e13-1c1ce2ba806a"] <-  "F_531"


write.csv(dta,file=paste(path,"raw/interim.csv",sep="/"), row.names=FALSE)

###how can we resolve duplicate -  for instance F_1067 in KALINDA is a duplicate

###read in baseline data
base <- read.csv("~/IFPRI Dropbox/Bjorn Van Campenhout/Space2Place/sampling/data/full_data/baseline_raw.csv")
### read in list to upload
list <- read.csv("~/IFPRI Dropbox/Bjorn Van Campenhout/Space2Place/sampling/sampling_ODK.csv")

dta$farmer_ID[dta$X_uuid  == "45ad88a9-7b04-4a72-b4fc-cb443696ce84"] <-  "F_1066"
dta <- subset(dta, !(X_uuid  == "d30ec984-5914-4776-99e5-7654ed67251a"))




#these are two records that Thomas submitted and ended up in the data - also remove duplicates
dta <- subset(dta, !(X_uuid %in% c("c609775f-11c7-4177-93ea-61cb72b45b02",
                                   "45ad88a9-7b04-4a72-b4fc-cb443696ce84",
                                   "e3c4c557-69b9-4d9e-b28b-255ab62a451f",
                                   "5ca0b81b-38e5-48f8-9e03-c4deade7a013",
                                   "cc3e9c32-a176-4663-a37e-1b9f98fd9987",
                                   "adb0c507-5ed3-4855-a40c-96c98381b3da",
                                   "fdadda40-87c6-44b6-92cd-c8a00f4a0502", 
                                   "1724d16d-04c0-480c-ba52-b048ea9bd0b7",
                                   "7b5d1ad4-a8a6-4bf7-a9d3-7b71331fd1dc",
                                   "fe9c666b-c3be-4a3d-810c-12c3877698f5",
                                   "0928feb9-c7b7-4cd5-a9a4-bd0f9a2f23ca",
                                   "a331ef94-5027-4d0b-a5f5-7cf23ced937b",
                                   "386b21bd-2f3b-4ed7-8498-3b2fda1ceb6c",
                                   "fbe5691e-9391-484f-abd1-309be6b4dfe3",
                                   "695947d6-e909-461b-8ba5-357c4395e234",
                                   "042cf6c0-52c0-4598-b1c7-ff3816bc319f",
                                   "09e7c79b-0083-417d-90b5-38a12dd9ba29",
                                   "afdac23e-8b24-43bc-a81e-2ed8854c8ce6",
                                   "88d6a7fe-076d-4afd-8a83-fcd5e83a18a3",
                                   "aa8c7b3e-e9c0-4b00-8ca4-1928f0d5e188",
                                   "20b4ec8f-19c3-49c5-ae2c-4d7a2fd0b7eb",
                                   "3faca5de-f889-4d0f-8650-c9dcb079ef59",
                                   "bb9c4504-944a-414f-b284-4422e7e43322",
                                   "b8e88e44-68f4-4c91-9064-0d7afc6d10af",
                                   "b474ca4a-80ad-4475-8a08-abf3260c1c21",
                                   "01bac8b7-8eba-4466-8e3b-7447a0667c5e",
                                   "2949aea3-cbd4-4411-b62d-a7068eeb70d5",
                                   "8c22b7ad-6233-45e2-bd07-05470b01a5fe",
                                   "617db344-e9e4-4e8b-a996-072751e54106",
                                   "d0bb327f-5060-4dc5-aa3d-14a489e329bf",
                                   "cd0ccc2e-f944-4447-ad62-f9af81777412",
                                   "d25116af-270a-4730-a347-ac87518e314b",
                                   "e6e71ed4-9a80-4fa3-93f7-936e0e4536ec")))



dta$cluster <- paste(paste(dta$q1,dta$q2, sep="_"),dta$q3, sep = ")")
# Step 2: Convert to a factor, then to a numeric ID
dta$cluster_id_num <- as.integer(factor(dta$cluster))

# Step 3: Create final cluster ID with "C_" prefix
dta$cluster_id <- paste0("C_", dta$cluster_id_num)

to_drop <- c("start", "end", "deviceid", "simserial", "phonenumber", "subscriberid", "enumerator", "district", "cluster","q2","q3", "farmer_name", 
             "phone1", "phone2", "nick", "lat", "long", "plot.1..plot_name", "plot.2..plot_name", "plot.3..plot_name", "plot.4..plot_name", "plot.5..plot_name","plot_select_name","test_plot.plot_samp",
              "location", "_location_latitude", "_location_longitude", "_location_altitude", "_location_precision", "meta.instanceID", 
             "X_id", "X_uuid", "X_submission_time", "X_date_modified", "X_tags", "X_notes", "X_version", "X_duration", "X_submitted_by", 
             "X_total_media", "X_media_count", "X_media_all_received", "X_xform_id")

dta <- dta[ , !(names(dta) %in% to_drop)]
###merge in treatment status
dta$treatment <- NULL
dta <- merge(dta, read.csv("~/IFPRI Dropbox/Bjorn Van Campenhout/Space2Place/sampling/sampling_ODK.csv")[c("farmer_ID","treat")] )


write.csv(dta,file=paste(path,"public/endline.csv",sep="/"), row.names=FALSE)


### some quick checks:
duplicated(dta$farmer_ID)  # still 21 duplicates
### yield on experimental plot

dta$sample_kg <- as.numeric(dta$test_plot.bags_Mcrp)
dta$sample_kg[dta$sample_kg == 999] <- NA
dta$sample_size <- as.numeric(dta$test_plot.plot_siz)

dta$sample_yield <- dta$sample_kg/dta$sample_size

dta$sample_crop <- dta$test_plot.main_crp
dta$sample_crop[dta$sample_crop == "n/a"] <- NA

library(lmtest)
library(sandwich)

library(dplyr)

## keep only crops with more than 10 records
dta <- dta %>%
  group_by(sample_crop) %>%
  filter(n() >= 10) %>%
  ungroup()

# Estimate the model
model <- lm(sample_yield ~ treat+sample_crop , data = dta)

# Clustered standard errors at the cluster_id level
cluster_se <- vcovCL(model, cluster = ~cluster_id)

# Display results with clustered SEs
coeftest(model, vcov = cluster_se)

##keep only maize

# Estimate the model
model <- lm(sample_yield ~ treat , data = dta[dta$sample_crop=="MAIZE",])

# Clustered standard errors at the cluster_id level
cluster_se <- vcovCL(model, cluster = dta[dta$sample_crop=="MAIZE",]$cluster_id)

# Display results with clustered SEs
coeftest(model, vcov = cluster_se)

##total production
# Estimate the model
model <- lm(sample_kg ~ treat , data = dta[dta$sample_crop=="MAIZE",])

# Clustered standard errors at the cluster_id level
cluster_se <- vcovCL(model, cluster = dta[dta$sample_crop=="MAIZE",]$cluster_id)

# Display results with clustered SEs
coeftest(model, vcov = cluster_se)



# Estimate the model
model <- lm(sample_yield ~ treat , data = dta[dta$sample_crop=="SOYABEAN",])

# Clustered standard errors at the cluster_id level
cluster_se <- vcovCL(model, cluster = dta[dta$sample_crop=="SOYABEAN",]$cluster_id)

# Display results with clustered SEs
coeftest(model, vcov = cluster_se)
