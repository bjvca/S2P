
### this file prepares raw data collected during consumer intervention for public release
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

dta[dta$X_uuid  %in% dta_first$X_uuid, garden_vector] <- dta_first[dta$X_uuid %in% dta_first$X_uuid,garden_vector]
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
                                   "a331ef94-5027-4d0b-a5f5-7cf23ced937b")))

#fix farmer ID

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
dta$farmer_ID[dta$X_uuid  == "d48ab590-b22e-449d-8187-e0c996cd5b6e"] <-  "F_3536"
dta$farmer_ID[dta$X_uuid  == "d48ab590-b22e-449d-8187-e0c996cd5b6e"] <-  "F_3536"
dta$farmer_ID[dta$X_uuid  == "4e54ae02-34a4-4997-bc78-e90d4964f8ad"] <-  "F_3547"
dta$farmer_ID[dta$X_uuid  == "d3365107-2d6b-4ad3-a3d8-e1a62c8a80f4"] <-  "F_528"
dta$farmer_ID[dta$X_uuid  == "4b535dae-2319-4964-970f-6f8345ea767e"] <-  "F_533"
dta$farmer_ID[dta$X_uuid  == "a33cc925-2691-4ebd-b5a2-1eef69e240a0"] <-  "F_974"
dta$farmer_ID[dta$X_uuid  == "36be82a1-aa80-4a02-bb0d-0409ffbd138f"] <-  "F_547"
dta$farmer_ID[dta$X_uuid  == "2de803f6-4bfa-4efd-8e13-1c1ce2ba806a"] <-  "F_531"


write.csv(dta,file=paste(path,"raw/interim.csv",sep="/"), row.names=FALSE)



to_drop <- c("start", "end", "deviceid", "simserial", "phonenumber", "subscriberid", "enumerator", "district",   "q2","q3", "farmer_name", 
             "phone1", "phone2", "nick", "lat", "long", "plot.1..plot_name", "plot.2..plot_name", "plot.3..plot_name", "plot.4..plot_name", "plot.5..plot_name","plot_select_name","test_plot.plot_samp",
              "location", "_location_latitude", "_location_longitude", "_location_altitude", "_location_precision", "meta.instanceID", 
             "X_id", "X_uuid", "X_submission_time", "X_date_modified", "X_tags", "X_notes", "X_version", "X_duration", "X_submitted_by", 
             "X_total_media", "X_media_count", "X_media_all_received", "X_xform_id")

dta <- dta[ , !(names(dta) %in% to_drop)]



write.csv(dta,file=paste(path,"public/endline.csv",sep="/"), row.names=FALSE)
