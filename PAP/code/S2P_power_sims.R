rm(list=ls())
library(dplyr)
library(clubSandwich)
library(car)
dta <- read.csv("~/data/projects/malawi/RCT/endline_2024/data/public/endline_2024.csv")
#we need the villages, merge in from baseline
villages <- read.csv("~/data/projects/malawi/RCT/midline_sept/data/raw/to_upload.csv")[c("farmer_ID", "q1", "q2","q3" )]

dta <- merge(dta, villages, by="farmer_ID")

dta$village <- paste(paste(dta$q1, dta$q2,sep="_"), dta$q3,sep="_")



dta$maize_hrv <- as.numeric(dta$maize_hrv)


dta$maize_acre <- as.numeric(dta$maize_acre)
boxplot(dta$maize_acre)
dta$maize_acre[dta$maize_acre <= 0] <- NA 

dta$maize_acre[dta$maize_acre > 10] <- NA 

dta$yield <- dta$maize_hrv*60/dta$maize_acre*2.5

dta$yield[dta$yield >8500] <- NA
dta$yield[dta$yield < 1500] <- NA
dta <- subset(dta,!is.na(dta$yield))

### make sure every village has 30 households

# Define the sampling function
sample_households <- function(group) {
  n <- nrow(group)
  if (n >= 15) {
    # Sample without replacement if the group size is 30 or more
    return(group %>% sample_n(15, replace = FALSE))
  } else {
    # Sample with replacement if the group size is less than 30
    return(group %>% sample_n(15, replace = TRUE))
  }
}


sampled_dta <- dta %>%
  group_by(village) %>%
  group_modify(~ sample_households(.x)) %>%
  ungroup()

dta <- data.frame(sampled_dta)

mean(dta$yield, na.rm=T)
sd(dta$yield, na.rm=T)

sum(!is.na(dta$yield))

library(randomizr)    # randomizr package for complete random assignment
###start with a fixed number of villages (eg 110)
###in the outer loop, randomly select share of villages in C2 pure control (rest will get C1, T1, T2 or T3)
### in inner loop select share in C1, rest is in T1, T2 or T3 according to the following distribution:
### we assume symmetry between T1 and T3
### we model T2 as an additional effect on T2, so size T2 is half the size of T1

### C2 are the control
### C1 get 0.15 - in other words we add a constant effect for all non C2 villages  
### T1 and T3 both get 0.07
### T3 gets an effect of .2

possible.ns <- seq(from=10, to=20, by=1)
possible.ns2 <- seq(from=.1, to=.5, by=0.05)


power.atleastone <-  matrix(NA, length(possible.ns),length(possible.ns2))  
power.alltreatments <-  matrix(NA, length(possible.ns),length(possible.ns2))  

power.one.T1 <-  matrix(NA, length(possible.ns),length(possible.ns2))   
power.one.T2 <-  matrix(NA, length(possible.ns),length(possible.ns2))  
power.one.comp <-  matrix(NA, length(possible.ns),length(possible.ns2))  

alpha <- 0.05
sims <- 1000
### N is number of villages - set to max
N <- 113

#### Outer loop to vary the number of subjects within village####
for (j in 1:length(possible.ns)){
  ### inner loop to determine T vs C 
  for (k in 1:length(possible.ns2)){
    significant.experiments <- rep(NA, sims)  
    significant.experiments.T1 <- rep(NA, sims)  
    significant.experiments.T2 <- rep(NA, sims)  
    significant.experiments.comp <- rep(NA, sims)  
  h <- possible.ns[j]
  shares_C <- possible.ns2[k]

  p.interact <- rep(NA, sims)
  c.interact <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
### start by taking a sample of N villages
    
    #define effects
    eff_T1 <-  mean(dta$yield, na.rm=T)*.13
    eff_T2 <-  mean(dta$yield, na.rm=T)*.13
    ### potential outcomes
    
    dta$T1 <- dta$yield + eff_T1
    dta$T2 <- dta$yield + eff_T1 + eff_T2
    
    #step one: from total sample size N, assign shares_C to control and 1-shares_C T1 and T2
    
    villages <- unique(dta$village)
    
    # Calculate the number of villages to sample
    
    sampled_villages <- sample(villages, size = ceiling(length(villages) *shares_C))
    
    non_selected_villages <- setdiff(unique(dta$village), sampled_villages)
    
    # Randomly shuffle the non-selected villages
    shuffled_villages <- sample(non_selected_villages)
    
    half <- length(shuffled_villages) / 2
    
    villages_T1 <- shuffled_villages[1:half]
    villages_T2 <- shuffled_villages[(half + 1):length(shuffled_villages)]
    
    
    sampled_df <- dta %>%
      mutate(
        treatment = case_when(
          village %in% villages_T1 ~ "T1",
          village %in% villages_T2 ~ "T2",
          village %in% sampled_villages ~ "C"                 )
      )
    sel_dta <-  data.frame(  sampled_df)
    #now within each village, randomly select h households
    
    final_sampled_households <- sel_dta %>%
      group_by(village) %>%
      slice_sample(n = h) %>%
      ungroup()
    sel_dta <- data.frame(final_sampled_households)
    
    
   sel_dta$Y.sim <- sel_dta$yield*(sel_dta$treatment == "C") + sel_dta$T1*(sel_dta$treatment == "T1") + sel_dta$T2*(sel_dta$treatment == "T2")
    
   sel_dta$treatment1 <- sel_dta$treatment=="T1" | sel_dta$treatment=="T2"
   sel_dta$treatment2 <- sel_dta$treatment=="T2"
   
    fit.sim1 <- lm(Y.sim ~treatment1 + treatment2, data=sel_dta)
    vcov_cluster <- vcovCR(fit.sim1,cluster=sel_dta$village,type="CR3")
 
   
    p.T1vsC <- coef_test(fit.sim1, vcov_cluster)$p_Satt[2]
    p.T1vsT2 <- coef_test(fit.sim1, vcov_cluster)$p_Satt[3]


 
    significant.experiments[i] <- ((p.T1vsC < alpha) & (p.T1vsT2 < alpha)) 
    significant.experiments.T1[i] <- (p.T1vsC < alpha)
    significant.experiments.comp[i] <- (p.T1vsT2 < alpha)
    ###now add and extra group for 
  }
  power.alltreatments[j,k] <- mean(significant.experiments)
  power.one.T1[j,k] <- mean(significant.experiments.T1)

  power.one.comp[j,k] <- mean(significant.experiments.comp)
  print(k)
  }
print(j)  
}

png(file="contour_plot.png",   width=600, height=350)

contour(possible.ns2,possible.ns,t(power.alltreatments))
contour(possible.ns2,possible.ns,t(power.alltreatments), levels = c(0.8), add = TRUE, col = "red", lwd = 2)

dev.off()

png(file="contour_plot_one.png",   width=600, height=350)

contour(possible.ns2,possible.ns,t(power.one.T1))
contour(possible.ns2,possible.ns,t(power.one.T1), levels = c(0.8), add = TRUE, col = "red", lwd = 2)
dev.off()

png(file="contour_plot_comp.png",   width=600, height=350)

contour(possible.ns2,possible.ns,t(power.one.comp))
contour(possible.ns2,possible.ns,t(power.one.comp), levels = c(0.8), add = TRUE, col = "red", lwd = 2)
dev.off()