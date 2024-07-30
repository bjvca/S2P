rm(list=ls())
dta <- read.csv("~/data/projects/malawi/RCT/endline_2024/data/public/endline_2024.csv")
dta$maize_hrv <- as.numeric(dta$maize_hrv)


dta$maize_acre <- as.numeric(dta$maize_acre)
boxplot(dta$maize_acre)
dta$maize_acre[dta$maize_acre <= 0] <- NA 

dta$maize_acre[dta$maize_acre > 10] <- NA 

dta$yield <- dta$maize_hrv*60/dta$maize_acre*2.5

dta$yield[dta$yield >8500] <- NA
dta$yield[dta$yield < 1500] <- NA
dta <- subset(dta,!is.na(dta$yield))
mean(dta$yield, na.rm=T)
sd(dta$yield, na.rm=T)

sum(!is.na(dta$yield))

library(randomizr)    # randomizr package for complete random assignment

possible.ns <- seq(from=.2, to=.8, by=0.05)
possible.ns2 <- seq(from=.2, to=.8, by=0.05)


power.atleastone <-  matrix(NA, length(possible.ns),length(possible.ns2))  
power.alltreatments <-  matrix(NA, length(possible.ns),length(possible.ns2))  

alpha <- 0.05
sims <- 1000
N <- 3000

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  for (k in 1:length(possible.ns2)){
    significant.experiments <- rep(NA, sims)  
    one.significant.experiment <- rep(NA, sims)  
  shares_C <- possible.ns[j]
  shares_T1 <- possible.ns[k]

  p.interact <- rep(NA, sims)
  c.interact <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  sample(dta$yield, size=N, replace = TRUE)
    Y0[Y0<0] <- 0
    tau_1 <-  mean(Y0, na.rm=T)*.07
    tau_2 <-  mean(Y0, na.rm=T)*.07
    tau_3 <-  mean(Y0, na.rm=T)*.21
    tau_4 <-  mean(Y0, na.rm=T)*.40
    
    Y1 <- Y0 + tau_1
    Y2 <- Y0 + tau_2
    Y3 <- Y0 + tau_3
    Y4 <- Y0 + tau_4
    
    Z.sim <- complete_ra(N=N,prob_each = c((1-shares_C)*shares_T1/2, (1-shares_C)*shares_T1/2,(1-shares_C)*(1-shares_T1)/2,(1-shares_C)*(1-shares_T1)/2, shares_C) )
    Y.sim <- Y0*(Z.sim=="T5")  + Y1*(Z.sim=="T1")  + Y2*(Z.sim=="T2") + Y3*(Z.sim=="T3") +  Y4*(Z.sim=="T4")
    sim.dta <- data.frame(Y.sim,Z.sim)
    fit.sim1 <- lm(Y.sim ~(Z.sim=="T1") + (Z.sim=="T2")+ (Z.sim=="T3" | Z.sim=="T4" ) +( Z.sim=="T4" ), data=sim.dta)

    
    
    p.T1vsC <- summary(fit.sim1)$coefficients[2,4]
    p.T2vsC <- summary(fit.sim1)$coefficients[3,4]
    p.T3vsC <- summary(fit.sim1)$coefficients[4,4]
    p.T3vsP4 <- summary(fit.sim1)$coefficients[5,4]
    
    
    
    significant.experiments[i] <- ((p.T1vsC < alpha) & (p.T2vsC < alpha)) & (p.T3vsC < alpha) & (p.T3vsP4 < alpha)
    one.significant.experiment[i] <- (p.T1vsC < alpha) | (p.T2vsC < alpha) | (p.T3vsC < alpha) | (p.T3vsP4 < alpha)
    ###now add and extra group for 
  }
  power.alltreatments[j,k] <- mean(significant.experiments)
  power.atleastone[j,k] <- mean(one.significant.experiment)
  print(k)
  }
print(j)  
}

png(file="contour_plot.png",   width=600, height=350)

contour(possible.ns2,possible.ns,t(power.alltreatments))
contour(possible.ns2,possible.ns,t(power.alltreatments), levels = c(0.8), add = TRUE, col = "red", lwd = 2)

dev.off()

possible.ss <- seq(from=200, to=2000, by=10)     # The sample sizes we'll be considering
power.all <- rep(NA, length(possible.ss))  
power.one <- rep(NA, length(possible.ss))  
power.T1 <- rep(NA, length(possible.ss))  
power.T2 <- rep(NA, length(possible.ss))  
power.T3 <- rep(NA, length(possible.ss))  
power.T4 <- rep(NA, length(possible.ss))  
alpha <- 0.05                                    # Standard significance level
sims <- 1000                                      # Number of simulations to conduct for each N

### fill in optimized shares from previous 
j <- 4
k <- 13

shares_C <- possible.ns[j]
shares_T1 <- possible.ns[k]


#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ss)){
  N <- possible.ss[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  one.significant.experiment <- rep(NA, sims)         # Empty object to count significant experiments
  T1.sig <- rep(NA, sims)         # Empty object to count significant experiments
  T2.sig <- rep(NA, sims)         # Empty object to count significant experiments
  T3.sig <- rep(NA, sims)         # Empty object to count significant experiments
  T4.sig <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  sample(dta$yield, size=N, replace = TRUE)
    Y0[Y0<0] <- 0
    tau_1 <-  mean(Y0, na.rm=T)*.07
    tau_2 <-  mean(Y0, na.rm=T)*.07
    tau_3 <-  mean(Y0, na.rm=T)*.21
    tau_4 <-  mean(Y0, na.rm=T)*.40
    
    
    Y1 <- Y0 + tau_1
    Y2 <- Y0 + tau_2
    Y3 <- Y0 + tau_3
    Y4 <- Y0 + tau_4
    
    Z.sim <- complete_ra(N=N,prob_each = c((1-shares_C)*shares_T1/2, (1-shares_C)*shares_T1/2,(1-shares_C)*(1-shares_T1)/2,(1-shares_C)*(1-shares_T1)/2, shares_C) )
    Y.sim <- Y0*(Z.sim=="T5")  + Y1*(Z.sim=="T1")  + Y2*(Z.sim=="T2") + Y3*(Z.sim=="T3") +  Y4*(Z.sim=="T4")
    sim.dta <- data.frame(Y.sim,Z.sim)
    fit.sim1 <- lm(Y.sim ~(Z.sim=="T1") + (Z.sim=="T2")+ (Z.sim=="T3" | Z.sim=="T4" ) +( Z.sim=="T4" ), data=sim.dta)
    
    
    p.T1vsC <- summary(fit.sim1)$coefficients[2,4]
    p.T2vsC <- summary(fit.sim1)$coefficients[3,4]
    p.T3vsC <- summary(fit.sim1)$coefficients[4,4]
    p.T3vsP4 <- summary(fit.sim1)$coefficients[5,4]
    
    
    
    significant.experiments[i] <- ((p.T1vsC < alpha) | (p.T2vsC < alpha)) & (p.T3vsC < alpha) & (p.T3vsP4 < alpha)
    one.significant.experiment[i] <- (p.T1vsC < alpha) | (p.T2vsC < alpha) | (p.T3vsC < alpha) | (p.T3vsP4 < alpha)
    T1.sig[i] <- (p.T1vsC < alpha)
    T2.sig[i] <- (p.T2vsC < alpha)
    T3.sig[i] <- (p.T3vsC < alpha)
    T4.sig[i] <- (p.T3vsP4 < alpha)
  }
  
  power.all[j] <- mean(significant.experiments)
  power.one[j] <- mean(one.significant.experiment)    
  power.T1[j] <- mean(T1.sig)    
  power.T2[j] <- mean(T2.sig)   
  power.T3[j] <- mean(T3.sig)  
  power.T4[j] <- mean(T4.sig)  
}

png(file="power_curves.png",   width=600, height=350)

plot(type = "l",possible.ss,  power.all, ylim=c(0,1), col="black")
lines(possible.ss,power.T1, col="green")
#lines(possible.ss,power.T2, col="red")
lines(possible.ss,power.T3, col="blue")
lines(possible.ss,power.T4, col="red")
abline(h=0.8)
dev.off()

### finally fill in sample size to get the groups:
table(complete_ra(N=1850,prob_each = c((1-shares_C)*shares_T1/2, (1-shares_C)*shares_T1/2,(1-shares_C)*(1-shares_T1)/2,(1-shares_C)*(1-shares_T1)/2, shares_C) ))
