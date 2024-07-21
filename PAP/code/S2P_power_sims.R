rm(list=ls())
dta <- read.csv("~/data/projects/malawi/RCT/endline_2024/data/public/endline_2024.csv")
dta$maize_hrv <- as.numeric(dta$maize_hrv)


dta$maize_acre <- as.numeric(dta$maize_acre)
boxplot(dta$maize_acre)
dta$maize_acre[dta$maize_acre <= 0] <- NA 

dta$maize_acre[dta$maize_acre > 10] <- NA 

dta$yield <- dta$maize_hrv*60/dta$maize_acre*2.5

dta$yield[dta$yield >8500] <- NA
dta$yield[dta$yield < 500] <- NA
dta <- subset(dta,!is.na(dta$yield))
mean(dta$yield, na.rm=T)
sd(dta$yield, na.rm=T)

sum(!is.na(dta$yield))

library(randomizr)    # randomizr package for complete random assignment

possible.ns <- seq(from=.2, to=.8, by=0.1)
possible.ns2 <- seq(from=.2, to=.8, by=0.1)


power.atleastone <-  matrix(NA, length(possible.ns),length(possible.ns2))  
power.alltreatments <-  matrix(NA, length(possible.ns),length(possible.ns2))  

alpha <- 0.5
sims <- 1000
N <- 1000

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
    Y0 <-  sample(dta$yield, size=N)
    Y0[Y0<0] <- 0
    tau_1 <-  mean(Y0, na.rm=T)*.06
    tau_2 <-  mean(Y0, na.rm=T)*.06
    tau_3 <-  mean(Y0, na.rm=T)*.10
    
    Y1 <- Y0 + tau_1
    Y2 <- Y0 + tau_2
    Y3 <- Y0 + tau_3
    
    Z.sim <- complete_ra(N=1000,prob_each = c((1-shares_C)*shares_T1/2, (1-shares_C)*shares_T1/2,(1-shares_C)*(1-shares_T1), shares_C) )
    Y.sim <- Y0*(Z.sim=="T4")  + Y1*(Z.sim=="T1")  + Y2*(Z.sim=="T2") + Y3*(Z.sim=="T3")

    fit.sim <- lm(Y.sim ~(Z.sim=="T1") + (Z.sim=="T2")+ (Z.sim=="T3"))

    
    p.T1vsC <- summary(fit.sim)$coefficients[2,4]
    p.T2vsC <- summary(fit.sim)$coefficients[3,4]
    p.T3vsC <- summary(fit.sim)$coefficients[4,4]
    
    significant.experiments[i] <- (p.T1vsC < alpha) & (p.T2vsC < alpha) & (p.T3vsC < alpha)
    one.significant.experiment[i] <- (p.T1vsC < alpha) | (p.T2vsC < alpha) | (p.T3vsC < alpha)
    ###now add and extra group for 
  }
  power.alltreatments[j,k] <- mean(significant.experiments)
  power.atleastone[j,k] <- mean(significant.experiments)
  print(k)
  }
print(j)  
}

png(file="power_plot1.png",
    width=600, height=350)

plot(type = "l",possible.ns,  power.T1vsC, ylim=c(0,1))
lines(possible.ns,  power.T2vsC, col="red")
lines(possible.ns, power.interact, col="blue")
lines(possible.ns, power.S2P, col="orange")
abline(h=0.8)

dev.off()

possible.ns <- seq(from=100, to=1500, by=10)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 1000                                      # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  sample(size = N, dta$yield)+tau_1+tau_2+tau_3              # control potential outcome
    tau <- (mean_base_outcome+tau_1+tau_2+tau_3)*.1                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  

}

png(file="power_plot2.png",
    width=600, height=350)

plot(type = "l",possible.ns,  powers, ylim=c(0,1), col="green")
abline(h=0.8)
dev.off()
