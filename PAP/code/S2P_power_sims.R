rm(list=ls())

mean_base_outcome <- 2600
sd_base_outcome <- 2000
#install.packages("randomizr")
library(randomizr)    # randomizr package for complete random assignment

possible.ns <- seq(from=100, to=1500, by=10)
power.T1vsC <- rep(NA, length(possible.ns))
power.T2vsC <- rep(NA, length(possible.ns))
power.interact <- rep(NA, length(possible.ns))
power.S2P <- rep(NA, length(possible.ns))
alpha <- 0.1  #(one-tailed test at .05 level)
sims <- 1000

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){

  N <- possible.ns[j]

  p.T1vsC <- rep(NA, sims)
  p.T2vsC <- rep(NA, sims)
  p.interact <- rep(NA, sims)
  p.S2P <- rep(NA, sims)
  
  c.T1vsC <- rep(NA, sims)
  c.T2vsC <- rep(NA, sims)
  c.interact <- rep(NA, sims)
  c.S2P <- rep(NA, sims)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=mean_base_outcome, sd=sd_base_outcome)
    Y0[Y0<0] <- 0
    tau_1 <- mean_base_outcome*.15
    tau_2 <- mean_base_outcome*.10
    tau_3 <- (mean_base_outcome + mean_base_outcome*.15 + mean_base_outcome*.10)*0.075
    
    Y1 <- Y0 + tau_1
    Y2 <- Y0 + tau_2
    Y3 <- Y0 + tau_1+ tau_2+ tau_3
    
    Z.sim <- complete_ra(N=N, num_arms=4)
    Y.sim <- Y0*(Z.sim=="T4")  + Y1*((Z.sim=="T1") ) + Y2*((Z.sim=="T2") )+ Y3*(Z.sim=="T3" )
  first_demeaned <-  (as.numeric(Z.sim=="T1" | Z.sim=="T3") - mean(as.numeric(Z.sim=="T1" | Z.sim=="T3")))
  second_demeaned <-  (as.numeric(Z.sim=="T2" | Z.sim=="T3") - mean(as.numeric(Z.sim=="T2" | Z.sim=="T3")))
    frame.sim <- data.frame(Y.sim, Z.sim, first_demeaned, second_demeaned)

    fit.T1vsC.sim <- lm(Y.sim ~as.numeric(Z.sim=="T1" | Z.sim=="T3") +second_demeaned+as.numeric(Z.sim=="T1" | Z.sim=="T3")*second_demeaned , data=frame.sim)
    fit.T2vsC.sim <-lm(Y.sim ~first_demeaned+ as.numeric(Z.sim=="T2" | Z.sim=="T3") +first_demeaned*as.numeric(Z.sim=="T2" | Z.sim=="T3"), data=frame.sim)
    fit.interact.sim <- lm(Y.sim ~first_demeaned+second_demeaned+((Z.sim=="T1" | Z.sim=="T3")  & (Z.sim=="T2" | Z.sim=="T3")), data=frame.sim)
    fit.S2P.sim <- lm(Y.sim ~ Z.sim=="T3", data=frame.sim[frame.sim$Z.sim=="T3" | frame.sim$Z.sim=="T4",])   
    ### Need to capture coefficients and pvalues (one-tailed tests, so signs are important)
    c.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,1]
    c.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[3,1]
    c.interact[i] <- summary(fit.interact.sim)$coefficients[4,1]
    c.S2P[i] <- summary(fit.S2P.sim)$coefficients[2,1]
    
    p.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,4]
    p.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[3,4]
    p.interact[i] <- summary(fit.interact.sim)$coefficients[4,4]
    p.S2P[i] <- summary(fit.S2P.sim)$coefficients[2,4]
    
    ###now add and extra group for 
  }
  power.T1vsC[j] <- mean(c.T1vsC>0 & (p.T1vsC < alpha/2 ))
  power.T2vsC[j] <- mean( c.T2vsC>0 & (p.T2vsC < alpha/2))
  power.interact[j] <- mean(c.interact>0 & (p.interact < alpha/2) )
  power.S2P[j] <- mean(c.S2P>0 & (p.S2P < alpha/2) )
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
    Y0 <-  rnorm(n=N, mean=mean_base_outcome+tau_1+tau_2+tau_3, sd=sd_base_outcome)              # control potential outcome
    tau <- (mean_base_outcome+tau_1+tau_2+tau_3)*.125                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}

png(file="power_plot2.png",
    width=600, height=350)

plot(type = "l",possible.ns,  powers, ylim=c(0,1), col="green")
abline(h=0.8)
dev.off()
