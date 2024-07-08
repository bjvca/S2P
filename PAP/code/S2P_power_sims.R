rm(list=ls())

#### power simulations to determine sample size for S2P project
#### Bjorn Van Campennhout - Jul 8th 2024
#### adapted for continuous outcomes with and without blocking on gender

possible.ns <- seq(from=100, to=2000, by=50) ## vector for different candidate sample size to iterate over
powers <- rep(NA, length(possible.ns)) ## will collect power levels (proportion of sig in total of sims)
powers.blocked <- rep(NA, length(possible.ns)) ## will collect power levels for blocked design
alpha <- 0.05
sims <- 1000  # Increase the number of simulations

for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
  
  significant.experiments <- rep(NA, sims)
  significant.experiments.blocked <- rep(NA, sims) # Need a second empty vector here too
  
  for (i in 1:sims){
    gender <- c(rep("F", N/2), rep("M", N/2)) # Generate "gender" covariate
    age <- sample(x=18:65, size=N, replace=TRUE) # Generate "age" covariate
    effectofgender <- 1000 # Increase the hypothesized effect of gender on continuous outcome
    effectofage <- 50 # Increase the hypothesized effect of age on continuous outcome
    
    ## Hypothesize Control Outcome as a function of gender, age, and error
    Y0 <- 2500 + effectofgender * (gender == "M") + effectofage * age + rnorm(n=N, mean=0, sd=sqrt(1000))
    
    ## This is all the same ##
    tau <- 150 # Treatment effect corresponding to a 150 unit increase
    Y1 <- Y0 + tau
    
    ## Random assignment without blocking
    Z.sim <- rbinom(n=N, size=1, prob=0.5)
    Y.sim <- Y1 * Z.sim + Y0 * (1 - Z.sim)
    
    ## Random assignment with blocking on gender
    Z.sim.blocked <- rep(NA, N)
    idx_male <- which(gender == "M")
    idx_female <- which(gender == "F")
    Z.sim.blocked[idx_male] <- rbinom(length(idx_male), size=1, prob=0.5)
    Z.sim.blocked[idx_female] <- rbinom(length(idx_female), size=1, prob=0.5)
    Y.sim.blocked <- Y1 * Z.sim.blocked + Y0 * (1 - Z.sim.blocked)
    
    ## Fit linear regression models ##
    fit.sim <- lm(Y.sim ~ Z.sim)
    fit.sim.blocked <- lm(Y.sim.blocked ~ Z.sim.blocked)
    
    ## Extract p-values and calculate significance ##
    p.value <- summary(fit.sim)$coefficients[2,4]
    p.value.blocked <- summary(fit.sim.blocked)$coefficients[2,4]
    significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.blocked[i] <- (p.value.blocked <= alpha)
  }
  
  powers[j] <- mean(significant.experiments)
  powers.blocked[j] <- mean(significant.experiments.blocked)
}

plot(possible.ns, powers, ylim=c(0,1), type="l", col="blue", xlab="Sample Size", ylab="Power", main="Power Analysis for Continuous Outcome")
lines(possible.ns, powers.blocked, col="red")
legend("bottomright", legend=c("Without Blocking", "With Blocking"), col=c("blue", "red"), lty=1)

