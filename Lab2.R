# Sample Size calculations

# libraries
library(dplyr)
library(MASS)
library(lmerTest)

# Problem 1

# We vary sample size from 5 to 40
p.matrix = c()
for (N in seq(5,40,by=5)) {
  p.temp.vector = c()
  # We repeat simulation for 500 times for each N (Step 3)
  for (i in 1:500) {
    # Set seed
    set.seed(i)
    # Step 1: generate data
    # Generate id
    dat = data.frame(id = paste0(615,1:N))
    # Long format: each subject was followed by 8 days
    dat = dat %>% slice(rep(1:n(), each=8))
    # Make Day variable
    dat = dat %>% group_by(id) %>% mutate(Days = 1:n()) %>% ungroup()
    # Simulate random error
    dat = dat %>% mutate(err = rnorm(8*N,mean=0,sd=30))
    
    # Simulate (correlated) subject-level random effects for intercepts and slopes
    ## Covariance matrix
    S1 = diag(c(24.741, 5.922)) %*% matrix(c(1, 0, 0, 1), nrow=2) %*% diag(c(24.741, 5.922))
    ## Simulate realization of random intercept and slope
    U1 = mvrnorm(N, mu=c(0,0), Sigma=S1) %>% as.data.frame()
    ## Add identifier (subject id)
    U1 = U1 %>% bind_cols(id = paste0(615,1:N))
    ## Merge subject-level random effects back to data
    dat = dat %>% left_join(U1,by="id")
    
    # Simulate the outcome: Reaction_ij
    dat = dat %>% mutate(Reaction = (251.405 + V1) + (3 +V2)*Days + err)
    
    # Step 2: test the null hypothesis
    mod = lmer(Reaction ~ Days + (Days | id), dat)
    p.value = summary(mod)$coef["Days","Pr(>|t|)"]
    # Save p value
    p.temp.vector = c(p.temp.vector,p.value)
  }
  # Save p value vector for each N
  p.matrix = cbind(p.matrix,p.temp.vector)
}
# Matrix => data.frame
p.matrix = p.matrix %>% as.data.frame()
# Add column names
names(p.matrix) = seq(5,40,by=5)
# Step 4: calculate power
power = p.matrix %>% summarise_all(function(x) mean(x<0.05))

plot(seq(5,40,by=5),power,xlab = "Sample size (N)",ylab="Power",type = "b", pch = 19)
abline(h=0.8,lty=2)

# Problem 2
N <- 40
b1.vector = c()
for (i in 1:500) {
  # Set seed
  set.seed(i)
  # Step 1: generate data
  # Generate id
  dat = data.frame(id = paste0(615,1:N))
  # Long format: each subject was followed by 8 days
  dat = dat %>% slice(rep(1:n(), each=8))
  # Make Day variable
  dat = dat %>% group_by(id) %>% mutate(Days = 1:n()) %>% ungroup()
  # Simulate random error
  dat = dat %>% mutate(err = rnorm(8*N,mean=0,sd=30))
  
  # Simulate (correlated) subject-level random effects for intercepts and slopes
  ## Covariance matrix
  S1 = diag(c(24.741, 5.922)) %*% matrix(c(1, 0, 0, 1), nrow=2) %*% diag(c(24.741, 5.922))
  ## Simulate realization of random intercept and slope
  U1 = mvrnorm(N, mu=c(0,0), Sigma=S1) %>% as.data.frame()
  ## Add identifier (subject id)
  U1 = U1 %>% bind_cols(id = paste0(615,1:N))
  ## Merge subject-level random effects back to data
  dat = dat %>% left_join(U1,by="id")
  
  # Simulate the outcome: Reaction_ij
  dat = dat %>% mutate(Reaction = (251.405 + V1) + (3 +V2)*Days + err)
  
  # Step 2: test the null hypothesis
  mod = lmer(Reaction ~ Days + (Days | id), dat)
  b1 = summary(mod)$coef["Days","Estimate"]
  # Save p value
  b1.vector = c(b1.vector,b1)
}

plot(b1.vector,type = "b", pch = 19)
abline(h=0.8,lty=2)
