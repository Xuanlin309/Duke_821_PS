install.packages("mlogit")
install.packages("dplyr")
library(mlogit)
library(dplyr)
setwd("/Volumes/USB30FD/821_ps")
#####data upload and prepare 
data <- read.csv("long_data.csv")
data <- data[-c(1)]
names(data)[names(data) == "idcase"] <- "id"
data <- mlogit.data(data, choice = "choice", 
                    shape = "long", alt.levels = (c(1:100)), id ="id")
#Q2.i:try the simple model first
model_1 <- mlogit (choice ~ ramp + restroom + walleye + salmon 
                   + panfish + travelcost | 0, data)
summary(model_1)

#Q2.ii: preference heterogenerity r.t. person_specific variables 
data$panfish_kids <- data$panfish * data$kids
data$restroom_kids <- data$restroom * data$kids
data$ramp_boat <- data$ramp * data$boat
data$walleye_boat <- data$walleye * data$boat

#write.csv(data, file="data_forloop.csv")

model_2 <- mlogit (choice ~ ramp + restroom + walleye + salmon + panfish 
                   + travelcost + panfish_kids + restroom_kids
                   + ramp_boat + walleye_boat | 0, data)
summary(model_2)

#Q2.iii: include an unobserved  site attribute 
library(mlogit)
library(dplyr)
library(maxLik)
setwd("/Volumes/USB30FD/821_ps")
data <-read.csv("data_forloop.csv")
data <- data[-c(1)]
data$alt_id <-as.numeric(as.character(data$alt_id))
data$id <-as.numeric(as.character(data$id))
##small loop 
delta = 1e-07 #tolerancel level for first loop to stop 
a <-  c(-0.1030564, -0.1632874, 0.3442013, 0.9818902, 0.6961032)   

theta_1 <- replicate(100,0)
theta_2 <- replicate(100,0)
data$theta_j = 0 
data$choice <- ifelse(data$choice == "FALSE", 0, 1)
#function that produce the prediction of share S_j for all j 
maxlikelihood <- c(2000000000)
b <-c(0)
##
repeat{
  data$Prob_ij_nominator <- exp(0 + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat)         
  
  data <- data %>%
    group_by(id) %>%
    mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
    ungroup()
  
  data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
  data <- data %>%
    group_by(alt_id) %>%
    mutate(share_j = mean(Prob_ij))%>%
    ungroup()
  
  share <- data$share_j[1:100]
  
  real_share <- data$shares[1:100]
  
  for (j in 1:100) {
    theta_2[j] <- theta_1[j] + log(real_share[j]) - log(share[j])
  }
  
  theta_diff <- theta_2 - theta_1
  
  
  
  while (max(abs(theta_diff))>delta){
    theta_1 <- theta_2 #update new thetas
    data$theta_j <- theta_1[data$alt_id]
    data <- data %>% 
      mutate(Prob_ij_nominator = exp(data$theta_j + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat))%>%
      group_by(id) %>%
      mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
      ungroup()
    data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
    data <- data %>% 
      group_by(alt_id) %>%
      mutate(share_j = mean(Prob_ij)) %>%
      ungroup()
    share <- data$share_j[1:100]
    for (j in 1:100) {
      theta_2[j] <- theta_1[j] + log(real_share[j]) - log(share[j])
    }
    theta_diff <- theta_2 - theta_1
    #print(theta_2)
  }
  
  # theta_2 now is the optimised baseline utility for the specific "a" parameter set.
  # level normalization: substract the mean 
  level_norm <- function (x) {
    scale(x, scale = FALSE)
  }
  
  baseline_util <- level_norm(theta_2) 
  ##update the new theta_j to the dataframe 
  data$theta_j <- baseline_util[data$alt_id]
  
  #x6<- replicate(2404, baseline_util)
  
  ## calculate likelihood function based on the baseline_utility and parameter set
  
  #changed here!!!! be careful 
  data <- data %>%            
    mutate(Prob_ij_nominator = exp(data$theta_j + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat))%>%
    group_by(id) %>%
    mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
    ungroup()
  data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
  #
  data$Prob_sitechoice <- log(data$Prob_ij^data$choice)
  LogL_attributes <- data$Prob_sitechoice
  llmax <- sum(LogL_attributes)
  maxlikelihood[-llmax < maxlikelihood] <- llmax
  
  ## update parameter set by conducting the maximisation loglikelihood 
  #compute loglikelood function  x1, x2, x3, x4, x5, x6
  
  #############################
  
  loglike = function(theta, x) {
    # theta parameter vector; x  
    mu1 = theta[1]
    mu2 = theta[2]
    mu3 = theta[3]
    mu4 = theta[4]
    mu5 = theta[5]
    x[, 20] <- exp( x[, 19] + mu1*x[, 8] +mu2*x[, 15] + mu3*x[, 16]+mu4*x[, 17] + mu5*x[, 18])
    x1 <- as.vector(unlist(x[,20]))
    x2 <- unname(tapply(x1,(seq_along(x1)-1) %/% 100, sum))
    x[, 21] <- cbind(rep(x2, each=100))
    x[, 22] <- x[, 20] / x[, 21]
    x[, 24]<- x[, 22]^x[, 3]
    ans = sum(log(x[, 24]))
    return(ans)
  }
  theta.start = a 
  names(theta.start) = c("mu1", "mu2", "mu3", "mu4", "mu5")
  theta.mle = maxLik (loglike, start=theta.start, x=data, method = "BFGS")
  #theta.mle = optim(par=theta.start, fn=loglike, x=data, method ="BFGS")
  summary(theta.mle)
  summary(theta.mle$coef)
  
  
  # write an additional loop for the first stage to converge 
  # print(theta.mle$estimate)
  print(theta.mle$maximum)
  a[theta.mle$maximum > maxlikelihood] <- theta.mle$estimate
  b[round(theta.mle$maximum, 3) == round(maxlikelihood, 3)] <- 1
  maxlikelihood[round(theta.mle$maximum, 3) > round(maxlikelihood, 3)] <- theta.mle$maximum  
  print(a)
  if( b > 0){
    break 
  }
}



names(a) <- c("travelcost", "panfish_kids", "restroom_kids", "ramp_boat", "walleye_boat")

print (maxlikelihood)
print(a)
###BLP second stage 
##
BLP2 <- lm(theta_j ~ ramp + restroom + walleye + salmon + panfish + 0, data=data)
summary(BLP2) 

###############################################################################################################
##############################################################################################################
###Q2.iv 
#clean the global environment for the new question and upload package and settings we need
rm(list=ls()) 
library(mlogit)
library(dplyr)
library(maxLik)
setwd("C:/821_ps")
data <-read.csv("data_forloop.csv")
data <- data[-c(1)]
data$alt_id <-as.numeric(as.character(data$alt_id))
data$id <-as.numeric(as.character(data$id))
##small loop 
delta = 1e-07 #tolerancel level for first loop to stop 
theta_1 <- replicate(100,0)
theta_2 <- replicate(100,0)
data$theta_j = 0 
data$choice <- ifelse(data$choice == "FALSE", 0, 1)
#function that produce the prediction of share S_j for all j 
maxlikelihood <- c(2000000000)
b <-c(0)
a <-  c(-0.1230331, -0.1408312,  0.5068775,  1.3974301,  0.5928853 , 1) ##1 is the initial parameter setting for normal distribution of random assignment  
##add a random component in preferences for walleye 
## and we also take the parameter set from Q2.iii for the initial guess. The initial guess for delta_j is still zero for all j 
## generate random value rnorm(2404, mean=0, sd=a[6])

repeat{
  random <- rnorm(2404, mean=0, sd=a[5])
  data$random <- rep(random, each=100)
  data$Prob_ij_nominator <- exp(0 + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat + data$random*data$walleye)         
  
  data <- data %>%
    group_by(id) %>%
    mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
    ungroup() 
  
  data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
  data <- data %>%
    group_by(alt_id) %>%
    mutate(share_j = mean(Prob_ij))%>%
    ungroup()
  
  share <- data$share_j[1:100]
  
  real_share <- data$shares[1:100]
  
  for (j in 1:100) {
    theta_2[j] <- theta_1[j] + log(real_share[j]) - log(share[j])
  }
  
  theta_diff <- theta_2 - theta_1
  
  
  
  while (max(abs(theta_diff))>delta){
    theta_1 <- theta_2 #update new thetas
    data$theta_j <- theta_1[data$alt_id]
    data <- data %>% 
      mutate(Prob_ij_nominator = exp(data$theta_j + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat + data$random*data$walleye))%>%
      group_by(id) %>%
      mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
      ungroup()
    data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
    data <- data %>% 
      group_by(alt_id) %>%
      mutate(share_j = mean(Prob_ij)) %>%
      ungroup()
    share <- data$share_j[1:100]
    for (j in 1:100) {
      theta_2[j] <- theta_1[j] + log(real_share[j]) - log(share[j])
    }
    theta_diff <- theta_2 - theta_1
    print(theta_2)
  }
  
  # theta_2 now is the optimised baseline utility for the specific "a" parameter set.
  # level normalization: substract the mean 
  level_norm <- function (x) {
    scale(x, scale = FALSE)
  }
  
  baseline_util <- level_norm(theta_2) 
  ##update the new theta_j to the dataframe 
  data$theta_j <- baseline_util[data$alt_id]
  
  #x6<- replicate(2404, baseline_util)
  
  ## calculate likelihood function based on the baseline_utility and parameter set
  
  data <- data %>%            
    mutate(Prob_ij_nominator = exp(data$theta_j + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat + data$random*data$walleye))%>%
    group_by(id) %>%
    mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
    ungroup()
  data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
  #
  data$Prob_sitechoice <- log(data$Prob_ij^data$choice)
  LogL_attributes <- data$Prob_sitechoice
  llmax <- sum(LogL_attributes)
  maxlikelihood[-llmax < maxlikelihood] <- llmax
  
  ## update parameter set by conducting the maximisation loglikelihood 
  #compute loglikelood function  x1, x2, x3, x4, x5, x6
  
  #############################
  
  loglike = function(theta, x) {
    # theta parameter vector; x  
    mu1 = theta[1]
    mu2 = theta[2]
    mu3 = theta[3]
    mu4 = theta[4]
    mu5 = theta[5]
    x[, 21] <- exp( x[, 19] + mu1*x[, 8] +mu2*x[, 15] + mu3*x[, 16]+mu4*x[, 17] + mu5*x[, 18] + x[,20]*x[,11])
    x1 <- as.vector(unlist(x[,21]))
    x2 <- unname(tapply(x1,(seq_along(x1)-1) %/% 100, sum))
    x[, 22] <- cbind(rep(x2, each=100))
    x[, 23] <- x[, 21] / x[, 22]
    x[, 25]<- x[, 23]^x[, 3]
    ans = sum(log(x[, 25]))
    return(ans)
  }
  
  theta.start = a 
  names(theta.start) = c("mu1", "mu2", "mu3", "mu4", "mu5", "sigma") 
  theta.mle = maxLik (loglike, start=theta.start, x=data, method = "BFGS")
  #theta.mle = optim(par=theta.start, fn=loglike, x=data, method ="BFGS")
  summary(theta.mle)
  summary(theta.mle$coef)
  
  
  # write an additional loop for the first stage to converge 
  # print(theta.mle$estimate)
  print(theta.mle$maximum)
  a[theta.mle$maximum > maxlikelihood] <- theta.mle$estimate
  a[6] <- sd(data$random)
  b[round(theta.mle$maximum, 3) == round(maxlikelihood, 3)] <- 1
  maxlikelihood[theta.mle$maximum > maxlikelihood] <- theta.mle$maximum  
  print(a)
  if( b > 0){
    break 
  }
}



print(a)
print (llmax)
print(theta.mle$estimate)



###BLP second stage 
##
BLP2 <- lm(theta_j ~ ramp + restroom + walleye + salmon + panfish + 0, data=data)
summary(BLP2) 

###################Q3 
library(mlogit)
library(dplyr)
library(maxLik)
setwd("/Volumes/USB30FD/821_ps")
#####data upload and prepare 
data <- read.csv("long_data.csv")
data <- data[-c(1)]
names(data)[names(data) == "idcase"] <- "id"
data <- mlogit.data(data, choice = "choice", 
                    shape = "long", alt.levels = (c(1:100)), id ="id")
data$shares <- data$shares*100
names(data)[names(data) == "shares"] <- "shares_100"
#Q3.1i:try the simple model first
model_1 <- mlogit (choice ~ ramp + restroom + walleye + salmon 
                   + panfish + travelcost + shares_100| 0, data)

summary(model_1)


#Q3.1ii: preference heterogenerity r.t. person_specific variables 
data$panfish_kids <- data$panfish * data$kids
data$restroom_kids <- data$restroom * data$kids
data$ramp_boat <- data$ramp * data$boat
data$walleye_boat <- data$walleye * data$boat

model_2 <- mlogit (choice ~ ramp + restroom + walleye + salmon + panfish 
                   + travelcost + panfish_kids + restroom_kids
                   + ramp_boat + walleye_boat + shares_100 | 0, data)
summary(model_2)



#Q3.1iii: BLP
data$alt_id <-as.numeric(as.character(data$alt_id))
data$id <-as.numeric(as.character(data$id))
##small loop 
delta = 1e-07 #tolerancel level for first loop to stop 
a <-  c(0, 0, 0, 0, 0)   

theta_1 <- replicate(100,0)
theta_2 <- replicate(100,0)
data$theta_j = 0 
data$choice <- ifelse(data$choice == "FALSE", 0, 1)
#function that produce the prediction of share S_j for all j 
maxlikelihood <- c(2000000000)
b <-c(0)
##
repeat{
  data$Prob_ij_nominator <- exp(0 + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat)         
  
  data <- data %>%
    group_by(id) %>%
    mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
    ungroup()
  
  data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
  data <- data %>%
    group_by(alt_id) %>%
    mutate(share_j = mean(Prob_ij)*100)%>%
    ungroup()
  
  share <- data$share_j[1:100]
  
  real_share <- data$shares_100[1:100]
  
  for (j in 1:100) {
    theta_2[j] <- theta_1[j] + log(real_share[j]) - log(share[j])
  }
  
  theta_diff <- theta_2 - theta_1
  
  
  
  while (max(abs(theta_diff))>delta){
    theta_1 <- theta_2 #update new thetas
    data$theta_j <- theta_1[data$alt_id]
    data <- data %>% 
      mutate(Prob_ij_nominator = exp(data$theta_j + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat))%>%
      group_by(id) %>%
      mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
      ungroup()
    data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
    data <- data %>% 
      group_by(alt_id) %>%
      mutate(share_j = mean(Prob_ij)*100) %>%
      ungroup()
    share <- data$share_j[1:100]
    for (j in 1:100) {
      theta_2[j] <- theta_1[j] + log(real_share[j]) - log(share[j])
    }
    theta_diff <- theta_2 - theta_1
    #print(theta_2)
  }
  
  # theta_2 now is the optimised baseline utility for the specific "a" parameter set.
  # level normalization: substract the mean 
  level_norm <- function (x) {
    scale(x, scale = FALSE)
  }
  
  baseline_util <- level_norm(theta_2) 
  ##update the new theta_j to the dataframe 
  data$theta_j <- baseline_util[data$alt_id]
  
  #x6<- replicate(2404, baseline_util)
  
  ## calculate likelihood function based on the baseline_utility and parameter set
  
  #changed here!!!! be careful 
  data <- data %>%            
    mutate(Prob_ij_nominator = exp(data$theta_j + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat))%>%
    group_by(id) %>%
    mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
    ungroup()
  data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
  #
  data$Prob_sitechoice <- log(data$Prob_ij^data$choice)
  LogL_attributes <- data$Prob_sitechoice
  llmax <- sum(LogL_attributes)
  maxlikelihood[-llmax < maxlikelihood] <- llmax
  
  ## update parameter set by conducting the maximisation loglikelihood 
  #compute loglikelood function  x1, x2, x3, x4, x5, x6
  
  #############################
  
  loglike = function(theta, x) {
    # theta parameter vector; x  
    mu1 = theta[1]
    mu2 = theta[2]
    mu3 = theta[3]
    mu4 = theta[4]
    mu5 = theta[5]
    x[, 20] <- exp( x[, 19] + mu1*x[, 8] +mu2*x[, 15] + mu3*x[, 16]+mu4*x[, 17] + mu5*x[, 18])
    x1 <- as.vector(unlist(x[,20]))
    x2 <- unname(tapply(x1,(seq_along(x1)-1) %/% 100, sum))
    x[, 21] <- cbind(rep(x2, each=100))
    x[, 22] <- x[, 20] / x[, 21]
    x[, 24]<- x[, 22]^x[, 3]
    ans = sum(log(x[, 24]))
    return(ans)
  }
  theta.start = a 
  names(theta.start) = c("mu1", "mu2", "mu3", "mu4", "mu5")
  theta.mle = maxLik (loglike, start=theta.start, x=data, method = "BFGS")
  #theta.mle = optim(par=theta.start, fn=loglike, x=data, method ="BFGS")
  summary(theta.mle)
  summary(theta.mle$coef)
  
  
  # write an additional loop for the first stage to converge 
  # print(theta.mle$estimate)
  print(theta.mle$maximum)
  a[theta.mle$maximum > maxlikelihood] <- theta.mle$estimate
  b[round(theta.mle$maximum, 3) == round(maxlikelihood, 3)] <- 1
  maxlikelihood[round(theta.mle$maximum, 3) > round(maxlikelihood, 3)] <- theta.mle$maximum  
  print(a)
  if( b > 0){
    break 
  }
}


names(a) <- c("travelcost", "panfish_kids", "restroom_kids", "ramp_boat", "walleye_boat")
print(a)
print (maxlikelihood)
print(theta.mle$estimate)

##> print(a)
##[1] -0.1230306 -0.  1408963  0.5048161  1.3881009  0.5524893
##> print (maxlikelihood)
##[1] -5136.971
##> print(theta.mle$estimate)
##mu1        mu2        mu3        mu4        mu5 
##-0.1230306 -0.1408963  0.5048161  1.3881009  0.5524893 

###BLP second stage 
##
BLP2 <- lm(theta_j ~ ramp + restroom + walleye + salmon + panfish + shares_100 + 0, data=data)
summary(BLP2) 
################################################################################################################

#Q3.2
library(quantreg)
library(gmm)
###arange data frame 
data_iv <- data[1:100 ,]
data_iv <- subset(data_iv, select=c("ramp", "restroom", "walleye","salmon","panfish","shares_100","theta_j"))
############generate instruments
###median regression 
rqfit <- rq(theta_j ~ ramp + restroom + walleye + salmon + panfish + shares_100, data=data_iv)
coef <-rqfit$coefficients
###calculate shares as one of instruments
data$Prob_ij_nominator <- exp(coef[1] + coef[2]*data$ramp + coef[3]*data$restroom + coef[4]*data$walleye + coef[5]*data$salmon + coef[6]*data$panfish + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat)         

data <- data %>%
  group_by(id) %>%
  mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
  ungroup()

data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
data <- data %>%
  group_by(alt_id) %>%
  mutate(share_j = mean(Prob_ij))%>%
  ungroup()

data_iv <- data[1:100 ,]
data_iv <- subset(data_iv, select=c("ramp", "restroom", "walleye","salmon","panfish","shares_100","theta_j","share_j"))
data_iv$share_j <- data_iv$share_j*100
####quantile IV GMM
##generate condition function 
g1 <- function (tet, x){
  #tet <- parameter set
  #x <- data_iv dataframe
  #Sn <- 0.25, same as the setting of orginal paper
  # intercept from median regression <- -0.1190958
  m1 <- (pnorm((x[,7] + 0.1190958 - tet[1]*x[,1] - tet[2]*x[,2] - tet[3]*x[,3] - tet[4]*x[,4] - tet[5]*x[,5] - tet[6]*x[,8])/0.25) - 0.5 )
  return(m1)
}

####other method: "inverse" quantile estimation 
install.packages("remotes")
remotes::install_github("yuchang0321/IVQR")
library(IVQR)

fit <- ivqr(theta_j~ shares_100 | share_j | ramp + restroom + walleye + salmon + panfish, 0.5, grid= seq(-4.5 , 0 , 0.05625), data = data_iv)
fit
####Other Method: 2sls
#OLS
ols<- lm(theta_j ~ ramp + restroom + walleye + salmon + panfish + shares_100 + 0, data=data_iv)
coef <-ols$coefficients
#predict shares of visiting as instrument, based on exogenous things only
data$Prob_ij_nominator <- exp( coef[1]*data$ramp + coef[2]*data$restroom + coef[3]*data$walleye + coef[4]*data$salmon + coef[5]*data$panfish + a[1]*data$travelcost +a[2]*data$panfish_kids + a[3]*data$restroom_kids+a[4]*data$ramp_boat + a[5]*data$walleye_boat)         

data <- data %>%
  group_by(id) %>%
  mutate(Prob_ij_denom = sum(Prob_ij_nominator))%>%
  ungroup()

data$Prob_ij <- data$Prob_ij_nominator / data$Prob_ij_denom
data <- data %>%
  group_by(alt_id) %>%
  mutate(share_j = mean(Prob_ij))%>%
  ungroup()

data_iv <- data[1:100 ,]
data_iv <- subset(data_iv, select=c("ramp", "restroom", "walleye","salmon","panfish","shares_100","theta_j","share_j"))
data_iv$share_j <- data_iv$share_j*100

#2SLS 
library(ivpack)
twosls <- ivreg(data_iv$theta_j~ data_iv$shares_100 + data_iv$ramp + data_iv$restroom +data_iv$walleye +data_iv$salmon + data_iv$panfish | data_iv$share_j + data_iv$ramp + data_iv$restroom +data_iv$walleye +data_iv$salmon + data_iv$panfish   )
beta <- c(-3.7598, -0.3720, -0.4888, 8.1968, 20.3990, 1.2842) ##from Q3_2
names(beta) <- c("shares_100" , "ramp", "restroom", "walleye", "salmon", "panfish")
data_iv$unobserved <- data_iv$theta_j - beta[1]*data_iv$shares_100 -beta[2]*data_iv$ramp - beta[3]*data_iv$restroom -beta[4]*data_iv$walleye -beta[5]*data_iv$salmon - beta[6]*data_iv$panfish
####################################################
##data preparation for Q4
unobserved <- data_iv$unobserved

data <- read.csv("long_data.csv")
data <- data[-c(1)]
names(data)[names(data) == "idcase"] <- "id"
data <- mlogit.data(data, choice = "choice", 
                    shape = "long", alt.levels = (c(1:100)), id ="id")
data$shares <- data$shares*100
names(data)[names(data) == "shares"] <- "shares_100"
data$panfish_kids <- data$panfish * data$kids
data$restroom_kids <- data$restroom * data$kids
data$ramp_boat <- data$ramp * data$boat
data$walleye_boat <- data$walleye * data$boat
data$alt_id <-as.numeric(as.character(data$alt_id))
data$id <-as.numeric(as.character(data$id))
data$unobserved <- cbind(rep( unobserved, 2404))
write.csv(data, file = "data_for_welfare_analysis.csv")

###### Welfare Analysis 
##data
library(dplyr)
setwd("C:/821_ps")
data <- read.csv("data_for_welfare_analysis.csv")
data <- data[-c(1)]
data$id <-as.numeric(as.character(data$id))

##generate utility calculation function 
beta <- c(-3.7598, -0.3720, -0.4888, 8.1968, 20.3990, 1.2842) ##from Q3_2
names(beta) <- c("shares_100" , "ramp", "restroom", "walleye", "salmon", "panfish")
alpha <- c(-0.1230306, -0.1408963,  0.5048161,  1.3881009,  0.5524893 )
names(alpha) <- c("travelcost", "panfish_kids", "restroom_kids", "ramp_boat", "walleye_boat")
U <- function(beta, alpha, x) {
  ans <- beta[1]*x[,14] + beta[2]*x[,9] + beta[3]*x[,10]+ beta[4]*x[,11] + beta[5]*x[,12] + beta[6]*x[,13] + alpha[1]*x[,8] + alpha[2]*x[,15] + alpha[3]*x[,16] + alpha[4]*x[,17] + alpha[5]*x[,18]+ x[,19] 
  return(ans)
  
}
#####################i Partial equilibrium 
data$old_utility <- U(beta, alpha, data)
write.csv(data, file="data_welfare.csv")
### scenario A
data$walleye <- data$walleye*1.3
data$walleye_boat <-data$walleye*data$boat
data$new_utility <- U(beta, alpha, data)

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 100, sum))
data$sum_new <- cbind(rep(sum_new, each =100))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 100, sum))
data$sum_old <- cbind(rep(sum_old, each =100))
data$sum_old <- log(data$sum_old)

data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)
mean(data$CV)   ###  5.217842


### scenario B
data <- read.csv("data_welfare.csv")
data <- data[-c(1)]
data$change <- ifelse(data$shares_100 >1.5, 1.3, 1)
data$walleye <- data$walleye*data$change
data$walleye_boat <-data$walleye*data$boat
data$new_utility <- U(beta, alpha, data)

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 100, sum))
data$sum_new <- cbind(rep(sum_new, each =100))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 100, sum))
data$sum_old <- cbind(rep(sum_old, each =100))
data$sum_old <- log(data$sum_old)
data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)

mean_affected <- data$CV[which(data$change == 1.3 & data$choice =="TRUE") ] %>% mean() ###5.09357
mean_unaffected <- mean(data$CV[which(data$change == 1.0 & data$choice =="TRUE") ]) %>% mean() ###2.993604


### scenario C
data <- read.csv("data_welfare.csv")
data <- data[-c(1)]
data <- data[which(data$shares_100 <= 1.5),] ##now we have 82 sites

data$new_utility <- U(beta, alpha, data)

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 82, sum))
data$sum_new <- cbind(rep(sum_new, each =82))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 82, sum))
data$sum_old <- cbind(rep(sum_old, each =82))
data$sum_old <- log(data$sum_old)
data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)

mean_unaffected_by_removal <- data$CV[which(data$choice =="TRUE") ] %>% mean() ###9.157485e-17

### scenario D
data <- read.csv("data_welfare.csv")
data <- data[-c(1)]
data$change <- ifelse(data$shares_100 >1.5, 10, 0)
data$travelcost <- data$travelcost+data$change

data$new_utility <- U(beta, alpha, data)

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 100, sum))
data$sum_new <- cbind(rep(sum_new, each =100))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 100, sum))
data$sum_old <- cbind(rep(sum_old, each =100))
data$sum_old <- log(data$sum_old)
data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)

mean_affected <- data$CV[which(data$change == 10 & data$choice =="TRUE") ] %>% mean() ###-4.563843
mean_unaffected <- mean(data$CV[which(data$change == 0 & data$choice =="TRUE") ]) %>% mean() ###-2.341332


######################ii general equilibrium <- resorting 

### scenario A
data <- read.csv("data_welfare.csv")
data <- data[-c(1)]
data$walleye <- data$walleye*1.3
data$walleye_boat <-data$walleye*data$boat
#stimulate new choice set 
data$nominator <- exp(U(beta, alpha, data))
nominator <- data$nominator
denominator <- unname(tapply(nominator,(seq_along(nominator)-1) %/% 100, sum))
data$denomitor <- cbind(rep(denominator, each =100))
data$pij <- data$nominator/data$denomitor
data<-arrange(data, data$alt_id)
pij <- data$pij
pij_mean <- unname(tapply(pij,(seq_along(pij)-1) %/% 2404, sum))
data$shares_new <- cbind(rep(pij_mean, each =2404))
data<-arrange(data, data$id)
##calculate new utility 
U_GE <- function(beta, alpha, x) {
  ans <- beta[1]*x[,24] + beta[2]*x[,9] + beta[3]*x[,10]+ beta[4]*x[,11] + beta[5]*x[,12] + beta[6]*x[,13] + alpha[1]*x[,8] + alpha[2]*x[,15] + alpha[3]*x[,16] + alpha[4]*x[,17] + alpha[5]*x[,18]+ x[,19] 
  return(ans)
}
data$new_utility <- U_GE(beta, alpha, data) 

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 100, sum))
data$sum_new <- cbind(rep(sum_new, each =100))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 100, sum))
data$sum_old <- cbind(rep(sum_old, each =100))
data$sum_old <- log(data$sum_old)

data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)
mean(data$CV)   ###  -147.337

### scenario B
data <- read.csv("data_welfare.csv")
data <- data[-c(1)]
data$change <- ifelse(data$shares_100 >1.5, 1.3, 1)
data$walleye <- data$walleye*data$change
data$walleye_boat <-data$walleye*data$boat
#stimulate new choice set 
data$nominator <- exp(U(beta, alpha, data))
nominator <- data$nominator
denominator <- unname(tapply(nominator,(seq_along(nominator)-1) %/% 100, sum))
data$denomitor <- cbind(rep(denominator, each =100))
data$pij <- data$nominator/data$denomitor
data<-arrange(data, data$alt_id)
pij <- data$pij
pij_mean <- unname(tapply(pij,(seq_along(pij)-1) %/% 2404, sum))
data$shares_new <- cbind(rep(pij_mean, each =2404))
data<-arrange(data, data$id)
##calculate new utility 
U_GE <- function(beta, alpha, x) {
  ans <- beta[1]*x[,25] + beta[2]*x[,9] + beta[3]*x[,10]+ beta[4]*x[,11] + beta[5]*x[,12] + beta[6]*x[,13] + alpha[1]*x[,8] + alpha[2]*x[,15] + alpha[3]*x[,16] + alpha[4]*x[,17] + alpha[5]*x[,18]+ x[,19] 
  return(ans)
}
data$new_utility <- U_GE(beta, alpha, data)

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 100, sum))
data$sum_new <- cbind(rep(sum_new, each =100))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 100, sum))
data$sum_old <- cbind(rep(sum_old, each =100))
data$sum_old <- log(data$sum_old)
data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)
## update actual choice 
data <- arrange(data, id)
new_utility <- c(data$new_utility)
data$new_utility <- new_utility
data<- arrange(data, id, desc(data$new_utility))
data$new_choice = "FALSE"
for (i in 0:2403 ){
  data$new_choice[(100*i+1)] <- "TRUE"
}

mean_affected <- data$CV[which(data$change == 1.3 & data$new_choice =="TRUE") ] %>% mean() ###NaN, suggesting no person change to the place when walleye increased in the crowded place.
mean_unaffected <- mean(data$CV[which(data$change == 1.0 & data$new_choice =="TRUE") ]) %>% mean() ###-144.6079


###Scenerio C
data <- read.csv("data_welfare.csv")
data <- data[-c(1)]
data <- data[which(data$shares_100 <= 1.5),] ##now we have 82 sites
#stimulate new choice set 
data$nominator <- exp(U(beta, alpha, data))
nominator <- data$nominator
denominator <- unname(tapply(nominator,(seq_along(nominator)-1) %/% 82, sum))
data$denomitor <- cbind(rep(denominator, each =82))
data$pij <- data$nominator/data$denomitor
data<-arrange(data, data$alt_id)
pij <- data$pij
pij_mean <- unname(tapply(pij,(seq_along(pij)-1) %/% 2404, sum))
data$shares_new <- cbind(rep(pij_mean, each =2404))
data<-arrange(data, data$id)
##calculate new utility 
U_GE <- function(beta, alpha, x) {
  ans <- beta[1]*x[,24] + beta[2]*x[,9] + beta[3]*x[,10]+ beta[4]*x[,11] + beta[5]*x[,12] + beta[6]*x[,13] + alpha[1]*x[,8] + alpha[2]*x[,15] + alpha[3]*x[,16] + alpha[4]*x[,17] + alpha[5]*x[,18]+ x[,19] 
  return(ans)
}
data$new_utility <- U_GE(beta, alpha, data)

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 82, sum))
data$sum_new <- cbind(rep(sum_new, each =82))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 82, sum))
data$sum_old <- cbind(rep(sum_old, each =82))
data$sum_old <- log(data$sum_old)
data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)

mean_unaffected_by_removal <- data$CV[which(data$choice =="TRUE") ] %>% mean() ###-350.9764

### scenario D
data <- read.csv("data_welfare.csv")
data <- data[-c(1)]
data$change <- ifelse(data$shares_100 >1.5, 10, 0)
data$travelcost <- data$travelcost+data$change
#stimulate new choice set 
data$nominator <- exp(U(beta, alpha, data))
nominator <- data$nominator
denominator <- unname(tapply(nominator,(seq_along(nominator)-1) %/% 100, sum))
data$denomitor <- cbind(rep(denominator, each =100))
data$pij <- data$nominator/data$denomitor
data<-arrange(data, data$alt_id)
pij <- data$pij
pij_mean <- unname(tapply(pij,(seq_along(pij)-1) %/% 2404, sum))
data$shares_new <- cbind(rep(pij_mean, each =2404))
data<-arrange(data, data$id)
##calculate new utility 
U_GE <- function(beta, alpha, x) {
  ans <- beta[1]*x[,25] + beta[2]*x[,9] + beta[3]*x[,10]+ beta[4]*x[,11] + beta[5]*x[,12] + beta[6]*x[,13] + alpha[1]*x[,8] + alpha[2]*x[,15] + alpha[3]*x[,16] + alpha[4]*x[,17] + alpha[5]*x[,18]+ x[,19] 
  return(ans)
}
data$new_utility <- U_GE(beta, alpha, data)

data$old_utility <- exp(data$old_utility)
data$new_utility <- exp(data$new_utility)

new_utility <- data$new_utility
sum_new <- unname(tapply(new_utility,(seq_along(new_utility)-1) %/% 100, sum))
data$sum_new <- cbind(rep(sum_new, each =100))
data$sum_new <- log(data$sum_new)

old_utility <- data$old_utility
sum_old <- unname(tapply(old_utility,(seq_along(old_utility)-1) %/% 100, sum))
data$sum_old <- cbind(rep(sum_old, each =100))
data$sum_old <- log(data$sum_old)
data$CV <- (1/0.123131)*(data$sum_new-data$sum_old)
## update actual choice 
data <- arrange(data, id)
new_utility <- c(data$new_utility)
data$new_utility <- new_utility
data<- arrange(data, id, desc(data$new_utility))
data$new_choice = "FALSE"
for (i in 0:2403 ){
  data$new_choice[(100*i+1)] <- "TRUE"
}

mean_affected <- data$CV[which(data$change == 10 & data$new_choice =="TRUE") ] %>% mean() ###NaN
mean_unaffected <- mean(data$CV[which(data$change == 0 & data$new_choice =="TRUE") ]) %>% mean() ###-345.9996

