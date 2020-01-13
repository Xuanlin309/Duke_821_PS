setwd("/Volumes/USB30FD/821_ps/hedonic")
library(dplyr)
library(purrr)
library(psych)
#Q1
###########################################################
la_data<- read.table("la_data.txt", header = FALSE)
names(la_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
la_sta <- la_data %>%
  group_by(county) %>%
  summarise(price_mean=mean(price), year_built_mean=mean(year_built), sq_footage_mean=mean(sq_footage), bathrooms_mean=mean(bathrooms),bedrooms_mean=mean(bedrooms), total_rooms_mean=mean(total_rooms),stories_mean=mean(stories), violent_crime_rate_mean=mean(violent_crime_rate), property_crime_rate_mean=mean(property_crime_rate), year_of_sale_mean=mean(year_of_sale),
            price_var=var(price), year_built_var=var(year_built), sq_footage_var=var(sq_footage), bathrooms_var=var(bathrooms),bedrooms_var=var(bedrooms), total_rooms_var=var(total_rooms),stories_var=var(stories), violent_crime_rate_var=var(violent_crime_rate), property_crime_rate_var=var(property_crime_rate), year_of_sale_var=var(year_of_sale))
la_sta$city <- "LA"

sf_data<- read.table("sf_data.txt", header = FALSE)
names(sf_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
sf_sta <-sf_data %>%
  subset(., sf_data$county==1|13|75|81|85) %>%
  group_by(county) %>%
  summarise(price_mean=mean(price), year_built_mean=mean(year_built), sq_footage_mean=mean(sq_footage), bathrooms_mean=mean(bathrooms),bedrooms_mean=mean(bedrooms), total_rooms_mean=mean(total_rooms),stories_mean=mean(stories), violent_crime_rate_mean=mean(violent_crime_rate), property_crime_rate_mean=mean(property_crime_rate), year_of_sale_mean=mean(year_of_sale),
            price_var=var(price), year_built_var=var(year_built), sq_footage_var=var(sq_footage), bathrooms_var=var(bathrooms),bedrooms_var=var(bedrooms), total_rooms_var=var(total_rooms),stories_var=var(stories), violent_crime_rate_var=var(violent_crime_rate), property_crime_rate_var=var(property_crime_rate), year_of_sale_var=var(year_of_sale))
sf_sta$city <- "SF"

##combine two stats table
total_sta <- rbind(la_sta, sf_sta) 
#########################################################################

#Q2

rm(list=ls())
######################################################
installation_needed  <- TRUE
loading_needed <- TRUE
package_list <- c('foreign', 'xtable', 'plm','gmm', 'AER','stargazer','readstata13', 'boot')
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}
library(boot)
library("dummies")
library(dplyr)
library(purrr)
library(psych)
##########################
# data & model_LA
la_data<- read.table("la_data.txt", header = FALSE)
names(la_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
la_data <- cbind(la_data, dummy(la_data$county, sep = "."))
la_data <- cbind(la_data, dummy(la_data$year_of_sale, sep = "."))
model_la <- price ~ bathrooms + bedrooms + stories + property_crime_rate + I(property_crime_rate^2) + year_built + I(year_built^2) + sq_footage + I(sq_footage^2) + total_rooms + I(total_rooms^2) + violent_crime_rate + I(violent_crime_rate^2) + bathrooms + la_data.1993 + la_data.1994 + la_data.1995 + la_data.1996 + la_data.1997 + la_data.1998 + la_data.2000 + la_data.2001 + la_data.2002 + la_data.2003 + la_data.2004 + la_data.2005 + la_data.2006 + la_data.2007 + la_data.2008 + la_data.59 + la_data.65 + la_data.71 + la_data.111

# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 500 replications 
results <- boot(data=la_data, statistic=bs, 
                R=500, formula=model_la)
results 
result_bootcef_la <- cbind(results$t[,13],results$t[,14])
write.csv(result_bootcef_la, file="result_bootcef_la.csv")
capture.output(results, file = "results_boots_la.txt", append = TRUE)
###################
# data & model_SF
sf_data<- read.table("sf_data.txt", header = FALSE)
names(sf_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
select(sf_data, sf_data$county==1|13|75|81|85)
sf_data <- cbind(sf_data, dummy(sf_data$county, sep = "."))
sf_data <- cbind(sf_data, dummy(sf_data$year_of_sale, sep = "."))
model_sf <- price ~ bathrooms + bedrooms + stories + property_crime_rate + I(property_crime_rate^2) + year_built + I(year_built^2) + sq_footage + I(sq_footage^2) + total_rooms + I(total_rooms^2) + violent_crime_rate + I(violent_crime_rate^2) + bathrooms + sf_data.1993 + sf_data.1994 + sf_data.1995 + sf_data.1996 + sf_data.1997 + sf_data.1998 + sf_data.2000 + sf_data.2001 + sf_data.2002 + sf_data.2003 + sf_data.2004 + sf_data.2005 + sf_data.2006 + sf_data.2007 + sf_data.2008 + sf_data.13 + sf_data.75 + sf_data.81 + sf_data.85

# bootstrapping with 500 replications 
results_boot_sf <- boot(data=sf_data, statistic=bs, 
                R=500, formula=model_sf)
results_boot_sf
result_bootcef_sf <- cbind(results_boot_sf$t[,13],results$t[,14])
write.csv(result_bootcef_sf, file="result_bootcef_sf.csv")
capture.output(results_boot_sf, file = "results_boots_sf.txt", append = TRUE)
#####################################################################################

#Q3
rm(list=ls())
###################### data read-in 
##buyers 
buyers_data<- read.table("buyer_data_sf_la.txt", header = FALSE)
names(buyers_data) <- c("buyerid","price","violent_crime_rate","property_crime_rate", "race", "income", "LA_indicator")
## boostrapped hedonic price gradients from Q2 
la_gradients <- read.csv("result_bootcef_la.csv")
sf_gradients <- read.csv("result_bootcef_sf.csv")
gradients_la_sf <- merge(la_gradients,sf_gradients, by="boot_round")
rm(la_gradients,sf_gradients)
######################assign implicit price of crime for each individual *500
for( i in 1:500){
  buyers_data[paste("implict_price", i, sep= "")] <- (buyers_data$LA_indicator)*gradients_la_sf[i,2]+ (1-buyers_data$LA_indicator)*gradients_la_sf[i,4] + 2*gradients_la_sf[i,3]*(buyers_data$violent_crime_rate)*(buyers_data$LA_indicator) + 2*gradients_la_sf[i,5]*(buyers_data$violent_crime_rate)*(1-buyers_data$LA_indicator)
}
######################run second stage regression $ save coefficients *500
##create dummies for race
buyers_data <- cbind(buyers_data, dummy(buyers_data$race, sep = "."))
##regression
coef1 <-c()
for (j in 1:500){
  test_data <- cbind(buyers_data[, 1:7], buyers_data[, 508:511], buyers_data[, 7+j])
  colnames(test_data)[12] <- "implicit_price"
  model <- lm(implicit_price ~ violent_crime_rate + buyers_data.3 + buyers_data.4 + buyers_data.5 + income + LA_indicator, data=test_data)
  newcoef1 <- model$coef
  coef1 <- rbind(coef1,newcoef1)
}
print(coef1)
#write.csv(coef1, file="second_stage_500.csv")

######################the actural point estimation
##first stage of LA, result: -2.022722e+02; 7.382382e-02
la_data<- read.table("la_data.txt", header = FALSE)
names(la_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
la_data <- cbind(la_data, dummy(la_data$county, sep = "."))
la_data <- cbind(la_data, dummy(la_data$year_of_sale, sep = "."))
model_la_actuall <- lm(price ~ bathrooms + bedrooms + stories + property_crime_rate + I(property_crime_rate^2) + year_built + I(year_built^2) + sq_footage + I(sq_footage^2) + total_rooms + I(total_rooms^2) + violent_crime_rate + I(violent_crime_rate^2) + bathrooms + la_data.1993 + la_data.1994 + la_data.1995 + la_data.1996 + la_data.1997 + la_data.1998 + la_data.2000 + la_data.2001 + la_data.2002 + la_data.2003 + la_data.2004 + la_data.2005 + la_data.2006 + la_data.2007 + la_data.2008 + la_data.59 + la_data.65 + la_data.71 + la_data.111, data=la_data)
summary(model_la_actuall)
##first stage of SF, result:  -2.251711e+02 1.116099e-01 
sf_data<- read.table("sf_data.txt", header = FALSE)
names(sf_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
select(sf_data, sf_data$county==1|13|75|81|85)
sf_data <- cbind(sf_data, dummy(sf_data$county, sep = "."))
sf_data <- cbind(sf_data, dummy(sf_data$year_of_sale, sep = "."))
model_sf_actuall <- lm(price ~ bathrooms + bedrooms + stories + property_crime_rate + I(property_crime_rate^2) + year_built + I(year_built^2) + sq_footage + I(sq_footage^2) + total_rooms + I(total_rooms^2) + violent_crime_rate + I(violent_crime_rate^2) + bathrooms + sf_data.1993 + sf_data.1994 + sf_data.1995 + sf_data.1996 + sf_data.1997 + sf_data.1998 + sf_data.2000 + sf_data.2001 + sf_data.2002 + sf_data.2003 + sf_data.2004 + sf_data.2005 + sf_data.2006 + sf_data.2007 + sf_data.2008 + sf_data.13 + sf_data.75 + sf_data.81 + sf_data.85, data=sf_data)
summary(model_sf_actuall)
##second stage 
buyers_data<- read.table("buyer_data_sf_la.txt", header = FALSE)
names(buyers_data) <- c("buyerid","price","violent_crime_rate","property_crime_rate", "race", "income", "LA_indicator")
buyers_data$implicit_price <- (buyers_data$LA_indicator)*-2.022722e+02+ (1-buyers_data$LA_indicator)*-2.251711e+02 + 2*7.382382e-02*(buyers_data$violent_crime_rate)*(buyers_data$LA_indicator) + 2*1.116099e-01*(buyers_data$violent_crime_rate)*(1-buyers_data$LA_indicator)
buyers_data <- cbind(buyers_data, dummy(buyers_data$race, sep = "."))
model_second_stage_actuall <- lm(implicit_price ~ violent_crime_rate + buyers_data.3 + buyers_data.4 + buyers_data.5 + income + LA_indicator, data=buyers_data)
summary(model_second_stage_actuall)

########################standard eroors by taking sd of bootstrapped parameters:0.003297735
secondstage_boot <-read.csv("second_stage_500.csv")
sd(secondstage_boot$violent_crime_rate)
###############################################################################################################

#Q4
rm(list=ls())

library(dplyr)
library(purrr)
library(psych)
library(dummies)

##import data of LA 
la_data<- read.table("la_data.txt", header = FALSE)
names(la_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")

##create a function that generates weight vectors (length: # observations) for each X 1:2000 and h:1:3 
la_data$weight <- 0
weight <-  function(x,h,theta){
  sigma <- sd(x[, 10])
  x[,13]<- (1/(h*sigma))*(1/sqrt(2*pi))*exp(-0.5*(((x[,10]-theta)/(h*sigma))^2))
  ans <- x[,13]
  return(ans)
}

## for h=1 
weight1 <- list()
for (i in 1:2000){
  weight1[[i]]<- weight(la_data, 1, i) 
}
## for h=3 
weight3 <- list()
for (i in 1:2000){
  weight3[[i]]<- weight(la_data, 3, i) 
}
## for h=10 
weight10 <- list()
for (i in 1:2000){
  weight10[[i]]<- weight(la_data, 10, i) 
}

## for h=1000 
weight1000 <- list()
for (i in 1:2000){
  weight1000[[i]]<- weight(la_data, 1000, i) 
}

###############save gradient vector
##model 
model <- price ~ violent_crime_rate
##h1
price_gradient_h1 <- replicate(2000,0)
for (i in 1:2000){
  estimation <-lm(model, la_data, weights=weight1[[i]])
  price_gradient_h1[i] <- estimation$coefficients[2] 
}
save(price_gradient_h1, file = "h1_price_gradients.Rdata")
##h3
price_gradient_h3 <- replicate(2000,0)
for (i in 1:2000){
  estimation <-lm(model, la_data, weights=weight3[[i]])
  price_gradient_h3[i] <- estimation$coefficients[2] 
}

##h10
price_gradient_h10 <- replicate(2000,0)
for (i in 1:2000){
  estimation <-lm(model, la_data, weights=weight10[[i]])
  price_gradient_h10[i] <- estimation$coefficients[2] 
}

##h1000
price_gradient_h1000 <- replicate(2000,0)
for (i in 1:2000){
  estimation <-lm(model, la_data, weights=weight1000[[i]])
  price_gradient_h1000[i] <- estimation$coefficients[2] } 
price_gradient_h1000

### We eventually get the final result! Let plot it now 
x_axis <- c(1:2000)
plot (x_axis, price_gradient_h1, type="b", pch = 19, col="blue", ylab="Price Gradients", xlab="X")
lines(x_axis, price_gradient_h1000, col="red", type="b")
lines(x_axis, price_gradient_h3, col="green", type="b")
lines(x_axis, price_gradient_h10, col="black", type="b")
legend("topleft", legend=c("h=1", "h=3","h=10", "h=1000"), 
       col=c("blue", "green", "black", "red"), lty=1:4, cex=0.6)
#####################################################################################
###  Q5
rm(list=ls())

library(dplyr)
library(purrr)
library(psych)
library(dummies)

##import buyer data
la_buyer_data<- read.table("buyer_data_la.txt", header = FALSE)
names(la_buyer_data) <- c("buyer_id","price","violent_crime_rate","property_crime_rate","race","income")
la_buyer_data <- cbind(la_buyer_data, dummy(la_buyer_data$race, sep = "."))
names(la_buyer_data) <- c("buyer_id","price","violent_crime_rate","property_crime_rate","race","income",
                          "Asian_pi", "black", "hispanic", "white")
                          
## import price gradients results 
price_gradient <- readRDS("h1_price_gradients.Rdata")

##allocate WMTP to each individual 
la_buyer_data$violent_crime_rate[la_buyer_data$violent_crime_rate >= 2000] <- 2000
la_buyer_data$MWTP <- price_gradient_h1[as.integer(la_buyer_data$violent_crime_rate)]
MWTP_estimation <- lm(MWTP~income+Asian_pi+black+hispanic, data=la_buyer_data)
summary(MWTP_estimation)
#######################################################################################



###Q6
rm(list=ls())
#First_step:boostrap 
######################################################
installation_needed  <- TRUE
loading_needed <- TRUE
package_list <- c('foreign', 'xtable', 'plm','gmm', 'AER','stargazer','readstata13', 'boot')
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}
library(boot)
library("dummies")
library(dplyr)
library(purrr)
library(psych)
##########################
# data & model_LA
la_data<- read.table("la_data.txt", header = FALSE)
names(la_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
la_data <- cbind(la_data, dummy(la_data$county, sep = "."))
la_data <- cbind(la_data, dummy(la_data$year_of_sale, sep = "."))
model_la <- price ~ bathrooms + bedrooms + stories + property_crime_rate + I(property_crime_rate^2) + year_built + I(year_built^2) + sq_footage + I(sq_footage^2) + total_rooms + I(total_rooms^2)+ violent_crime_rate + I(violent_crime_rate^2) + I(violent_crime_rate^3)+ I(violent_crime_rate^4)+ I(violent_crime_rate^5)+ I(violent_crime_rate^6) + bathrooms + la_data.1993 + la_data.1994 + la_data.1995 + la_data.1996 + la_data.1997 + la_data.1998 + la_data.2000 + la_data.2001 + la_data.2002 + la_data.2003 + la_data.2004 + la_data.2005 + la_data.2006 + la_data.2007 + la_data.2008 + la_data.59 + la_data.65 + la_data.71 + la_data.111

# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 500 replications 
results <- boot(data=la_data, statistic=bs, 
                R=500, formula=model_la)
results 
result_bootcef_la <- cbind(results$t[,13],results$t[,14],results$t[,15],results$t[,16],results$t[,17],results$t[,18])
write.csv(result_bootcef_la, file="q6_result_bootcef_la.csv")
capture.output(results, file = "results_boots_la.txt", append = TRUE)
###################
# data & model_SF
sf_data<- read.table("sf_data.txt", header = FALSE)
names(sf_data) <- c("houseid","price","county","year_built","sq_footage","bathrooms","bedrooms","total_rooms","stories","violent_crime_rate","property_crime_rate", "year_of_sale")
select(sf_data, sf_data$county==1|13|75|81|85)
sf_data <- cbind(sf_data, dummy(sf_data$county, sep = "."))
sf_data <- cbind(sf_data, dummy(sf_data$year_of_sale, sep = "."))
model_sf <- price ~ bathrooms + bedrooms + stories + property_crime_rate + I(property_crime_rate^2) + year_built + I(year_built^2) + sq_footage + I(sq_footage^2) + total_rooms + I(total_rooms^2) + violent_crime_rate + I(violent_crime_rate^2) + I(violent_crime_rate^3)+ I(violent_crime_rate^4)+ I(violent_crime_rate^5)+ I(violent_crime_rate^6) + bathrooms + sf_data.1993 + sf_data.1994 + sf_data.1995 + sf_data.1996 + sf_data.1997 + sf_data.1998 + sf_data.2000 + sf_data.2001 + sf_data.2002 + sf_data.2003 + sf_data.2004 + sf_data.2005 + sf_data.2006 + sf_data.2007 + sf_data.2008 + sf_data.13 + sf_data.75 + sf_data.81 + sf_data.85

# bootstrapping with 500 replications 
results_boot_sf <- boot(data=sf_data, statistic=bs, 
                        R=500, formula=model_sf)
results_boot_sf
result_bootcef_sf <- cbind(results_boot_sf$t[,13],results_boot_sf$t[,14],results_boot_sf$t[,15],results_boot_sf$t[,16],results_boot_sf$t[,17],results_boot_sf$t[,18])
write.csv(result_bootcef_sf, file="q6_result_bootcef_sf.csv")
capture.output(results_boot_sf, file = "results_boots_sf.txt", append = TRUE)
#####################################################################################
##Bishop Timmins
library(mlogit)
library(dplyr)
library(maxLik)
##buyers data
buyers_data<- read.table("buyer_data_sf_la.txt", header = FALSE)
names(buyers_data) <- c("buyerid","price","violent_crime_rate","property_crime_rate", "race", "income", "LA_indicator")
## boostrapped hedonic price gradients from Q5 
la_gradients <- read.csv("q6_result_bootcef_la.csv")
sf_gradients <- read.csv("q6_result_bootcef_sf.csv")
gradients_la_sf <- merge(la_gradients,sf_gradients, by="X")
rm(la_gradients,sf_gradients)
#####point estimation:
point_la <-lm(model_la, data=la_data)
point_gradient_la <- point_la$coefficients[13:18]
point_sf <- lm(model_sf, data=sf_data)
point_gradient_sf <- point_sf$coefficients[13:18]
point_gradient <- cbind(point_gradient_la, point_gradient_sf)
buyers_data$implicit_price <- (buyers_data$LA_indicator)*point_gradient[1,1]+ (1-buyers_data$LA_indicator)*point_gradient[1,2]+ 2*point_gradient[2,1]*(buyers_data$violent_crime_rate)*(buyers_data$LA_indicator) + 2*point_gradient[2,2]*(buyers_data$violent_crime_rate)*(1-buyers_data$LA_indicator)+ 3*point_gradient[3,1]*(buyers_data$violent_crime_rate^2)*(buyers_data$LA_indicator) + 3*point_gradient[3,2]*(buyers_data$violent_crime_rate^2)*(1-buyers_data$LA_indicator)+ 4*point_gradient[4,1]*(buyers_data$violent_crime_rate^3)*(buyers_data$LA_indicator) + 4*point_gradient[4,2]*(buyers_data$violent_crime_rate^3)*(1-buyers_data$LA_indicator)+ 5*point_gradient[5,1]*(buyers_data$violent_crime_rate^4)*(buyers_data$LA_indicator) + 5*point_gradient[5,2]*(buyers_data$violent_crime_rate^4)*(1-buyers_data$LA_indicator)+ 6*point_gradient[6,1]*(buyers_data$violent_crime_rate^5)*(buyers_data$LA_indicator) + 6*point_gradient[6,2]*(buyers_data$violent_crime_rate^5)*(1-buyers_data$LA_indicator)
buyers_data$df_implict_price <- 2*point_gradient[2,1]*(buyers_data$LA_indicator) + 2*point_gradient[2,2]*(1-buyers_data$LA_indicator)+ 6*point_gradient[3,1]*(buyers_data$violent_crime_rate)*(buyers_data$LA_indicator) + 6*point_gradient[3,2]*(buyers_data$violent_crime_rate)*(1-buyers_data$LA_indicator)+ 12*point_gradient[4,1]*(buyers_data$violent_crime_rate^2)*(buyers_data$LA_indicator) + 12*point_gradient[4,2]*(buyers_data$violent_crime_rate^2)*(1-buyers_data$LA_indicator)+ 20*point_gradient[5,1]*(buyers_data$violent_crime_rate^3)*(buyers_data$LA_indicator) + 20*point_gradient[5,2]*(buyers_data$violent_crime_rate^3)*(1-buyers_data$LA_indicator)+ 30*point_gradient[6,1]*(buyers_data$violent_crime_rate^4)*(buyers_data$LA_indicator) + 30*point_gradient[6,2]*(buyers_data$violent_crime_rate^4)*(1-buyers_data$LA_indicator)
##create dummies for race 
buyers_data <- cbind(buyers_data, dummy(buyers_data$race, sep = "."))
#generate mle function 
mle = function(theta, x) {
  # theta parameter vector; x  
  mu1 = theta[1]
  mu2 = theta[2]
  mu3 = theta[3]
  mu4 = theta[4]
  mu5 = theta[5]
  mu6 = theta[6]
  #x[,14]:vij 
  x[, 14] <- x[,8] - mu1 -mu2*x[,3]-mu3*x[,6]-mu4*x[,10]-mu5*x[,11]-mu6*x[,12]
  sigma <- sd(x[,14])
  x[, 15] <- (1/(sigma*sqrt(2*pi)))*exp(-(1/(2*(sigma^2)))*(x[,14]^2))*abs((x[,9]-mu2))
  x <- subset(x, x[,15] >0 ) ###We need to drop 0 value!!!!! fairly important for mle method to work 
  ans <- sum(log(x[,15]))
  return(ans)
}
#use ols to try the start point 
try <- lm(implicit_price~ violent_crime_rate + income + buyers_data.2 + buyers_data.3 + buyers_data.4, data=buyers_data)
theta.start = c(-200,0.169, 0,0,0,0) 
names(theta.start) = c("mu1", "mu2", "mu3", "mu4", "mu5", "mu6")
#theta.mle = maxLik (mle, start=theta.start, x=buyers_data, method = "BFGS")  ###the problem here is that mle could be very sensitive about the start point
theta.mle = optim(par=theta.start, fn=mle, x=buyers_data, method ="BFGS")
######################assign df(implicit price) of crime for each individual *500
buyers_data<- read.table("buyer_data_sf_la.txt", header = FALSE)
names(buyers_data) <- c("buyerid","price","violent_crime_rate","property_crime_rate", "race", "income", "LA_indicator")
for( i in 1:500){
  buyers_data[paste("implict_price", i, sep= "")] <- (buyers_data$LA_indicator)*gradients_la_sf[i,2]+ (1-buyers_data$LA_indicator)*gradients_la_sf[i,8]+ 2*gradients_la_sf[i,3]*(buyers_data$violent_crime_rate)*(buyers_data$LA_indicator) + 2*gradients_la_sf[i,9]*(buyers_data$violent_crime_rate)*(1-buyers_data$LA_indicator)+ 3*gradients_la_sf[i,4]*(buyers_data$violent_crime_rate^2)*(buyers_data$LA_indicator) + 3*gradients_la_sf[i,10]*(buyers_data$violent_crime_rate^2)*(1-buyers_data$LA_indicator)+ 4*gradients_la_sf[i,4]*(buyers_data$violent_crime_rate^3)*(buyers_data$LA_indicator) + 4*gradients_la_sf[i,11]*(buyers_data$violent_crime_rate^3)*(1-buyers_data$LA_indicator)+ 5*gradients_la_sf[i,6]*(buyers_data$violent_crime_rate^4)*(buyers_data$LA_indicator) + 5*gradients_la_sf[i,12]*(buyers_data$violent_crime_rate^4)*(1-buyers_data$LA_indicator)+ 6*gradients_la_sf[i,7]*(buyers_data$violent_crime_rate^5)*(buyers_data$LA_indicator) + 6*gradients_la_sf[i,13]*(buyers_data$violent_crime_rate^5)*(1-buyers_data$LA_indicator)
}
######################assign df(implicit price) of crime for each individual *500
for( i in 1:500){
  buyers_data[paste("df_implict_price", i, sep= "")] <- 2*gradients_la_sf[i,3]*(buyers_data$LA_indicator) + 2*gradients_la_sf[i,9]*(1-buyers_data$LA_indicator)+ 6*gradients_la_sf[i,4]*(buyers_data$violent_crime_rate)*(buyers_data$LA_indicator) + 6*gradients_la_sf[i,10]*(buyers_data$violent_crime_rate)*(1-buyers_data$LA_indicator)+ 12*gradients_la_sf[i,4]*(buyers_data$violent_crime_rate^2)*(buyers_data$LA_indicator) + 12*gradients_la_sf[i,11]*(buyers_data$violent_crime_rate^2)*(1-buyers_data$LA_indicator)+ 20*gradients_la_sf[i,6]*(buyers_data$violent_crime_rate^3)*(buyers_data$LA_indicator) + 20*gradients_la_sf[i,12]*(buyers_data$violent_crime_rate^3)*(1-buyers_data$LA_indicator)+ 30*gradients_la_sf[i,7]*(buyers_data$violent_crime_rate^4)*(buyers_data$LA_indicator) + 30*gradients_la_sf[i,13]*(buyers_data$violent_crime_rate^4)*(1-buyers_data$LA_indicator)
}
######################run second stage regression $ save coefficients *500
##create dummies for race
buyers_data <- cbind(buyers_data, dummy(buyers_data$race, sep = "."))
theta.start <- c(-1.939636e+02,  4.428857e-01,  9.380051e+05,  9.751060e-01,  2.879410e-01,  1.159335e+00)
names(theta.start) = c("mu1", "mu2", "mu3", "mu4", "mu5", "mu6")
intercept <- c()
violent_crime <- c()
income <- c()
asian_pi <-c()
black <- c()
hispanic <- c()
for (i in 1:500){
  sample <- buyers_data[, c(1:7,7+i,507+i,1008:1011)]
  theta.mle = optim(par=theta.start, fn=mle, x=sample, method ="BFGS")
  intercept[i] <- theta.mle$par[1]
  violent_crime[i] <- theta.mle$par[2]
  income[i] <- theta.mle$par[3]
  asian_pi[i] <- theta.mle$par[4]
  black[i] <- theta.mle$par[5]
  hispanic[i] <- theta.mle$par[6]
}

###drop outlier before summarizing boostrap results
install.packages("outliers")
library(outliers)
boxplot(income)
violent_crime <- rm.outlier(violent_crime)
violent_crime <- rm.outlier(violent_crime)

mean(violent_crime)
sd(violent_crime)



