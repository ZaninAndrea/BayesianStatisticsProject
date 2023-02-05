setwd("C:/Users/panze/Desktop/BAYESIAN STATISTICS/PROGETTO/Dataset")

library(rjags)
library(coda)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
require(gplots)
require(ggpubr)  

#We go through Stochastic Search Variable Selection (SSVS) approach  to do Bayesian Feature Selection among all the numerical variables we reconduced to

DATA <- read.csv("house12.csv", header=TRUE, sep=",", row.names=1)
#We standardize continuous variables and AgeofHouse because, even if it is discrete, it has too big values
DATA_scale <- scale(DATA[,c(1,3,4,5,7,8,11,12)])
DATA_scale <- cbind(DATA_scale,DATA[,c(2,6,9,10)])

sub.idx = 2:12
num.data = dim(DATA_scale)[1]

X <- as.matrix(DATA_scale[1:num.data,sub.idx])
Y <- as.vector(DATA_scale[1:num.data,1])

N <- dim(X)[1]
p <- dim(X)[2]

#Parameters of the spike slab prior
c_ss <- 10
intersect <- 0.05
tau_ss <- intersect / sqrt(2 * log(c_ss) * c_ss^2/(c_ss^2 - 1))

#The variance of the quasi-spike prior is
tau_ss^2

#While the variance of the slab prior is 
(tau_ss*c_ss)^2

x11()
curve(dnorm(x, 0, tau_ss), xlim = c(-5, 5))
curve(dnorm(x, 0, tau_ss * c_ss), xlim = c(-5, 5), add = T, col = 2)

#Data to pass to JAGS (see the code in SSVS.bug)
data_JAGS_SSVS <- list(N = N, p = p, Y = Y, X = as.matrix(X), 
                       tau_ss = tau_ss, c_ss = c_ss)

#A list of initial value for the MCMC algorithm that JAGS will implement
inits_SSVS = function() {
  list(beta0 = 0.0, beta = rep(0,p), g = rep(0,p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model_SSVS = jags.model("SSVS.bug",
                        data = data_JAGS_SSVS,
                        n.adapt = 10000,
                        inits = inits_SSVS,
                        n.chains = 1) 

#Burn-in
update(model_SSVS,n.iter=10000)

#Posterior parameters JAGS will save   
param_SSVS <- c("beta0", "beta", "g", "mdl")

#Number of iterations
nit_SSVS <- 100000

#Thinning
thin_SSVS <- 10

#The command coda.samples() calls jags from R passing the data and initial value just defined
output_SSVS_f <- coda.samples(model = model_SSVS,
                            variable.names = param_SSVS,
                            n.iter = nit_SSVS,
                            thin = thin_SSVS)

#save(output_SSVS_f,file='ssvs_f.dat')   #we save the chain
load('ssvs_f.dat')

#We give a look at the trace plot and at the density summary of the posterior chains for each of the parameters
x11()
plot(output_SSVS_f,ask=T)

summary(output_SSVS)

output_SSVS <- as.matrix(output_SSVS_f)
head(output_SSVS)


#MPM (Median Probability Model)

post_g_SSVS <- as.matrix(output_SSVS[,13:23])
post_mean_g_SSVS <- apply(post_g_SSVS,2,"mean") 

x11()
p2_SSVS <- data.frame(value = post_mean_g_SSVS, var = colnames(X)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position="none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("")
p2_SSVS

MPM_SSVS <- as.vector(which(post_mean_g_SSVS > 0.5))
post_mean_g_SSVS[MPM_SSVS]

#MPM yields a model with the following covariates:
#Garage.Area, Total.Bsmt.SF, X1st.Flr.SF, AgeofHouse, Mas.Vnr.Area, Wood.Deck.SF, Overall.Qual, Full.Bath, TotRms.AbvGrd, Fireplaces


#HPD (Highest Posterior Density)

#Plot of the visited models
x11()
plot(output_SSVS[,"mdl"], pch = 20)

#For example, at iteration 10, the chain explored the model: 
output_SSVS[10,"mdl"]

#Number of visited models (out of 2^(K+1)=2^12)
length(unique(output_SSVS[,"mdl"])) #45

#Posterior frequency of the visited models
visited_models_SSVS <- table(output_SSVS[,"mdl"])
visited_models_SSVS

#Getting the unique profiles and sort the results
unique_model_SSVS <- unique(post_g_SSVS, MARGIN  = 1)
freq_SSVS <- apply(unique_model_SSVS, 1, function(b) sum(apply(post_g_SSVS, MARGIN = 1, function(a) all(a == b))))
cbind(unique_model_SSVS[order(freq_SSVS,decreasing = T),], sort(freq_SSVS,decreasing = T))

#HPD model 
colnames(X)[as.logical(unique_model_SSVS[which.max(freq_SSVS),])]
HPD_SSVS <- c(1:11)[as.logical(unique_model_SSVS[which.max(freq_SSVS),])]

#HPD yields a model with the following covariates:
#Garage.Area, Total.Bsmt.SF, X1st.Flr.SF, AgeofHouse, Mas.Vnr.Area, Wood.Deck.SF, Overall.Qual, Full.Bath, TotRms.AbvGrd, Fireplaces
#Same as MPM


#HS (Hard Shrinkage)

beta_SSVS = as.matrix(output_SSVS[,1:11])

#Compute the 95% posterior credible interval for beta
CI_beta_SSVS = apply(beta_SSVS, 2, quantile, c(0.025, 0.975)) 
CI_beta_SSVS

#If the credibility interval does not contain 0 then I keep the variable
idx_cov_BL_SSVS = NULL
for(l in 1:p){
  if(CI_beta_SSVS[1,l]<0 && CI_beta_SSVS[2,l]>0)
  {
    cat("*** variable ", colnames(X)[l], " excluded \n")
  }
  else
  {
    cat("*** variable ", colnames(X)[l], " included \n")
    idx_cov_BL_SSVS = c(idx_cov_BL_SSVS, l)
  }
  
}

mean_beta_post_SSVS <- apply(beta_SSVS, 2, "mean")
mean_beta_post_SSVS

x11()
data.frame(x = as.vector(beta_SSVS), var = rep(colnames(X)[1:11], each = nrow(beta_SSVS))) %>%
  ggplot(aes(x = var, y = x, fill = var)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "null") + 
  geom_hline(aes(yintercept = 0), col = 2, lty = 2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + 
  ylab("")

#HS yields a model with the following covariates:
#Garage.Area, Total.Bsmt.SF, X1st.Flr.SF, AgeofHouse, Mas.Vnr.Area, Wood.Deck.SF, Overall.Qual, Full.Bath, TotRms.AbvGrd, Fireplaces
#Same as MPM and HPD


#We also tried with Spike & Slab approach:
#Some betas present weird posterior distributions, 
#this because the chain explore the support of those betas too slowly and without a good mixing!
#--> it's not good!
#SSVS approach leads to definitely better results!


#Conclusions:
#We have selected the following covariates:
#Garage.Area, Total.Bsmt.SF, X1st.Flr.SF, AgeofHouse, Mas.Vnr.Area, Wood.Deck.SF, Overall.Qual, Full.Bath, TotRms.AbvGrd, Fireplaces


###############################################################################################################################


#Actually, we moved on in this way:
#After finding the numerical variables most significant for the price prediction in the jupyter file "Preliminary Analysis",
#we found the categorical variables most significant.
#Discarding all the categorical variables didn't seem related to the price, and the ones seemed highly correlated to
#others we are going to consider already, we reconduced ourselves to the following categorical variables:
#Land.Contour, Utilities, Condition.1, Central.Air.

#We have studied these categoriacal variables:

house <- read.csv("house.csv", header=TRUE, sep=",", row.names=1)

x11()
boxplot(house$price ~ house$Utilities, col=rainbow(8),main='Effect of utilities on price')
a1 <- aov(house$price ~ house$Utilities)
summary(a1)
#p-value 0.215 --> effect "Utilities" doesn't significantly influence the price

x11()
boxplot(house$price ~ house$Land.Contour, col=rainbow(8),main='Effect of Land.Contour on price')
a2 <- aov(house$price ~ house$Land.Contour)
summary(a2)
#p-value 2e-16 --> effect "Land.Contour" significantly influence the price

x11()
boxplot(house$price ~ house$Central.Air, col=rainbow(8),main='Effect of Central.Air on price')
a3 <- aov(house$price ~ house$Central.Air)
summary(a3)
#p-value 2e-16 --> effect "Central.Air" significantly influence the price

x11()
boxplot(house$price ~ house$Condition.1, col=rainbow(8),main='Effect of Condition.1 on price')
a4 <- aov(house$price ~ house$Condition.1)
summary(a4)
#p-value 2e-16 --> effect "Condition.1" significantly influence the price


#We created dummy variables to take into consideration Land.Contour, Central.Air and Condition.1:

house12 <- read.csv("house12.csv", header=TRUE, sep=",", row.names=1)
n <- dim(house12)[1]

landcontour.name <- factor(house$Land.Contour, labels=c('Bnk','HLS', 'Low', 'Lvl'))
i1_lc <- which(landcontour.name=='Bnk')
i2_lc <- which(landcontour.name=='HLS')
i3_lc <- which(landcontour.name=='Low')
i4_lc <- which(landcontour.name=='Lvl')
dummy1_lc <- matrix(rep(0,n),nrow=n)
dummy2_lc <- matrix(rep(0,n),nrow=n)
dummy3_lc <- matrix(rep(0,n),nrow=n)

centralair.name <- factor(house$Central.Air, labels=c('N','Y'))
i1_ca <- which(centralair.name=='N')
i2_ca <- which(centralair.name=='Y')
dummy_ca <- matrix(rep(0,n),nrow=n)

condition1.name <- factor(house$Condition.1, labels=c('Artery','Feedr','Norm','RRNn','RRAn','PosN','PosA','RRNe','RRAe'))
i1_con <- which(condition1.name=='Artery')
i2_con <- which(condition1.name=='Feedr')
i3_con <- which(condition1.name=='Norm')
i4_con <- which(condition1.name=='RRNn')
i5_con <- which(condition1.name=='RRAn')
i6_con <- which(condition1.name=='PosN')
i7_con <- which(condition1.name=='PosA')
i8_con <- which(condition1.name=='RRNe')
i9_con <- which(condition1.name=='RRAe')
dummy1_con <- matrix(rep(0,n),nrow=n)
dummy2_con <- matrix(rep(0,n),nrow=n)
dummy3_con <- matrix(rep(0,n),nrow=n)
dummy4_con <- matrix(rep(0,n),nrow=n)
dummy5_con <- matrix(rep(0,n),nrow=n)
dummy6_con <- matrix(rep(0,n),nrow=n)
dummy7_con <- matrix(rep(0,n),nrow=n)
dummy8_con <- matrix(rep(0,n),nrow=n)

DATA <- cbind(house12,dummy1_lc,dummy2_lc,dummy3_lc,dummy_ca,dummy1_con,dummy2_con,dummy3_con,dummy4_con,dummy5_con,dummy6_con,dummy7_con,dummy8_con)

DATA[i2_lc,13] <- 1
DATA[i3_lc,14] <- 1
DATA[i4_lc,15] <- 1

DATA[i2_ca,16] <- 1

DATA[i2_con,17] <- 1
DATA[i3_con,18] <- 1
DATA[i4_con,19] <- 1
DATA[i5_con,20] <- 1
DATA[i6_con,21] <- 1
DATA[i7_con,22] <- 1
DATA[i8_con,23] <- 1
DATA[i9_con,24] <- 1


#We moved through Stochastic Search Variable Selection (SSVS) approach to do Bayesian Feature Selection among all the selected variables:

DATA_scale <- scale(DATA[,c(1,3,4,5,7,8,11,12)])
DATA_scale <- cbind(DATA_scale,DATA[,c(2,6,9,10,13,14,15,16,17,18,19,20,21,22,23,24)])
sub.idx = 2:24
num.data = dim(DATA_scale)[1]
X <- as.matrix(DATA_scale[1:num.data,sub.idx])
Y <- as.vector(DATA_scale[1:num.data,1])
N <- dim(X)[1]
p <- dim(X)[2]

c_ss <- 10
intersect <- 0.05
tau_ss <- intersect / sqrt(2 * log(c_ss) * c_ss^2/(c_ss^2 - 1))

x11()
curve(dnorm(x, 0, tau_ss), xlim = c(-5, 5))
curve(dnorm(x, 0, tau_ss * c_ss), xlim = c(-5, 5), add = T, col = 2)

data_JAGS_SSVS <- list(N = N, p = p, Y = Y, X = as.matrix(X), 
                       tau_ss = tau_ss, c_ss = c_ss)

inits_SSVS = function() {
  list(beta0 = 0.0, beta = rep(0,p), g = rep(0,p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model_SSVS = jags.model("SSVS.bug",
                        data = data_JAGS_SSVS,
                        n.adapt = 10000,
                        inits = inits_SSVS,
                        n.chains = 1) 

update(model_SSVS,n.iter=10000)

param_SSVS <- c("beta0", "beta", "g", "mdl")
nit_SSVS <- 100000
thin_SSVS <-10

output_SSVS <- coda.samples(model = model_SSVS,
                            variable.names = param_SSVS,
                            n.iter = nit_SSVS,
                            thin = thin_SSVS)

#save(output_SSVS,file='ssvs.dat')   
load('ssvs.dat')

x11()
plot(output_SSVS,ask=T)

output_SSVS <- as.matrix(output_SSVS)

#MPM (Median Probability Model)
post_g_SSVS <- as.matrix(output_SSVS[,25:47])
post_mean_g_SSVS <- apply(post_g_SSVS,2,"mean") 

x11()
p2_SSVS <- data.frame(value = post_mean_g_SSVS, var = colnames(X)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position="none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("")
p2_SSVS

MPM_SSVS <- as.vector(which(post_mean_g_SSVS > 0.5))
post_mean_g_SSVS[MPM_SSVS]

#MPM yields a model with the following covariates:
#Garage.Area, Total.Bsmt.SF, X1st.Flr.SF, AgeofHouse, Mas.Vnr.Area, Overall.Qual, Full.Bath,
#TotRms.AbvGrd, Fireplaces, dummy1_lc, dummy2_lc, dummy3_lc, dummy2_con, dummy3_con, dummy4_con

#So we carried on the work keeping also the dummy variables dummy1_lc, dummy2_lc, dummy3_lc, dummy2_con, dummy3_con and dummy4_con,
#but running our future models, the effect of these dummy variables seemed really irrelevant, so we decided to go back and restart our analysis 
#without the dummy variables derived from the categorical variables Land.Contour, Central.Air and Condition.1






