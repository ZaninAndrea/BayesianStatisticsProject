setwd("C:/Users/panze/Desktop/BAYESIAN STATISTICS/PROGETTO/Dataset")

#house12 <- read.csv("house12.csv", header=TRUE, sep=",", row.names=1)
#house <- read.csv("house.csv", header=TRUE, sep=",", row.names=1)
#house12$Neighborhood <- house$Neighborhood
#house12 <- house12[,-12]

#write.table(house12, "houseneighh.csv", 
#            sep = ";",             
#            row.names = FALSE,     
#            dec = ",",             
#            na = "",
#            quote = TRUE)

houseneigh <- read.csv("houseneighh.csv", header=TRUE, sep=";")


#PRICE
x11()
par(mfrow=c(1,3))
plot(houseneigh$price,main='occurencies in DATA') 
hist(houseneigh$price,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$price)

x11()
qqnorm(houseneigh$price)
qqline(houseneigh$price)

#try log10 transform
x11()
par(mfrow=c(1,3))
plot(log10(houseneigh$price),main='occurencies in DATA') 
hist(log10(houseneigh$price),main = 'distribution of occurencies',prob=T)
boxplot(log10(houseneigh$price))

x11()
qqnorm(log10(houseneigh$price))
qqline(log10(houseneigh$price))
#clearly better
#we decide to work on a log10 scale for price 


#GARAGEAREA
x11()
par(mfrow=c(1,3))
plot(houseneigh$Garage.Area,main='occurencies in DATA') 
hist(houseneigh$Garage.Area,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$Garage.Area)

x11()
qqnorm(houseneigh$Garage.Area)
qqline(houseneigh$Garage.Area)

#try sqrt transform (square feet --> feet) * 0.3 (feet --> meters)
x11()
par(mfrow=c(1,3))
plot(0.3*sqrt(houseneigh$Garage.Area),main='occurencies in DATA') 
hist(0.3*sqrt(houseneigh$Garage.Area),main = 'distribution of occurencies',prob=T)
boxplot(0.3*sqrt(houseneigh$Garage.Area))

x11()
qqnorm(0.3*sqrt(houseneigh$Garage.Area))
qqline(0.3*sqrt(houseneigh$Garage.Area))
#better
#we decide to work on a sqrt scale for Garage.Area * 0.3


#TOTALBSMTSF
x11()
par(mfrow=c(1,3))
plot(houseneigh$Total.Bsmt.SF,main='occurencies in DATA') 
hist(houseneigh$Total.Bsmt.SF,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$Total.Bsmt.SF)

x11()
qqnorm(houseneigh$Total.Bsmt.SF)
qqline(houseneigh$Total.Bsmt.SF)

#try sqrt transform (square feet --> feet) * 0.3 (feet --> meters)
x11()
par(mfrow=c(1,3))
plot(0.3*sqrt(houseneigh$Total.Bsmt.SF),main='occurencies in DATA') 
hist(0.3*sqrt(houseneigh$Total.Bsmt.SF),main = 'distribution of occurencies',prob=T)
boxplot(0.3*sqrt(houseneigh$Total.Bsmt.SF))

x11()
qqnorm(0.3*sqrt(houseneigh$Total.Bsmt.SF))
qqline(0.3*sqrt(houseneigh$Total.Bsmt.SF))
#better
#we decide to work on a sqrt scale for Total.Bsmt.SF* 0.3


#X1STFLRSF
x11()
par(mfrow=c(1,3))
plot(houseneigh$X1st.Flr.SF,main='occurencies in DATA') 
hist(houseneigh$X1st.Flr.SF,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$X1st.Flr.SF)

x11()
qqnorm(houseneigh$X1st.Flr.SF)
qqline(houseneigh$X1st.Flr.SF)

#try sqrt transform (square feet --> feet) * 0.3 (feet --> meters)
x11()
par(mfrow=c(1,3))
plot(0.3*sqrt(houseneigh$X1st.Flr.SF),main='occurencies in DATA') 
hist(0.3*sqrt(houseneigh$X1st.Flr.SF),main = 'distribution of occurencies',prob=T)
boxplot(0.3*sqrt(houseneigh$X1st.Flr.SF))

x11()
qqnorm(0.3*sqrt(houseneigh$X1st.Flr.SF))
qqline(0.3*sqrt(houseneigh$X1st.Flr.SF))
#clearly better
#we decide to work on a sqrt scale for X1st.Flr.SF * 0.3


#MASVNAREA
x11()
par(mfrow=c(1,3))
plot(houseneigh$Mas.Vnr.Area,main='occurencies in DATA') 
hist(houseneigh$Mas.Vnr.Area,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$Mas.Vnr.Area)

x11()
qqnorm(houseneigh$Mas.Vnr.Area)
qqline(houseneigh$Mas.Vnr.Area)

#try sqrt transform (square feet --> feet) * 0.3 (feet --> meters)
x11()
par(mfrow=c(1,3))
plot(0.3*sqrt(houseneigh$Mas.Vnr.Area),main='occurencies in DATA') 
hist(0.3*sqrt(houseneigh$Mas.Vnr.Area),main = 'distribution of occurencies',prob=T)
boxplot(0.3*sqrt(houseneigh$Mas.Vnr.Area))

x11()
qqnorm(0.3*sqrt(houseneigh$Mas.Vnr.Area))
qqline(0.3*sqrt(houseneigh$Mas.Vnr.Area))
#better
#we decide to work on a sqrt scale for Mas.Vnr.Area * 0.3


#WOODDECKSF
x11()
par(mfrow=c(1,3))
plot(houseneigh$Wood.Deck.SF,main='occurencies in DATA') 
hist(houseneigh$Wood.Deck.SF,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$Wood.Deck.SF)

x11()
qqnorm(houseneigh$Wood.Deck.SF)
qqline(houseneigh$Wood.Deck.SF)

#try sqrt transform (square feet --> feet) * 0.3 (feet --> meters)
x11()
par(mfrow=c(1,3))
plot(0.3*sqrt(houseneigh$Wood.Deck.SF),main='occurencies in DATA') 
hist(0.3*sqrt(houseneigh$Wood.Deck.SF),main = 'distribution of occurencies',prob=T)
boxplot(0.3*sqrt(houseneigh$Wood.Deck.SF))

x11()
qqnorm(0.3*sqrt(houseneigh$Wood.Deck.SF))
qqline(0.3*sqrt(houseneigh$Wood.Deck.SF))
#better
#we decide to work on a sqrt scale for Wood.Deck.SF * 0.3


#OVERALLQUAL
x11()
par(mfrow=c(1,3))
plot(houseneigh$Overall.Qual,main='occurencies in DATA') 
hist(houseneigh$Overall.Qual,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$Overall.Qual)


#FULLBATH
x11()
par(mfrow=c(1,3))
plot(houseneigh$Full.Bath,main='occurencies in DATA') 
hist(houseneigh$Full.Bath,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$Full.Bath)


#AGEOFHOUSE
x11()
par(mfrow=c(1,3))
plot(houseneigh$AgeofHouse,main='occurencies in DATA') 
hist(houseneigh$AgeofHouse,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$AgeofHouse)

#try ceiling(AgeofHouse/5)
x11()
par(mfrow=c(1,3))
plot(ceiling((houseneigh$AgeofHouse)/5),main='occurencies in DATA') 
hist(ceiling((houseneigh$AgeofHouse)/5),main = 'distribution of occurencies',prob=T)
boxplot(ceiling((houseneigh$AgeofHouse)/5))
#better
#we decide to work on ceiling(AgeofHouse/5)


#TOTRMSABVGRD
x11()
par(mfrow=c(1,3))
plot(houseneigh$TotRms.AbvGrd,main='occurencies in DATA') 
hist(houseneigh$TotRms.AbvGrd,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$TotRms.AbvGrd)


#FIREPLACES
x11()
par(mfrow=c(1,3))
plot(houseneigh$Fireplaces,main='occurencies in DATA') 
hist(houseneigh$Fireplaces,main = 'distribution of occurencies',prob=T)
boxplot(houseneigh$Fireplaces)


#NEIGHBORHOOD
x11()
boxplot(log10(houseneigh$price) ~ houseneigh$Neighborhood, col=rainbow(8),main='Effect of Neighborhood on log10price', xlab="Neighborhood", ylab="log10price")


#Summarizing:
#We consider log10(price) as target variable.
#We consider Overall.Qual, sqrt(Garage.Area)*0.3, sqrt(Total.Bsmt.SF)*0.3, sqrt(X1st.Flr.SF)*0.3, Full.Bath, ceiling(AgeofHouse/5), sqrt(Mas.Vnr.Area)*0.3, TotRms.AbvGrd, Fireplaces, sqrt(Wood.Deck.SF)*0.3 as covariates. 
#We consider Neighborhood as spatial random effect


######################################################################################################################################################################################################################################

#We create a train set (80% of the dataset) and a test set (20% of the dataset)

sample <- sample(c(TRUE, FALSE), nrow(houseneigh), replace=TRUE, prob=c(0.8,0.2))
train <- houseneigh[sample, ]
train <- arrange(train,train$Neighborhood)
test <- houseneigh[!sample, ]
test <- arrange(test,test$Neighborhood)

#write.table(train, "train.csv", 
#            sep = ";",             
#            row.names = FALSE,     
#            dec = ".",             
#            na = "",
#            quote = TRUE)

#write.table(test, "test.csv", 
#            sep = ";",             
#            row.names = FALSE,     
#            dec = ".",             
#            na = "",
#            quote = TRUE)


##############################################################################################################

train <- read.csv("train.csv", header=TRUE, sep=";")

train$price <- round(log10(train$price),3)
train$Garage.Area <- round(sqrt(train$Garage.Area)*0.3,3)
train$Total.Bsmt.SF <- round(sqrt(train$Total.Bsmt.SF)*0.3,3)
train$X1st.Flr.SF <- round(sqrt(train$X1st.Flr.SF)*0.3,3)
train$AgeofHouse <- ceiling((train$AgeofHouse)/5)
train$Mas.Vnr.Area <- round(sqrt(train$Mas.Vnr.Area)*0.3,3)
train$Wood.Deck.SF <- round(sqrt(train$Wood.Deck.SF)*0.3,3)

#write.table(train, "traintransf.csv", 
#            sep = ";",             
#            row.names = FALSE,     
#            dec = ".",             
#            na = "",
#            quote = TRUE)


test <- read.csv("test.csv", header=TRUE, sep=";")

test$price <- round(log10(test$price),3)
test$Garage.Area <- round(sqrt(test$Garage.Area)*0.3,3)
test$Total.Bsmt.SF <- round(sqrt(test$Total.Bsmt.SF)*0.3,3)
test$X1st.Flr.SF <- round(sqrt(test$X1st.Flr.SF)*0.3,3)
test$AgeofHouse <- ceiling((test$AgeofHouse)/5)
test$Mas.Vnr.Area <- round(sqrt(test$Mas.Vnr.Area)*0.3,3)
test$Wood.Deck.SF <- round(sqrt(test$Wood.Deck.SF)*0.3,3)

#write.table(test, "testtransf.csv", 
#            sep = ";",             
#            row.names = FALSE,     
#            dec = ".",             
#            na = "",
#            quote = TRUE)


