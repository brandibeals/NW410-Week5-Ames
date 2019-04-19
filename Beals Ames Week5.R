# Set working directory and read CSV file
setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/410-DL-57/Data")
mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")

# Calculate additional variables
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$TotalSqftCalc <- mydata$BsmtFinSF1 + mydata$BsmtFinSF2 + mydata$GrLivArea
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF

# Select and build "typical" sample
sample <- subset(mydata, Zoning=="RL"|Zoning=="RM"|Zoning=="FV"|Zoning=="RH") #2901 observations
sample <- subset(sample, SaleCondition=="Normal") #2397 observations
sample <- subset(sample, Utilities=="AllPub") #2396 observations
sample <- subset(sample, Street=="Pave") #2391 observations
sample <- subset(sample, TotalFloorSF<4000) #2390 observations


# Subset numeric data
quantdatafull <- subset(mydata, select=c("SalePrice","logSalePrice","SubClass","LotFrontage","LotArea",
                                     "OverallQual","OverallCond","YearBuilt","YearRemodel","MasVnrArea",
                                     "BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","FirstFlrSF","SecondFlrSF",
                                     "LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath",
                                     "BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt",
                                     "GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","ThreeSsnPorch",
                                     "ScreenPorch","PoolArea","MiscVal","MoSold","YrSold","TotalFloorSF","HouseAge","QualityIndex"))

# Limit to relevant variables
quantdata <- subset(mydata, select=c("SalePrice","logSalePrice","SubClass","LotArea","OverallQual","OverallCond",
                                          "YearBuilt","YearRemodel","FirstFlrSF","SecondFlrSF","LowQualFinSF",
                                          "GrLivArea","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd",
                                          "Fireplaces","WoodDeckSF","OpenPorchSF","EnclosedPorch","ThreeSsnPorch",
                                          "ScreenPorch","PoolArea","MiscVal","MoSold","YrSold","TotalFloorSF","HouseAge","QualityIndex"))
corrplot(cor(quantdatafull), method="shade", shade.col=NA, type="upper", tl.col="black", tl.cex=0.8)

# Limit again to highly correlated variables
quantdatasmall <- subset(mydata, select=c("SalePrice","logSalePrice","OverallQual","YearBuilt","YearRemodel",
                                          "FirstFlrSF","GrLivArea","FullBath","TotRmsAbvGrd","Fireplaces",
                                          "TotalFloorSF","HouseAge","QualityIndex"))
corrplot(cor(quantdatasmall), method="shade", shade.col=NA, type="upper", tl.col="black", tl.cex=0.8, addCoef.col="black")

quantdatasmallsub <- subset(sample, select=c("SalePrice","logSalePrice","OverallQual","YearBuilt","YearRemodel",
                                          "FirstFlrSF","GrLivArea","FullBath","TotRmsAbvGrd","Fireplaces",
                                          "TotalFloorSF","HouseAge","QualityIndex"))
corrplot(cor(quantdatasmallsub), method="shade", shade.col=NA, type="upper", tl.col="black", tl.cex=0.8, addCoef.col="black")

####################################################################
##################  Assignment 2  ##################################
####################################################################

library(scatterplot3d)

# Group 1
ggplot(sample, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###
ggplot(sample, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###

attach(sample)
s3d1 <-scatterplot3d(OverallQual,TotalFloorSF,SalePrice,pch=16,highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit1 <- lm(SalePrice ~ OverallQual + TotalFloorSF) 
s3d1$plane3d(fit1)

# Group 2
ggplot(sample, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###
ggplot(sample, aes(x=Fireplaces, y=SalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Fireplaces") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###

attach(sample)
s3d2 <-scatterplot3d(OverallQual,Fireplaces,SalePrice,pch=16,highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit2 <- lm(SalePrice ~ OverallQual + Fireplaces) 
s3d2$plane3d(fit2)

# Group 3
ggplot(sample, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="green", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###
ggplot(sample, aes(x=YearBuilt, y=SalePrice)) + 
  geom_point(color="green", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Year Built") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###

attach(sample)
s3d3 <-scatterplot3d(TotalFloorSF,YearBuilt,SalePrice,pch=16,highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit3 <- lm(SalePrice ~ TotalFloorSF + YearBuilt) 
s3d3$plane3d(fit3)

# 3D Scatterplot with Coloring and Vertical Lines and Regression Plane 

library(Rcmdr)
attach(sample)
scatter3d(SalePrice,OverallQual,TotalFloorSF)

############## fitting a SLR ###################################

# OverallQual
ggplot(sample, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

SLRresult1 = lm(SalePrice ~ OverallQual, data=sample)
anova(SLRresult1)
summary(SLRresult1)
par(mfrow=c(2,2))
plot(SLRresult2)
par(mfrow=c(1,1))

pred1 <- as.data.frame(predict(SLRresult1,sample,interval="prediction"))
head(pred1)

# TotalFloorSF
ggplot(sample, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, color="red")  ## method=lm, se=FALSE ###

SLRresult2 = lm(SalePrice ~ TotalFloorSF, data=sample)
anova(SLRresult2)
summary(SLRresult2)
par(mfrow=c(2,2))
plot(SLRresult2)
par(mfrow=c(1,1))

pred2 <- as.data.frame(predict(SLRresult2,sample,interval="prediction"))
head(pred2)

# YearBuilt
ggplot(sample, aes(x=YearBuilt, y=SalePrice)) + 
  geom_point(color="darkgreen", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Year Built") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, color="darkgreen")  ## method=lm, se=FALSE ###

SLRresult3 = lm(SalePrice ~ YearBuilt, data=sample)
anova(SLRresult3)
summary(SLRresult3)
par(mfrow=c(2,2))
plot(SLRresult3)
par(mfrow=c(1,1))

pred3 <- as.data.frame(predict(SLRresult3,sample,interval="prediction"))
head(pred3)

############## fitting a MLR ###################################

# Model 1
MLRresult1 = lm(SalePrice ~ OverallQual+TotalFloorSF, data=sample)
anova(MLRresult1)
summary(MLRresult1)
par(mfrow=c(2,2))
plot(MLRresult1)
par(mfrow=c(1,1))

# Model 2
MLRresult2 = lm(SalePrice ~ TotalFloorSF+YearBuilt, data=sample)
anova(MLRresult2)
summary(MLRresult2)
par(mfrow=c(2,2))
plot(MLRresult2)
par(mfrow=c(1,1))

# Predictions using Model 1
pred4 <- as.data.frame(predict(MLRresult1,sample,interval="prediction"))
head(pred4)

samplepred <- cbind(sample,pred4)
samplepred <- subset(samplepred, select = -lwr)
samplepred <- subset(samplepred, select = -upr)
samplepred$res <- samplepred$SalePrice - samplepred$fit
head(samplepred)

# Recreate models using logSalePrice

# Overall Quality
ggplot(sample, aes(x=OverallQual, y=logSalePrice)) + 
  geom_point(color="purple", shape=1) +
  ggtitle("Scatter Plot of Log Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE, color="purple")  ## method=lm, se=FALSE ###

SLRresult4 = lm(logSalePrice ~ OverallQual, data=sample)
anova(SLRresult4)
summary(SLRresult4)
par(mfrow=c(2,2))
plot(SLRresult4)
par(mfrow=c(1,1))

# Total Floor SF
ggplot(sample, aes(x=TotalFloorSF, y=logSalePrice)) + 
  geom_point(color="orange", shape=1) +
  ggtitle("Scatter Plot of Log Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, color="orange")  ## method=lm, se=FALSE ###

SLRresult5 = lm(logSalePrice ~ TotalFloorSF, data=sample)
anova(SLRresult5)
summary(SLRresult5)
par(mfrow=c(2,2))
plot(SLRresult5)
par(mfrow=c(1,1))

# Overall Quality + Total Floor SF
attach(sample)
s3d4 <-scatterplot3d(OverallQual,TotalFloorSF,logSalePrice,pch=16,highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit4 <- lm(logSalePrice ~ OverallQual + TotalFloorSF) 
s3d4$plane3d(fit4)

MLRresult3 = lm(logSalePrice ~ OverallQual+TotalFloorSF, data=sample)
anova(MLRresult3)
summary(MLRresult3)
par(mfrow=c(2,2))
plot(MLRresult3)
par(mfrow=c(1,1))

# Compare to full data set
attach(mydata)
s3d5 <-scatterplot3d(OverallQual,TotalFloorSF,logSalePrice,pch=16,highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit5 <- lm(logSalePrice ~ OverallQual + TotalFloorSF) 
s3d4$plane3d(fit5)

MLRresult4 = lm(logSalePrice ~ OverallQual+TotalFloorSF, data=mydata)
anova(MLRresult4)
summary(MLRresult4)
par(mfrow=c(2,2))
plot(MLRresult4)
par(mfrow=c(1,1))

###################################################################
##################### Assignment 3  ###############################
###################################################################

# Categorical Variable Model
library(plyr)
NeighborhoodMean <- ddply(mydata, .(Neighborhood), summarise, MeanPrice = mean(SalePrice))
NeighborhoodLM <- lm(SalePrice ~ Neighborhood, data=mydata)
anova(NeighborhoodLM)
summary(NeighborhoodLM)

par(mfrow=c(2,2))
plot(NeighborhoodLM)
par(mfrow=c(1,1))

NeighborhoodPred <- predict(NeighborhoodLM,mydata)
mydata2 <- cbind(mydata,NeighborhoodPred)
ddply(mydata2, .(Neighborhood), summarise, MeanPred = mean(NeighborhoodPred))

par(mfrow=c(1,2))
plot(mydata2$Neighborhood, NeighborhoodPred, ylim=c(0,800000), main="Average Predicted SalePrice")
plot(mydata$Neighborhood, mydata$SalePrice, ylim=c(0,800000), main="Boxplot of Average SalePrice")
par(mfrow=c(1,1))

# Dummy Variables
mydata2$NbhdGrp1 <- ifelse(mydata2$Neighborhood == "Blmngtn", 1, 0)
mydata2$NbhdGrp2 <- ifelse(mydata2$Neighborhood == "Blueste", 1, 0)
mydata2$NbhdGrp3 <- ifelse(mydata2$Neighborhood == "BrDale", 1, 0)
mydata2$NbhdGrp4 <- ifelse(mydata2$Neighborhood == "BrkSide", 1, 0)
mydata2$NbhdGrp5 <- ifelse(mydata2$Neighborhood == "ClearCr", 1, 0)
mydata2$NbhdGrp6 <- ifelse(mydata2$Neighborhood == "CollgCr", 1, 0)
mydata2$NbhdGrp7 <- ifelse(mydata2$Neighborhood == "Crawfor", 1, 0)
mydata2$NbhdGrp8 <- ifelse(mydata2$Neighborhood == "Edwards", 1, 0)
mydata2$NbhdGrp9 <- ifelse(mydata2$Neighborhood == "Gilbert", 1, 0)
mydata2$NbhdGrp10 <- ifelse(mydata2$Neighborhood == "Greens", 1, 0)
mydata2$NbhdGrp11 <- ifelse(mydata2$Neighborhood == "GrnHill", 1, 0)
mydata2$NbhdGrp12 <- ifelse(mydata2$Neighborhood == "IDOTRR", 1, 0)
mydata2$NbhdGrp13 <- ifelse(mydata2$Neighborhood == "Landmrk", 1, 0)
mydata2$NbhdGrp14 <- ifelse(mydata2$Neighborhood == "MeadowV", 1, 0)
mydata2$NbhdGrp15 <- ifelse(mydata2$Neighborhood == "Mitchel", 1, 0)
mydata2$NbhdGrp16 <- ifelse(mydata2$Neighborhood == "NAmes", 1, 0)
mydata2$NbhdGrp17 <- ifelse(mydata2$Neighborhood == "NoRidge", 1, 0)
mydata2$NbhdGrp18 <- ifelse(mydata2$Neighborhood == "NPkVill", 1, 0)
mydata2$NbhdGrp19 <- ifelse(mydata2$Neighborhood == "NridgHt", 1, 0)
mydata2$NbhdGrp20 <- ifelse(mydata2$Neighborhood == "NWAmes", 1, 0)
mydata2$NbhdGrp21 <- ifelse(mydata2$Neighborhood == "OldTown", 1, 0)
mydata2$NbhdGrp22 <- ifelse(mydata2$Neighborhood == "Sawyer", 1, 0)
mydata2$NbhdGrp23 <- ifelse(mydata2$Neighborhood == "SawyerW", 1, 0)
mydata2$NbhdGrp24 <- ifelse(mydata2$Neighborhood == "Somerst", 1, 0)
mydata2$NbhdGrp25 <- ifelse(mydata2$Neighborhood == "StoneBr", 1, 0)
mydata2$NbhdGrp26 <- ifelse(mydata2$Neighborhood == "SWISU", 1, 0)
mydata2$NbhdGrp27 <- ifelse(mydata2$Neighborhood == "Timber", 1, 0)

DumNghbrhdMLR = lm(SalePrice ~ NbhdGrp1+NbhdGrp2+NbhdGrp3+NbhdGrp4+NbhdGrp5+NbhdGrp6+NbhdGrp7+NbhdGrp8+NbhdGrp9+
                     NbhdGrp10+NbhdGrp11+NbhdGrp12+NbhdGrp13+NbhdGrp14+NbhdGrp15+NbhdGrp16+NbhdGrp17+NbhdGrp18+
                     NbhdGrp19+NbhdGrp20+NbhdGrp21+NbhdGrp22+NbhdGrp23+NbhdGrp24+NbhdGrp25+NbhdGrp26+NbhdGrp27, data=mydata2)
anova(DumNghbrhdMLR)
summary(DumNghbrhdMLR)

NeighborhoodPred2 <- predict(DumNghbrhdMLR,mydata2)
mydata3 <- cbind(mydata2,NeighborhoodPred2)
ddply(mydata3, .(Neighborhood), summarise, MeanPred = mean(DumNghbrhdMLR))

par(mfrow=c(2,2))
plot(DumNghbrhdMLR)
par(mfrow=c(1,1))

# Multiple Linear Regression
MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual+HouseAge+GarageArea, data=mydata)
anova(MLRresult)
summary(MLRresult)

par(mfrow=c(2,2))
plot(MLRresult)
par(mfrow=c(1,1))

# Analysis of Neighborhood
pred <- as.data.frame(predict(MLRresult,mydata))

library(reshape)
pred <- rename(pred, c("predict(MLRresult, mydata)" = "prd"))
mydata$pred <- pred$prd
mydata$res <- mydata$SalePrice - mydata$pred
mydata$absres <- abs(mydata$res)
MAE <- mean(mydata$absres)
MAE

require(ggplot2)
ggplot(mydata, aes(x=Neighborhood, y=res)) + 
  geom_boxplot() +
  #geom_point(color="black", size=2) +
  ggtitle("Scatter Plot of Residual vs Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(method=lm,se=FALSE) +
  geom_hline(yintercept=0, color="red")

library(plyr)
subdat1 <- ddply(mydata, .(Neighborhood), summarise, MAE = mean(absres))
subdat2 <- ddply(mydata, .(Neighborhood), summarise, MeanPrice = mean(SalePrice))
subdat3 <- ddply(mydata, .(Neighborhood), summarise, TotalPrice = sum(SalePrice))
subdat4 <- ddply(mydata, .(Neighborhood), summarise, TotalSqft = sum(TotalFloorSF))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MAE)) + 
  geom_point(color="black", shape=1, size=3) +
  ggtitle("Scatter Plot of Mean of Absolute Errors vs Average Price per Square Foot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Neighborhood MLR
mydata$GarageArea[is.na(mydata$GarageArea)] <- 0

mydata$NbhdGrp <-
  ifelse(mydata$price_sqft<=100, "grp1", 
         ifelse(mydata$price_sqft<=125, "grp2",
                ifelse(mydata$price_sqft<=150, "grp3",
                       "grp4"))) 

mydata$NbhdGrp1 <- ifelse(mydata$NbhdGrp == "grp1", 1, 0)
mydata$NbhdGrp2 <- ifelse(mydata$NbhdGrp == "grp2", 1, 0)
mydata$NbhdGrp3 <- ifelse(mydata$NbhdGrp == "grp3", 1, 0)

MLRresult2 = lm(SalePrice ~ TotalFloorSF+OverallQual+HouseAge+GarageArea+
                  NbhdGrp1+NbhdGrp2+NbhdGrp3, data=mydata)
anova(MLRresult2)
summary(MLRresult2)

par(mfrow=c(2,2))
plot(MLRresult2)
par(mfrow=c(1,1))

pred2 <- as.data.frame(predict(MLRresult2,mydata))

library(reshape)
pred2 <- rename(pred2, c("predict(MLRresult2, mydata)" = "prd2"))
mydata$pred2 <- pred2$prd2
mydata$res2 <- mydata$SalePrice - mydata$pred2
mydata$absres2 <- abs(mydata$res2)
MAE2 <- mean(mydata$absres2)
MAE2

################################################################
############### Log Transformation #############################
################################################################

# Model 3
MLRresult3 = lm(SalePrice ~ TotalFloorSF+OverallQual+HouseAge+GarageArea+
                  NbhdGrp1+NbhdGrp2+NbhdGrp3, data=mydata)
anova(MLRresult3)
summary(MLRresult3)

par(mfrow=c(2,2))
plot(MLRresult3)
par(mfrow=c(1,1))

pred3 <- as.data.frame(predict(MLRresult3,mydata))

library(reshape)
pred3 <- rename(pred3, c("predict(MLRresult3, mydata)" = "prd3"))
mydata$pred3 <- pred3$prd3
mydata$res3 <- mydata$SalePrice - mydata$pred3
mydata$absres3 <- abs(mydata$res3)
MAE3 <- mean(mydata$absres3)
MAE3

# Model 4
MLRresult4 = lm(logSalePrice ~ TotalFloorSF+OverallQual+HouseAge+GarageArea+
                  NbhdGrp1+NbhdGrp2+NbhdGrp3, data=mydata)
anova(MLRresult4)
summary(MLRresult4)

par(mfrow=c(2,2))
plot(MLRresult4)
par(mfrow=c(1,1))

pred4 <- as.data.frame(predict(MLRresult4,mydata))

library(reshape)
pred4 <- rename(pred4, c("predict(MLRresult4, mydata)" = "prd4"))
mydata$pred4 <- pred4$prd4
mydata$res4 <- mydata$logSalePrice - mydata$pred4
mydata$absres4 <- abs(mydata$res4)
MAE4 <- mean(mydata$absres4)
MAE4

# Examine influential points

library(car)
vif(MLRresult4)
par(mfrow=c(1,1))
influencePlot(MLRresult4,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

summary(inflm.MLRLog <- influence.measures(MLRresult4))
dffitslog <- dffits(MLRresult4)
subdatnum <- cbind(mydata,dffitslog)
str(subdatnum)

ggplot(mydata, aes(x=OverallQual, y=res4)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################ influential points removed  #######
subdatnum$absdf <- abs(subdatnum$dffitslog)
head(subdatnum)
subdatnuminf <- subdatnum[which(subdatnum$absdf < 0.10465),]

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual+HouseAge+GarageArea+
                    NbhdGrp1+NbhdGrp2+NbhdGrp3, data=subdatnuminf)
anova(MLRLogresult)
summary(MLRLogresult)

par(mfrow=c(2,2))
plot(MLRLogresult)
par(mfrow=c(1,1))

############## analyze Neighborhood variable #########

require(ggplot2)
ggplot(subdat, aes(x=Neighborhood, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

library(plyr)
subdat1 <- ddply(mydata, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(subdat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalSqft = sum(TotalFloorSF))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#### Clean up of the Neighborhood varaible  ########

subdat$NbhdGrp <-
  ifelse(subdat$price_sqft<=100, "grp1", 
         ifelse(subdat$price_sqft<=120, "grp2",
                ifelse(subdat$price_sqft<=140, "grp3",
                       "grp4"))) 

################ include categorical variable in the model #######

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp, data=subdat)
anova(MLRresult)
summary(MLRresult)
pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

################# define dummy variables ###################

subdat$NbhdGrp1 <- 
  ifelse(subdat$NbhdGrp == "grp1", 1, 0)
subdat$NbhdGrp2 <- 
  ifelse(subdat$NbhdGrp == "grp2", 1, 0)
subdat$NbhdGrp3 <- 
    ifelse(subdat$NbhdGrp == "grp3", 1, 0)

MLRresult4 = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp1+NbhdGrp2+NbhdGrp3, 
                data=subdat)
anova(MLRresult4)
summary(MLRresult4)



############################################################
############## Assignment 5 ################################
############################################################

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
my.data <- sample
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);
names(train.df)

# Check your data split. The sum of the parts should equal the whole.
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

# Create slimmed down data set
drop.list <- c('SID','PID','SubClass','Zoning','LotFrontage','Street','Alley','LandContour','Utilities','LandSlope',
               'Condition1','Condition2','OverallQual','OverallCond','YearBuilt','YearRemodel','RoofStyle','RoofMat',
               'Exterior1','Exterior2','MasVnrType','MasVnrArea','ExterQual','ExterCond','BsmtQual','BsmtCond',
               'BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF',
               'Heating','CentralAir','Electrical','FirstFlrSF','SecondFlrSF','LowQualFinSF','GrLivArea',
               'BsmtFullBath','BsmtHalfBath','KitchenAbvGr','KitchenQual','TotRmsAbvGrd','Functional','FireplaceQu',
               'GarageType','GarageYrBlt','GarageFinish','GarageCars','GarageQual','GarageCond','PavedDrive',
               'WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch','ScreenPorch','PoolQC','Fence','MiscFeature',
               'MiscVal','MoSold','YrSold','SaleType','SaleCondition','logSalePrice','price_sqft','u','Neighborhood'
               )

train.clean <- train.df[,!(names(my.data) %in% drop.list)]
test.clean <- test.df[,!(names(my.data) %in% drop.list)]

# train.clean <- na.omit(train.clean)
# test.clean <- na.omit(test.clean)

# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean);
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalFloorSF,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=formula(lower.lm)),direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=formula(lower.lm)),direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(SalePrice ~ LotFrontage + OverallQual + OverallCond + YearBuilt + GrLivArea + KitchenAbvGr + 
                TotRmsAbvGrd + GarageType + LowQualFinSF, data=train.df)
summary(junk.lm)

library(car)
viffwd <- vif(forward.lm)
vifbkwd <- vif(backward.lm)
vifsw <- vif(stepwise.lm)
vifjk <- vif(junk.lm)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

# AIC
extractAIC(forward.lm)
extractAIC(backward.lm)
extractAIC(stepwise.lm)
extractAIC(junk.lm)

# BIC
BIC(forward.lm)
BIC(backward.lm)
BIC(stepwise.lm)
BIC(junk.lm)

# MSE
mean(forward.lm$residuals^2)
mean(backward.lm$residuals^2)
mean(stepwise.lm$residuals^2)
mean(junk.lm$residuals^2)

# MAE
mean(abs(forward.lm$residuals))
mean(abs(backward.lm$residuals))
mean(abs(stepwise.lm$residuals))
mean(abs(junk.lm$residuals))

# Predictions using the test data set
forward.test <- predict(forward.lm,newdata=test.clean);
backward.test <- predict(backward.lm,newdata=test.clean);
stepwise.test <- predict(stepwise.lm,newdata=test.clean);
junk.test <- predict(junk.lm,newdata=test.df);

# MSE Test
mean((test.clean$SalePrice-forward.test)^2)
mean((test.clean$SalePrice-backward.test)^2)
mean((test.clean$SalePrice-stepwise.test)^2)
mean(na.omit(test.df$SalePrice-junk.test)^2)

# MAE Test
mean(abs(test.clean$SalePrice-forward.test))
mean(abs(test.clean$SalePrice-backward.test))
mean(abs(test.clean$SalePrice-stepwise.test))
mean(na.omit(abs(test.clean$SalePrice-junk.test)))



# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(stepwise.pct)
MAPE

junk.pred <- predict(junk.lm,newdata=train.df);
junk.pct <- na.omit(abs(train.df$SalePrice-junk.pred)/train.df$SalePrice);
MAPE <- mean(junk.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- na.omit(abs(test.df$SalePrice-junk.test)/test.df$SalePrice);
MAPE <- mean(junk.testPCT)
MAPE


# Assign Prediction Grades training data;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)

# Assign Prediction Grades test data;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)

# Assign Prediction Grades training data;
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

backward.trainTable <- table(backward.PredictionGrade)
backward.trainTable/sum(backward.trainTable)

# Assign Prediction Grades test data;
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )					
)

backward.testTable <-table(backward.testPredictionGrade)
backward.testTable/sum(backward.testTable)

# Assign Prediction Grades training data;
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

stepwise.trainTable <- table(stepwise.PredictionGrade)
stepwise.trainTable/sum(stepwise.trainTable)

# Assign Prediction Grades test data;
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )					
)

stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)

# Assign Prediction Grades training data;
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]',
                               ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                             'Grade 4: (0.25+]')
                               )					
)

junk.trainTable <- table(junk.PredictionGrade)
junk.trainTable/sum(junk.trainTable)

# Assign Prediction Grades test data;
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)

######################################################################

anova(forward.lm)
summary(forward.lm)
par(mfrow=c(2,2))
plot(forward.lm)
par(mfrow=c(1,1))

summary(influence.measures(forward.lm))
dffits <- dffits(forward.lm)
subdatnum <- cbind(train.clean,dffits)
str(subdatnum)

subdatnum$absdf <- abs(subdatnum$dffits)
head(subdatnum)
subdatnuminf <- subdatnum[which(subdatnum$absdf < (2*sqrt((15+1)/(1682-15-1)))),] # 2*sqrt((p+1)/(n-p-1))

# Dummy Variables

# Foundation
train.clean$FoundGrp <-
  ifelse(train.clean$Foundation=="CBlock", "grp1", 
         ifelse(train.clean$Foundation=="PConc", "grp2",
              "grp3"))

train.clean$FoundGrp1 <- ifelse(train.clean$FoundGrp == "grp1", 1, 0)
train.clean$FoundGrp2 <- ifelse(train.clean$FoundGrp == "grp2", 1, 0)

# HouseStyle
train.clean$HsStylGrp <-
  ifelse(train.clean$HouseStyle=="1Story", "grp1", 
         ifelse(train.clean$HouseStyle=="2Story", "grp2",
                "grp3"))

train.clean$HsStylGrp1 <- ifelse(train.clean$HsStylGrp == "grp1", 1, 0)
train.clean$HsStylGrp2 <- ifelse(train.clean$HsStylGrp == "grp2", 1, 0)

# BldgType
train.clean$BldgTypeGrp <- ifelse(train.clean$BldgType=="1Fam", 1,0) 

# HeatingQC
train.clean$HeatQCGrp <-
  ifelse(train.clean$HeatingQC=="Ex", "grp1", 
         ifelse(train.clean$HeatingQC=="Gd", "grp2",
                ifelse(train.clean$HeatingQC=="TA", "grp2",        
                "grp3")))

train.clean$HeatQCGrp1 <- ifelse(train.clean$HeatQCGrp == "grp1", 1, 0)
train.clean$HeatQCGrp2 <- ifelse(train.clean$HeatQCGrp == "grp2", 1, 0)

# LotShape
train.clean$LotShapeGrp <- ifelse(train.clean$LotShape=="Reg", 1,0)

# LotConfig
train.clean$LotConGrp <-
  ifelse(train.clean$LotConfig=="Inside", "grp1", 
         ifelse(train.clean$LotConfig=="CulDSac", "grp2",
                       "grp3"))

train.clean$LotConGrp1 <- ifelse(train.clean$LotConGrp == "grp1", 1, 0)
train.clean$LotConGrp2 <- ifelse(train.clean$LotConGrp == "grp2", 1, 0)

# Create data sets
finalmodel <- lm(SalePrice ~ TotalSqftCalc + QualityIndex + HouseAge + GarageArea + TotalFloorSF + 
                   BedroomAbvGr + LotArea + Fireplaces + FullBath + FoundGrp1 + FoundGrp2 + HsStylGrp1 + HsStylGrp2 + 
                   BldgTypeGrp + HeatQCGrp1 + HeatQCGrp2 + LotShapeGrp + LotConGrp1 + LotConGrp2, data = train.clean)
summary(finalmodel)

par(mfrow=c(2,2))
plot(finalmodel)
par(mfrow=c(1,1))

# Outliers Removed
dffits <- dffits(finalmodel)
subdatnum <- cbind(train.clean,dffits)
subdatnum$absdf <- abs(subdatnum$dffits)
subdatnuminf <- subdatnum[which(subdatnum$absdf < (2*sqrt((15+1)/(1682-15-1)))),] # 2*sqrt((p+1)/(n-p-1))

finalmodelout <- lm(SalePrice ~ TotalSqftCalc + QualityIndex + HouseAge + GarageArea + TotalFloorSF + 
                   BedroomAbvGr + LotArea + Fireplaces + FullBath + FoundGrp1 + FoundGrp2 + HsStylGrp1 + HsStylGrp2 + 
                   BldgTypeGrp + HeatQCGrp1 + HeatQCGrp2 + LotShapeGrp + LotConGrp1 + LotConGrp2, data = subdatnuminf)
summary(finalmodelout)

par(mfrow=c(2,2))
plot(finalmodelout)
par(mfrow=c(1,1))


# Log Sale Price model
finalmodellog <- lm(logSalePrice ~ TotalSqftCalc + QualityIndex + HouseAge + GarageArea + TotalFloorSF + 
                      BedroomAbvGr + LotArea + Fireplaces + FullBath + FoundGrp1 + FoundGrp2 + HsStylGrp1 + HsStylGrp2 + 
                      BldgTypeGrp + HeatQCGrp1 + HeatQCGrp2 + LotShapeGrp + LotConGrp1 + LotConGrp2, data = train.clean)

summary(finalmodellog)

par(mfrow=c(2,2))
plot(finalmodellog)
par(mfrow=c(1,1))







sub <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                                         "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                         "TotalBsmtSF","Style1","Style2"))

MLRresult1 = lm(logSalePrice ~ ., data=sub)

sub2 <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                               "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                               "TotalBsmtSF"))

MLRresult2 = lm(logSalePrice ~ ., data=sub2)
anova(MLRresult1,MLRresult2)

anova(MLRresult1)
summary(MLRresult1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

names(MLRresult)
head(MLRresult$df.residual)

inflm.MLRLog <- influence.measures(MLRresult)
names(inflm.MLRLog)
str(inflm.MLRLog)
inflmetrics <- as.data.frame(inflm.MLRLog$infmat)
dffit_df <- subset(inflmetrics, select= c(dffit))
sub$r1 <- row.names(sub)
dffit_df$r1 <- row.names(dffit_df)
subnew <- merge(sub, dffit_df, all=FALSE)

subnew <- subset(subnew, select= -c(r1))

subnew$absdffit <- abs(subnew$dffit)

subnewinf <- subnew[which(subnew$absdf < 0.064),]



MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual+HouseAge+
            LotArea+BsmtFinSF1+TotalBsmtSF+Style1+Style2,data=subnewinf)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)
