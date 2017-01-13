## load all these packages

library(lattice) ## package to draw x-y plot
library(caret) ## package for several machine learning techniques
library(e1071) 
library(randomForest) ## package for random forest 

##########################################
### read in .csv data
##########################################

calved <- read.csv("C:\\Users\\Desktop\\hour.csv", header=T)

##########################################
### quick look at the graphs of avgactivity ~ daysprior
##########################################

xyplot(avgtotalmotion ~ hoursbefore, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved, subset=(hoursbefore>=-252 ))

xyplot(avgtotalsteps ~ hoursbefore, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved, subset=(hoursbefore>=-252))

xyplot(avghoursstanding ~ hoursbefore, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved, subset=(hoursbefore>=-252))

xyplot(avghourslying ~ hoursbefore, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved, subset=(hoursbefore>=-252))

xyplot(avglyingbouts  ~ hoursbefore, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved, subset=(hoursbefore>=-252))

xyplot(avgrumination ~ hoursbefore, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved, subset=(hoursbefore>=-252))

xyplot(avgactivity ~ hoursbefore, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved, subset=(hoursbefore>=-252))

############################
### 10 fold cross validation
############################

fitControl <- trainControl(## 4-fold CV
                           method = "LOOCV",
#                           number = 4,
                           ## repeated ten times
#                           repeats = 10,
                         classProbs = T)

##########################################
## select only hoursbefore
##########################################

eval(parse(text = paste0( "trial1 <- subset(calved, (hoursbefore==2 |", paste0("hoursbefore==-",seq(2,22,by=2), collapse=" | "), "))")))

trial1$hoursbefore <- factor(paste("hoursbefore",abs(trial1$hoursbefore),sep=""))


eval(parse(text=paste0( "levels(trial1$hoursbefore) <- list(", paste0("'hoursbefore",seq(2,22,by=2),"'=","'hoursbefore",seq(2,22,by=2),"'", collapse=", "), ")")))

### only use complete data

trial1 <- trial1[complete.cases(trial1),]

summary(trial1)

set.seed(45)

### randomly select 80% data for training the algorithm, and 20% for testing from subset of data trial 

inTrain1 <- createDataPartition(y=trial1$hoursbefore, p=0.8, list=F)

training1 <- trial1[ inTrain1,]
testing1 <- trial1[ -inTrain1,]

####################################
## Random forest analysis
####################################

rfFit1 <- train(hoursbefore ~ parity + TIME + avgtotalmotion + avgtotalsteps + avglyingbouts + avgrumination + 
                  avgactivity + avghoursstanding + avghourslying, trControl = fitControl,
                data=training1, method = "rf")

rfClasses1 <- predict(rfFit1, newdata = testing1)

confusionMatrix(rfClasses1, testing1$hoursbefore)
#comboRF <- 
#write.csv(comboRF$table, "C:\\Users\\Desktop\\ROutput\\hour\\comboRF.csv")

###################################
### Linear discriminant analysis
###################################

ldaFit1 <- train(hoursbefore ~ parity + TIME + avgtotalmotion + avgtotalsteps + avglyingbouts + avgrumination + 
                   avgactivity + avghoursstanding + avghourslying, trControl = fitControl,
                 data=training1, method = "lda")

ldaClasses1 <- predict(ldaFit1, newdata = testing1)

confusionMatrix(ldaClasses1, testing1$hoursbefore)
#combolda <- 
#write.csv(combolda$table, "C:\\Users\\Desktop\\ROutput\\hour\\LDACombo.csv")
####################################
## Neural network analysis
####################################

nnetFit1 <- train(hoursbefore ~ parity + TIME + avgtotalmotion + avgtotalsteps + avglyingbouts + avgrumination + 
                    avgactivity + avghoursstanding + avghourslying, trControl = fitControl,trace = FALSE,
                  data=training1, method = "nnet")

nnetClasses1 <- predict(nnetFit1, newdata = testing1)

confusionMatrix(nnetClasses1, testing1$hoursbefore)
#combonn <- 
#write.csv(combonn$table, "C:\\Users\\Desktop\\ROutput\\hour\\NNCombo.csv")


##############################################
##IceQube
##############################################
####################################
## Random forest analysis
####################################

rfFit1 <- train(hoursbefore ~ parity + TIME + avgtotalmotion + avgtotalsteps + avglyingbouts + 
                  avghoursstanding + avghourslying, 
                data=training1, trControl = fitControl,method = "rf")

rfClasses1 <- predict(rfFit1, newdata = testing1)

confusionMatrix(rfClasses1, testing1$hoursbefore)
#IQRF <- 
#write.csv(IQRF$table, "C:\\Users\\Desktop\\ROutput\\hour\\IQRF.csv")

###################################
### Linear discriminant analysis
###################################

ldaFit1 <- train(hoursbefore ~ parity + TIME + avgtotalmotion + avgtotalsteps + avglyingbouts + 
                   avghoursstanding + avghourslying, 
                 data=training1, method = "lda",
                 trControl = fitControl)

ldaClasses1 <- predict(ldaFit1, newdata = testing1)

confusionMatrix(ldaClasses1, testing1$hoursbefore)
#IQLDA <- 
#write.csv(IQLDA$table, "C:\\Users\\Desktop\\ROutput\\hour\\IQLDA.csv")


####################################
## Neural network analysis
####################################

nnetFit1 <- train(hoursbefore ~ parity + TIME + avgtotalmotion + avgtotalsteps + avglyingbouts + 
                    avghoursstanding + avghourslying, trace = FALSE,
                  data=training1, method = "nnet",trControl = fitControl)

nnetClasses1 <- predict(nnetFit1, newdata = testing1)

confusionMatrix(nnetClasses1, testing1$hoursbefore)
#IQNN <- 

#write.csv(IQNN$table, "C:\\Users\\Desktop\\ROutput\\hour\\IQNN.csv")






##############################################
##HR-Tag
##############################################
####################################
## Random forest analysis
####################################

rfFit1 <- train(hoursbefore ~ parity + TIME + avgrumination + avgactivity, 
                data=training1, trControl = fitControl,method = "rf")

rfClasses1 <- predict(rfFit1, newdata = testing1)

confusionMatrix(rfClasses1, testing1$hoursbefore)
#HRRF <- 
#write.csv(HRRF$table, "C:\\Users\\Desktop\\ROutput\\hour\\HRRF.csv")


###################################
### Linear discriminant analysis
###################################

ldaFit1 <- train(hoursbefore ~  parity + TIME + avgrumination + 
                   avgactivity, 
                 data=training1, method = "lda",
                 trControl = fitControl)

ldaClasses1 <- predict(ldaFit1, newdata = testing1)

confusionMatrix(ldaClasses1, testing1$hoursbefore)
#HRLDA <- 
#write.csv(HRLDA$table, "C:\\Users\\Desktop\\ROutput\\hour\\HRLDA.csv")


####################################
## Neural network analysis
####################################

nnetFit1 <- train(hoursbefore ~  parity + TIME + avgrumination + 
                    avgactivity, trace = FALSE,
                  data=training1, method = "nnet",trControl = fitControl)

nnetClasses1 <- predict(nnetFit1, newdata = testing1)

confusionMatrix(nnetClasses1, testing1$hoursbefore)
#HRNN <- 
#write.csv(HRNN$table, "C:\\Users\\Desktop\\ROutput\\hour\\HRNN.csv")
