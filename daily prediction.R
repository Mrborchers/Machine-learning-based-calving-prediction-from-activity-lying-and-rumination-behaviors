
## load all these packages

library(lattice) ## package to draw x-y plot
library(caret) ## package for several machine learning techniques
library(e1071) 
library(randomForest) ## package for random forest 

##########################################
### read in .csv data
##########################################

calved <- read.csv("C:\\Users\\Desktop\\day.csv", header=T)


##########################################
### quick look at the graphs of behaviors ~ daysprior
##########################################

xyplot(mtotalmotion ~ daysprior, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved)

xyplot(mtotalsteps ~ daysprior, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved)

xyplot(mhoursstanding ~ daysprior, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved)

xyplot(mhourslying ~ daysprior, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved)

xyplot(mlyingbouts  ~ daysprior, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved)

xyplot(mrumination ~ daysprior, jitter.x=T, type=c("p","smooth"),cex=0.5,lwd=3,data=calved)


############################################
#### Leave one out cross validation
############################################
fitControl <- trainControl(
                           method = "LOOCV",
                           classProbs = T)

##########################################
## select only daysprior for the classification
##########################################

trial1 <- subset(calved, (daysprior==-1 | daysprior==-2 | daysprior==-3 |daysprior==-4| daysprior==-5 | daysprior==-6 | daysprior==-7 | daysprior==-8 |	daysprior==-9 | daysprior==-10 | daysprior==-11 | daysprior==-12 | daysprior==-13 |	daysprior==-14))

trial1$daysprior <- factor(paste("day",abs(trial1$daysprior), sep=""))

levels(trial1$daysprior) <- list("day1"="day1","day2"="day2","day3"="day3","day4"="day4","day5"="day5","day6"="day6","day7"="day7","day8"="day8","day9"="day9",
"day10"="day10","day11"="day11","day12"="day12","day13"="day13","day14"="day14"
)


### only use complete data
trial1 <- trial1[complete.cases(trial1),]

summary(trial1)

set.seed(45)

### randomly select 80% data for training the algorithm, and 20% for testing from subset of data trial 

inTrain1 <- createDataPartition(y=trial1$daysprior, p=0.8, list=F)

training1 <- trial1[ inTrain1,]
testing1 <- trial1[ -inTrain1,]

####################################
## Random forest analysis
####################################

rfFit1 <- train(daysprior ~ parity + predictedcalving + mtotalmotion + mtotalsteps + mlyingbouts + mrumination + 
                      mactivity + mhoursstanding + mhourslying, trControl = fitControl,
                      data=training1, method = "rf")

rfClasses1 <- predict(rfFit1, newdata = testing1)

confusionMatrix(rfClasses1, testing1$daysprior)

#write.csv(comboRF$table, "C:\\Users\\Desktop\\ROutput\\newday\\comborf.csv")


###################################
### Linear discriminant analysis
###################################

ldaFit1 <- train(daysprior ~ parity + predictedcalving + mtotalmotion + mtotalsteps + mlyingbouts + mrumination + 
                   mactivity + mhoursstanding + mhourslying, trControl = fitControl,
                      data=training1, method = "lda")

ldaClasses1 <- predict(ldaFit1, newdata = testing1)

confusionMatrix(ldaClasses1, testing1$daysprior)

#write.csv(comboLDA$table, "C:\\Users\\Desktop\\ROutput\\newday\\combolda.csv")


####################################
## Neural network analysis
####################################

nnetFit1 <- train(daysprior ~ parity + predictedcalving + mtotalmotion + mtotalsteps + mlyingbouts + mrumination + 
                    mactivity + mhoursstanding + mhourslying, trControl = fitControl,trace = FALSE,
                      data=training1, method = "nnet")

nnetClasses1 <- predict(nnetFit1, newdata = testing1)

confusionMatrix(nnetClasses1, testing1$daysprior)

#write.csv(comboNN$table, "C:\\Users\\Desktop\\ROutput\\newday\\combonn.csv")



##############################################
##IceQube
##############################################
####################################
## Random forest analysis
####################################

rfFit1 <- train(daysprior ~ parity + predictedcalving + mtotalmotion + mtotalsteps + mlyingbouts + 
				mhoursstanding + mhourslying, 
                      data=training1, trControl = fitControl,method = "rf")

rfClasses1 <- predict(rfFit1, newdata = testing1)

confusionMatrix(rfClasses1, testing1$daysprior)

#write.csv(iqRF$table, "C:\\Users\\Desktop\\ROutput\\newday\\iqrf.csv")


###################################
### Linear discriminant analysis
###################################

ldaFit1 <- train(daysprior ~ parity + predictedcalving + mtotalmotion + mtotalsteps + mlyingbouts + 
                   mhoursstanding + mhourslying, 
                      data=training1, method = "lda",
                      trControl = fitControl)

ldaClasses1 <- predict(ldaFit1, newdata = testing1)

confusionMatrix(ldaClasses1, testing1$daysprior)

#write.csv(iqLDA$table, "C:\\Users\\Desktop\\ROutput\\newday\\iqlda.csv")


####################################
## Neural network analysis
####################################

nnetFit1 <- train(daysprior ~ parity + predictedcalving + mtotalmotion + mtotalsteps + mlyingbouts + 
                    mhoursstanding + mhourslying, trace = FALSE,
                      data=training1, method = "nnet",trControl = fitControl)

nnetClasses1 <- predict(nnetFit1, newdata = testing1)

confusionMatrix(nnetClasses1, testing1$daysprior)

#write.csv(iqNN$table, "C:\\Users\\Desktop\\ROutput\\newday\\iqnn.csv")







##############################################
##HR-Tag
##############################################
####################################
## Random forest analysis
####################################

rfFit1 <- train(daysprior ~ parity + predictedcalving + mrumination + 
                      mactivity, 
                      data=training1, trControl = fitControl,method = "rf")

rfClasses1 <- predict(rfFit1, newdata = testing1)

confusionMatrix(rfClasses1, testing1$daysprior)

#write.csv(hrRF$table, "C:\\Users\\Desktop\\ROutput\\newday\\hrrf.csv")


###################################
### Linear discriminant analysis
###################################

ldaFit1 <- train(daysprior ~  parity + predictedcalving + mrumination + 
                   mactivity, 
                      data=training1, method = "lda",
                      trControl = fitControl)

ldaClasses1 <- predict(ldaFit1, newdata = testing1)

confusionMatrix(ldaClasses1, testing1$daysprior)

#write.csv(hrLDA$table, "C:\\Users\\Desktop\\ROutput\\newday\\hrlda.csv")


####################################
## Neural network analysis
####################################

nnetFit1 <- train(daysprior ~  parity + predictedcalving + mrumination + 
                    mactivity, trace = FALSE,
                      data=training1, method = "nnet",trControl = fitControl)

nnetClasses1 <- predict(nnetFit1, newdata = testing1)

confusionMatrix(nnetClasses1, testing1$daysprior)

#write.csv(hrNN$table, "C:\\Users\\Desktop\\ROutput\\newday\\hrNN.csv")

