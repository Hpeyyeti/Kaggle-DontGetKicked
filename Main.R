setwd("/bujji/bujji/SEM 3/CMPE 239/Project data")

require(ggplot2)
require(pROC)
require(car)
require(FSelector)
require(caret)
require(caTools)
library(DMwR)
require(ROCR)
#######################
#Data reading
#######################

# Read Test and Training data from csv files
train <- read.csv("training_carvana.csv", sep=',', header=T, na.strings = c("NA", "na","NULL", "null", ""), stringsAsFactors=F)
test <- read.csv("test_carvana.csv", sep=',', header=T, na.strings = c("NA", "na","NULL", "null", ""), stringsAsFactors=F)

train$dataset <- "train"
test$dataset <- "test"
test$IsBadBuy <- 1
print(prop.table(table(train$IsBadBuy)))
# get the test data columns in order of those of the train
test <- test[,intersect(names(train),names(test))]

# combine row wise all the data
df.all <- rbind(train, test)
rm(train)
rm(test)

###########################
#Data cleaning
###########################
#1) Replace missing values for numeric variables with median 
#and character variables with #NA#

handleMissingValues <- function(df){
  
  replaceMissing <- function(colName){
    x <- df[,colName]
    na <- is.na(x)
    if(any(na)){
      if(is.numeric(x)){
        x[na] <- median(x, na.rm=T)
        print(sprintf("***Numeric: %i", sum(na)))
      }else{
        x <- tolower(x)
        x[na] <- "#NA#"
        x <- as.factor(x)
        print(sprintf("NON-Numeric: %i", sum(na)))
      }
    }
    return(x)
  }    
  
  df <- sapply(names(df), replaceMissing, simplify=F)
  return(as.data.frame(df))
}

df.all <- handleMissingValues(df.all)

#2) Drop nearZeroVar variables and useless variables
#Check for nearzero variance in the feature distribution
#nzv diagnoses predictors that have one unique value 
#or predictors that have very few unique values relative to the number of samples and 
#the ratio of the most common value to the second most common value is large.
#nearZeroVar(df.all,saveMetrics = T, freqCut=95/5)
#PRIMEUNIT, Transmission, IsOnlineSale,AUCGUART, Nationality have near zero var and are hence removed.
# Remove Wheel type id which is ID of wheel type
df.all$PRIMEUNIT <- NULL
df.all$Transmission <- NULL
df.all$IsOnlineSale <- NULL
df.all$AUCGUART <- NULL
df.all$WheelTypeID <- NULL

#3) Transform variables
#Create Purchase month and year from date
#df.all$PurchMonth <- format(as.Date(df.all$PurchDate, "%m/%d/%Y"), format="%m")
df.all$PurchYear <- format(as.Date(df.all$PurchDate, "%m/%d/%Y"), format="%Y")
df.all$Qtr <- quarters(as.Date(df.all$PurchDate, "%m/%d/%Y"))
df.all$PurchDate <- NULL

# Regroup Color to form fewer buckets
df.all$Brand <- recode(df.all$Make, "c('HUMMER','OLDSMOBILE','TOYOTA SCION','PLYMOUTH','MINI','SUBARU','LEXUS','CADILLAC',
                       'LINCOLN','INFINITI','VOLVO','ACURA','SCION','VOLKSWAGEN', 'ISUZU',
                       'HONDA','GMC','BUICK','MERCURY','MAZDA') ='OTHERS'")
df.all$Make <- NULL

df.all$ColorNew <- recode(df.all$Color, "c('beige','gold','orange','purple','yellow','other','pink','brown',
                          'not avail','#NA#','green','maroon') ='others'")
df.all$Color <- NULL
# Command Shift C multiple lines code comment

#4) Convert character variables to factor variables
df.all$IsBadBuy <- as.factor(df.all$IsBadBuy)
#change this to numeric for glm model
df.all$VehYear <- as.factor(df.all$VehYear)
df.all$VehicleAge <- as.numeric(df.all$VehicleAge)
df.all$WheelType <- as.factor(df.all$WheelType)
df.all$BYRNO <- as.factor(df.all$BYRNO)
df.all$VNZIP1 <- as.factor(df.all$VNZIP1)
df.all$VNST <- as.factor(df.all$VNST)
df.all$PurchMonth <- as.factor(df.all$PurchMonth)
df.all$PurchYear <- as.factor(df.all$PurchYear)
df.all$VehAge <- ifelse(df.all$VehicleAge <= 1, 1, df.all$VehicleAge)
df.all$VehicleAge <- NULL

df.all$DiffAucCleanPrice <- (df.all$MMRAcquisitionAuctionCleanPrice - df.all$MMRCurrentAuctionCleanPrice)/df.all$VehAge
df.all$DiffAucAvgPrice <- (df.all$MMRAcquisitionAuctionAveragePrice - df.all$MMRCurrentAuctionAveragePrice)/df.all$VehAge
df.all$DiffRetCleanPrice <- (df.all$MMRAcquisitonRetailCleanPrice - df.all$MMRCurrentRetailCleanPrice)/df.all$VehAge
df.all$DiffRetAvgPrice <- (df.all$MMRAcquisitionRetailAveragePrice - df.all$MMRCurrentRetailAveragePrice)/df.all$VehAge

df.all$MMRAcquisitionAuctionCleanPrice <-NULL
df.all$MMRCurrentAuctionCleanPrice <-NULL
df.all$MMRAcquisitionAuctionAveragePrice <-NULL
df.all$MMRCurrentAuctionAveragePrice <-NULL
df.all$MMRAcquisitonRetailCleanPrice <-NULL
df.all$MMRCurrentRetailCleanPrice <-NULL
df.all$MMRAcquisitionRetailAveragePrice <-NULL
df.all$MMRCurrentRetailAveragePrice <-NULL

#Model -- 1130 levels Trim -- 137 levels Submodel -- 934 levels ZIPCode -- 200 levels
df.all$Trim<- NULL
df.all$Model <- NULL
df.all$SubModel <- NULL
df.all$VNZIP1 <- NULL
df.all$BYRNO <- NULL
df.all$VNST <- NULL
str(df.all)
df.all$RefId <- as.character(df.all$RefId)
df.all$VehAge <- as.character(df.all$VehAge)

#5) Normalize numeric variables in the range 0 to 1
preProcValues <- preProcess(df.all, method = "range")
df.all <- predict(preProcValues, df.all)
df.all$RefId <- as.numeric(df.all$RefId)
df.all$VehAge <- as.numeric(df.all$VehAge)

# df.all$target <- as.factor(df.all$IsBadBuy)
# df.all$IsBadBuy <- NULL
#Data subsetting before running the model
#Split training set into sub-training and sub-testing
train.clean <- subset(df.all, dataset=="train")
test.clean <- subset(df.all, dataset=="test")
train.clean$dataset <- NULL
test.clean$dataset <- NULL

predictorsNames <- c('WheelType','VehOdo','Brand','DiffRetAvgPrice','DiffAucCleanPrice','DiffAucAvgPrice','VehAge')

train.clean$IsBadBuy1 <- ifelse(train.clean$IsBadBuy==1,'bad','good')

set.seed(1234)
split = sample.split(train.clean$IsBadBuy, SplitRatio = 0.7)
train.train <- subset(train.clean, split == TRUE)
train.test <- subset(train.clean, split == FALSE)
# str(train.train)
# prop.table(table(train.train$target))
# prop.table(table(train.test$target))

#############Balancing data#######################
tr.clean <-train.clean
tr.clean$IsBadBuy1 <- as.factor(tr.clean$IsBadBuy1)
tr.clean$IsBadBuy <- NULL
tr.clean$RefId <- NULL
tr.clean$Auction <- NULL
tr.clean$VehYear <- NULL
tr.clean$Nationality <- NULL
tr.clean$Size <- NULL
tr.clean$TopThreeAmericanName <- NULL
tr.clean$VehBCost <- NULL
tr.clean$WarrantyCost <- NULL
tr.clean$PurchYear <- NULL
tr.clean$ColorNew <- NULL
tr.clean$Qtr<- NULL

trbal.clean <- SMOTE(IsBadBuy1 ~ ., tr.clean, perc.over = 500, perc.under = 120)
table(trbal.clean$IsBadBuy) ## 50-50 good and bad instances. data is balanced
rm(tr.clean)
#################################################
# gbm
#################################################

objControl <- trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

# run model
model.gbm <- train(IsBadBuy1 ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                   data=train.train,
                   method='gbm', 
                   trControl=objControl,  
                   metric = "ROC",
                   verbose = FALSE)

ggplot(model.gbm)

# find out variable importance of gbm 
summary(objModel1)

# find out model details
model.gbm

# probabilites 
predictions <- predict(object=model.gbm, train.test[,predictorsNames], type='prob')
auc <- roc(ifelse(train.test$IsBadBuy1=="bad",1,0), predictions[[1]])
plot(auc, col='blue')
print(auc$auc)
pred.gbm <- prediction(predictions[[1]],train.test$IsBadBuy)
RP.perf <- performance(pred, "f")

plot(RP.perf)
RP.perf1 <- performance(pred, "prec", "rec")
plot (RP.perf1)
# ROC.perf2 <- performance(pred, "tpr", "fpr")
# plot (ROC.perf2)

conf<- pred.gbm >= 0.25
confus.gbm <- table(conf, train.test$IsBadBuy)
confus.gbm
# display variable importance on a +/- scale 
vimp <- varImp(model.logitbal, scale=F)
results <- data.frame(row.names(vimp$importance),vimp$importance$Overall)
results$VariableName <- rownames(vimp)
colnames(results) <- c('VariableName','Weight')
results <- results[order(results$Weight),]
results <- results[(results$Weight != 0),]

par(mar=c(5,15,4,2)) # increase y-axis margin. 
xx <- barplot(results$Weight, width = 0.85, 
              main = paste("Variable Importance -",outcomeName), horiz = T, 
              xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE, 
              col = ifelse((results$Weight > 0), 'blue', 'red')) 
axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6) 

############ Balanced data gbm ###########
model.gbmbal <- train(IsBadBuy1 ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                   data=trbal.clean,
                   method='gbm', 
                   trControl=objControl,  
                   metric = "ROC",
                   verbose = FALSE)

ggplot(model.gbmbal)
# find out variable importance of gbm 
summary(model.gbmbal)

# find out model details
model.gbmbal

# probabilites 
predictions <- predict(object=model.gbmbal, train.test[,predictorsNames], type='prob')
auc <- roc(ifelse(train.test$IsBadBuy1=="bad",1,0), predictions[[1]])
print(auc$auc)

pred.gbmbal <- prediction(predictions[[1]],train.test$IsBadBuy)
RP.perf <- performance(pred.gbmbal, "f")

plot(RP.perf)
RP.perf1 <- performance(pred, "prec", "rec")
plot (RP.perf1)
# ROC.perf2 <- performance(pred, "tpr", "fpr")
# plot (ROC.perf2)

conf<- pred.gbmbal >= 0.65
confus.gbmbal <- table(conf, train.test$IsBadBuy)
confus.gbmbal
################################################
# glm model
################################################

# pick model gbm and find out what type of model it is
getModelInfo()$glmnet$type
outcomeName <- 'IsBadBuy'
# split data into training and testing chunks
# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=5, returnResamp='none')
model.glm <- train(IsBadBuy ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                   data=train.train,
                   method='glmnet', 
                   metric = "RMSE",
                   trControl=objControl)

pred_glm <- predict(object=model.glm , train.test[,predictorsNames])
auc <- roc(train.test$IsBadBuy, pred_glm)
print(auc$auc)
plot(auc, col='blue')
pred <- prediction(pred_glm,train.test$IsBadBuy)
RP.perf <- performance(pred, "f")

plot(RP.perf)
RP.perf1 <- performance(pred, "prec", "rec")
plot (RP.perf1)
# ROC.perf2 <- performance(pred, "tpr", "fpr")
# plot (ROC.perf2)

conf<- pred_glm >= 0.20
confus.logit2 <- table(conf, train.test$IsBadBuy)
confus.logit2
######### Balanced data glm ##########
trbal.clean$IsBadBuy <- ifelse(trbal.clean$IsBadBuy1=="bad",1,0)
model.glmbal  <- train(IsBadBuy ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                   data=trbal.clean,
                   method='glmnet', 
                   metric = "RMSE",
                   trControl=objControl)
pred_glm <- predict(object=model.glmbal, train.test[,predictorsNames])
auc <- roc(train.test$IsBadBuy, pred_glm)
print(auc$auc)
pred <- prediction(pred_glm,train.test$IsBadBuy)
RP.perf <- performance(pred, "f")

plot(RP.perf)
RP.perf1 <- performance(pred, "prec", "rec")
plot (RP.perf1)

conf<- pred_glm >= 0.60
confus.logit2 <- table(conf, train.test$IsBadBuy)
confus.logit2
################################################
# Logitboost
################################################

# pick model gbm and find out what type of model it is
getModelInfo()$glmnet$type
outcomeName <- 'IsBadBuy1'
# split data into training and testing chunks
# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=5, returnResamp='none')
#predictorsNames <- names(train.train[, - c(1,2)])

model.logit <- train(IsBadBuy1 ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                   data=train.train,
                   method='LogitBoost', 
                   trControl=objControl
)

# probabilites 
pred_test2 <- predict(object=model.logit, train.test[,predictorsNames], type='prob')
auc <- roc(ifelse(train.test[,outcomeName]=="bad",1,0), pred_test2[[1]])
print(auc$auc)
plot(auc, col='blue')
pred <- prediction(pred_test2[[1]],train.test$IsBadBuy)
RP.perf <- performance(pred, "f")

plot(RP.perf)
RP.perf1 <- performance(pred, "prec", "rec")
plot (RP.perf1)
ggplot(model.logit)
conf<- pred_test2[[1]] >= 0.30
confus.l <- table(conf, train.test$IsBadBuy)
confus.l
############Balanced data Logitboost#########################
model.logitbal <- train(IsBadBuy1 ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                   data=trbal.clean,
                   method='LogitBoost', 
                   trControl=objControl
)

# probabilites 
pred_test3 <- predict(object=model.logitbal, train.test[,predictorsNames], type='prob')
auc <- roc(ifelse(train.test[,outcomeName]=="bad",1,0), pred_test2[[1]])
print(auc$auc)
ggplot(model.logitbal)
pred <- prediction(pred_test3[[1]],train.test$IsBadBuy)
RP.perf <- performance(pred, "f")

plot(RP.perf)
RP.perf1 <- performance(pred, "prec", "rec")
plot (RP.perf1)
ggplot(model.logit)
conf<- pred_test3[[1]] >= 0.75
confus.l <- table(conf, train.test$IsBadBuy)
confus.l
 
################################################
# SVM
################################################
library(kernlab)
svmFit <- train(IsBadBuy1 ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                data=train.train,
                method='svmLinear', 
                trControl=objControl,
                metric = "ROC",
                tuneLength = 5)


svmFit
ggplot(svmFit)

# find out variable importance of gbm 
summary(svmFit)

# probabilites 
predictions <- predict(object=svmFit, train.test[,predictorsNames], type='prob')
auc <- roc(ifelse(train.test$IsBadBuy1=="bad",1,0), predictions[[1]])
print(auc$auc)

pred <- prediction(predictions[[1]],train.test$IsBadBuy)
RP.perf <- performance(pred, "f")
plot(RP.perf)
conf<- predictions[[1]] >= 0.3
confus.l <- table(conf, train.test$IsBadBuy)
confus.l
#############svmradial###########
svmRad <- train(IsBadBuy1 ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                data=train.train,
                method='svmRadial', 
                trControl=objControl,
                metric = "ROC",
                tuneLength = 5)


svmRad
ggplot(svmRad)

# find out variable importance of gbm 
summary(svmRad)

# probabilites 
predictions.rad <- predict(object=svmRad, train.test[,predictorsNames], type='prob')
auc <- roc(ifelse(train.test$IsBadBuy1=="bad",1,0), predictions.rad[[1]])
print(auc$auc)

pred <- prediction(predictions.rad[[1]],train.test$IsBadBuy)
RP.perf <- performance(pred, "f")
plot(RP.perf)
conf<- predictions.rad[[1]] >= 0.3
confus.l <- table(conf, train.test$IsBadBuy)
confus.l
########SVM on balanced data##########
svmFit.bal <- train(IsBadBuy1 ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo,
                data=trbal.clean,
                method='svmLinear', 
                trControl=objControl,
                metric = "ROC",
                tuneLength = 5)


svmFit.bal
ggplot(svmFit.bal)

# find out variable importance of gbm 
summary(svmFit.bal)

# probabilites 
predictions.sbal <- predict(object=svmFit.bal, train.test[,predictorsNames], type='prob')
auc <- roc(ifelse(train.test$IsBadBuy1=="bad",1,0), predictions.sbal[[1]])
print(auc$auc)
pred <- prediction(predictions.sbal[[1]],train.test$IsBadBuy)
RP.perf <- performance(pred, "f")
plot(RP.perf)
conf<- predictions.sbal[[1]] >= 0.6
confus.l <- table(conf, train.test$IsBadBuy)
confus.l

#############Compare models##########


resamps <- resamples(list(
  GBM=model.gbm,
  SVM=svmFit,
  rda=svmRad,
  GLM=model.glm,
  LogitBoost=model.logit))
resamps







#1glmodel <- glm(IsBadBuy ~ WheelType+DiffAucCleanPrice+DiffRetAvgPrice+DiffAucAvgPrice+Brand+VehAge+VehOdo, data=train.train, family="binomial")
summary(glmodel2)
glmodel2 <- glm(IsBadBuy ~ WheelType + DiffAucAvgPrice + DiffAucCleanPrice + VehAge + DiffRetAvgPrice + VehYear + DiffRetCleanPrice + VehBCost + WarrantyCost, data=train.train, family="binomial")




prob.logit2 <- predict(glmodel2, newdata = train.test, type = "response")
summary(prob.logit2)
head(prob.logit2, n=10)
pred.logit2 <- prob.logit2 >= 0.50

test.all$dataset <- NULL

confus.logit2 <- table(pred.logit2, train.test$IsBadBuy)
confus.logit2
accur.logit2 <- (confus.logit2[1,1]+confus.logit2[2,2])/(confus.logit2[1,1]+confus.logit2[2,2] + confus.logit2[1,2] + confus.logit1[2,1])
accur.logit2

pred2 <- prediction(prob.logit2, train.test$IsBadBuy) 
perf <- performance(pred, measure='acc') #Simple accuracy, what % were right?

perf <- performance(pred, measure='prec') #What % of the elements I predicted to be in the class actually?
perf <- performance(pred, measure='recall') #What % of the elements that are in class, did I predict to be in this class?

perf <- performance(pred, measure='f') #F-measure a balance between them
perf <- performance(pred2, measure='auc') 



weights <- information.gain(IsBadBuy~., train.clean)
weights<- cbind(variable = rownames(weights), weights)
row.names(weights)<-NULL
sorted_weights<-weights[order(-weights$attr_importance),]
sorted_weights



##############


# IsBadBuy
# VehYear
# VehicleAge
# WheelTypeID <- remove
# WheelType
# BYRNO
# VNZIP1
# VNST
# IsOnlineSale
# PurchMonth
# PurchYear

# length(unique(df.all$VNST))
# unique(df.all$WheelType)
# 
# cor(df.all$WheelTypeID, xtfrm(df.all$WheelType))


# View(head(df.all[, c("PurchDate", "PurchMonth", "PurchYear")], n = 100))
#idCol <- "RefId"
#targetCol <- "IsBadBuy"
# #Trying Feature hashing
# library(FeatureHashing)
# df_hash <- subset(df.all, dataset=="train")
# predictorNames <- setdiff(names(df_hash), df.all$IsBadBuy)
# #predictorNames <- setdiff(names(df_hash), c(df.all$IsBadBuy, df.all$dataset))
# 
# set.seed(1234)
# split <- sample(nrow(df_hash), floor(0.7*nrow(df_hash)))
# objTrain <-df_hash[split,]
# objTest <- df_hash[-split,]
# 
# objTrain_hashed = hashed.model.matrix(~., data=objTrain[,predictorNames],hash.size = 2^20, transpose=FALSE, create.mapping=TRUE)
# objTrain_hashed = as(objTrain_hashed, "dgCMatrix")
# objTest_hashed = as(objTest_hashed, "dgCMatrix")
# objTest_hashed = hashed.model.matrix(~., data=objTest[,predictorNames], hash.size=2^20, transpose=FALSE, create.mapping=TRUE)
# 
# library(glmnet)
# glmnetModel <- cv.glmnet(objTrain_hashed, objTrain[,df.all$IsBadBuy], 
#                          family = "binomial", type.measure = "auc")
# # Integer overflow; num_classes*num_lambda*pmax should not exceed .Machine$integer.max. Reduce pmax to be below 372
# 


# #Dummifying all categorical variables
# dmy <- dummyVars(" ~ .", data = train.clean, fullRank=T)
# trsf <- data.frame(predict(dmy, newdata = train.clean))
# trsf$Outcome <- train.clean$IsBadBuy
# trsf$IsBadBuy.1 <- NULL
# print(trsf)
# str(df.all)