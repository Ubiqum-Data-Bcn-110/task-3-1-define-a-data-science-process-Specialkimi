#### library upload, formats #### 
library(caret)
library(dplyr)
library(matrixStats)
library(apply)

#install.packages("matrixStats")
install.packages(class)
install.packages()
library(class)
library(e1071)
####uploading dataset####
setwd("~/Desktop/tasks /M3T2")

validationData <- read.csv("task3-2-wifi-Specialkimi/validationData.csv",stringsAsFactors=FALSE)
#View(validationData)
trainingData <- read.csv("task3-2-wifi-Specialkimi/trainingData.csv",stringsAsFactors=FALSE)
#View(trainingData)

####change formats training ####
trainingData[,1:520] <- apply(trainingData[,1:520],2,function(x) as.double(x))
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
trainingData$BUILDINGID <- factor(trainingData$BUILDINGID,levels = c(0,1,2),labels = c("0","1","2"))
trainingData$FLOOR <- factor(trainingData$FLOOR,levels = c(0,1,2,3,4),labels = c("0","1","2","3","4"))
trainingData$SPACEID <- as.factor(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)  
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$LONGITUDE <- as.double(trainingData$LONGITUDE)
trainingData$LATITUDE <- as.double(trainingData$LATITUDE)
trainingData$FLOOR <- as.factor(trainingData$FLOOR)

####change formats validation ####
validationData[,1:520] <- apply(validationData[,1:520],2,function(x) as.double(x))
validationData$PHONEID <- as.factor(validationData$PHONEID)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)  
validationData$USERID <- as.factor(validationData$USERID)
validationData$LONGITUDE <- as.double(validationData$LONGITUDE)
validationData$LATITUDE <- as.double(validationData$LATITUDE)
validationData$FLOOR <- as.factor(validationData$FLOOR)

#means for rows
means_per_row_training <- apply(trainingData[,1:520],1,function(x) mean(x))
row_to_del_train <- which(means_per_row_training == 100)
means_per_row_validation <- apply(validationData[,1:520],1,function(x) mean(x))
row_to_del_valid <- which(means_per_row_validation == 100)

# rows to remove in train
if (length(row_to_del_train) != 0) {
  trainingData <- trainingData[-row_to_del_train,] 
}

# rows to remove from validation
if (length(row_to_del_valid) != 0) {
  validationData <- validationData[-row_to_del_valid,]
}

#### detect WAPS with mean == 100  ####
means_per_column_training <- apply(trainingData[,1:520],2,function(x) mean(x))
col_to_del_train <- which(means_per_column_training == 100)
trainingData <- trainingData[,-col_to_del_train]
means_per_column_validation <- apply(validationData[,1:520],2,function(x) mean(x))
col_to_del_valid <- which(means_per_column_validation == 100)
validationData <- validationData[,-col_to_del_valid]

#removing null from columns
trainingData <- trainingData[,-col_to_del_train]
validationData <- validationData[,-col_to_del_valid]

# interseccion
common_col <- intersect(names(trainingData),names(validationData))

# nos quedamos con las columnas comunes en ambos datsets
trainingData   <- trainingData[,names(trainingData) %in% common_col]
validationData <- validationData[,names(validationData) %in% common_col]

####datapartition####
muestra <- createDataPartition(trainingData$BUILDINGID, p=0.01, list=FALSE)
#fuera <- setdiff(1:nrow(trainset),muestra)
df_muestra <- trainingData[muestra,]
trainset <- createDataPartition(df_muestra$BUILDINGID, p=0.70, list=FALSE)
df_for_train <- df_muestra[trainset,]
#df_for_test  <- trainingData[-trainset,]
df_for_test  <- df_muestra[-trainset,]

#### create BF for T ####
trainingData <- mutate(trainingData,  BF = paste0(BUILDINGID,FLOOR)) 
trainingData$BF <- as.factor(trainingData$BF)
df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
df_for_train$BF <- as.factor(df_for_train$BF)
validationData <- mutate(validationData,BF = paste0(BUILDINGID,FLOOR))
validationData$BF <- factor(validationData$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)



#####intersect ####
#trainingData <- trainingData[,intersect(names(validationData),names(trainingData))]
#validationData <- validationData[,intersect(names(validationData),names(trainingData))]
#intersect(names(validationData),names(trainingData))

#### replace build predciction on validation #### 
validationData$predictbuild <-svm_linear_pred
#### removing building column of validation ####
validationDatafloor <- validationData[,-316]


#### split by build for train/valid ####
Build0training <- (filter(trainingData, BUILDINGID == 0))
Build0training$BF <- factor(Build0training$BF)
Build1training<- (filter(trainingData, BUILDINGID == 1))
Build1training$BF <- factor(Build1training$BF)
Build2training <- (filter(trainingData, BUILDINGID == 2))
Build2training$BF <- factor(Build2training$BF)

Build0validationfloor <- (filter(validationDatafloor, predictbuild == 0))
Build0validationfloor$predictbuild <- factor(Build0validationfloor$predictbuild)
Build1validationfloor<- (filter(validationData, predictbuild == 1))
Build1validationfloor$BF <- factor(Build1validation$BF)
Build2validationfloor <- (filter(validationData, predictbuild == 2))
Build2validation$BF <- factor(Build2validationfloor$BF)

####100means + BF for B0 T####
means_per_column_training_B0 <- apply(Build0training[,1:312],2,function(x) mean(x))
col_to_del_train_B0 <- which(means_per_column_training_B0 == 100)
means_per_row_training_B0 <- apply(Build0training[,1:312],1,function(x) mean(x))
row_to_del_train_B0 <- which(means_per_row_training_B0 == 100)
Build0training <- Build0training[,-col_to_del_train_B0]
#trainingData[, c(3,4,92,93,94,95,152,158,159,160,215,217,226,227,
#                 238,239,240,241,242,243,244,245,246,247,254,293, 296,301,303,304,307,333,349,353)]=NULL


Build0training <- mutate(Build0training,  BF = paste0(BUILDINGID,FLOOR)) 
Build0training$BF <- as.factor(Build0training$BF)
#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

####100means + BF for B0 V ####
means_per_column_validation_B0 <- apply(Build0validation[,1:312],2,function(x) mean(x))
col_to_del_valid_B0 <- which(means_per_column_validation_B0 == 100)
means_per_row_validation_B0 <- apply(validationData[,1:312],1,function(x) mean(x))
row_to_del_valid_B0 <- which(means_per_row_validation_B0 == 100)
Build0validation <- Build0validation[,-col_to_del_valid_B0]
#Build0validation <- Build0validation[-row_to_del_train_B0,]
#validationData[, c(3,4,92,93,94,95,152,158,159,160,215,217,226,227,
#                 238,239,240,241,242,243,244,245,246,247,254,293, 296,301,303,304,307,333,349,353)]=NULL


Build0validation <- mutate(Build0validation,  BF = paste0(BUILDINGID,FLOOR)) 
Build0validation$BF <- as.factor(Build0validation$BF)
#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

intersect(names(Build0validation),names(Build0training))

####100means + BF for B1 T ####
means_per_column_training_B1 <-apply(Build1training[,1:312],2,function(x) mean(x))
col_to_del_train_B1 <- which(means_per_column_training_B1 == 100)
means_per_row_training_B1 <- apply(trainingData[,1:312],1,function(x) mean(x))
row_to_del_train_B1 <- which(means_per_row_training_B1 == 100)
Build1training <- Build1training[,-col_to_del_train_B1]

Build1training <- mutate(Build1training,  BF = paste0(BUILDINGID,FLOOR)) 
Build1training$BF <- as.factor(Build1training$BF)
df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)


####100means + BF for B1 V####
means_per_column_validation_B1 <- apply(Build1validation[,1:312],2,function(x) mean(x))
col_to_del_valid_B1 <- which(means_per_column_validation_B1 == 100)
means_per_row_validation_B1 <- apply(Build1validation[,1:312],1,function(x) mean(x))
row_to_del_train_B1 <- which(means_per_row_validation_B1 == 100)
Build1validation <- Build1validation[,-col_to_del_valid_B1]


Build1validation <- mutate(Build1validation,  BF = paste0(BUILDINGID,FLOOR)) 
Build1validation$BF <- as.factor(Build1validation$BF)

#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

intersect(names(Build1validation),names(Build1training))

####100means + BF for B2 T####
means_per_column_training_B2 <- apply(Build2training [,1:312],2,function(x) mean(x))
col_to_del_train_B2 <- which(means_per_column_training_B2 == 100)
means_per_row_training_2 <- apply(Build2training[,1:312],1,function(x) mean(x))
row_to_del_train_B2 <- which(means_per_row_training_2 == 100)

Build2training <- mutate(Build2training,  BF = paste0(BUILDINGID,FLOOR)) 
Build2training$BF <- as.factor(Build2training$BF)
#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

#### intersect B1
common_col_B1 <-intersect(names(Build1validation),names(Build1training))
Build1training <-Build1training[,common_col_B1]
Build1validation <-Build1training[,common_col_B1]

####100means + BF for B2 V####
means_per_column_validation_B2 <- apply(Build2validation [,1:312],2,function(x) mean(x))
col_to_del_valid_B2 <- which(means_per_column_validation_B2 == 100)
means_per_row_validation_2 <- apply(Build2validation[,1:114],1,function(x) mean(x))
row_to_del_train_B2 <- which(means_per_row_validation_2 == 100)
Build2validation <- Build0validation[,-col_to_del_valid_B2]
#Build2validation <- Build0validation[-row_to_del_train_B2,]

Build2validation <- mutate(Build2validation,  BF = paste0(BUILDINGID,FLOOR)) 
Build2validation$BF <- as.factor(Build2validation$BF)
#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)


dim(df_for_train)
dim(df_for_test)
head(df_for_train)



#### knn for BUILD ####
ctrl <- trainControl(method="repeatedcv",number = 10, repeats = 1) 
#model1knnfit <- train(y=df_for_train$BUILDINGID, x=df_for_train[,1:312], method = "knn", trcontrol = ctrl)

#knnPredict1 <- predict(model1knnfit, newdata = validationData )
confusionMatrix(knnPredict1, validationData$BUILDINGID)
#model1 <- knn(train = df_for_train, test = df_for_test, trcontrol = ctrl )
#sum(apply(trainingData[,1:283],2,function(x) is.numeric(x)))

#### svm for BUILD #### 
svm_Linear <- train(y=df_for_train$BUILDINGID, x=df_for_train[,1:312], method = "svmLinear",
                    trControl=ctrl,
                    #preProcess = c("center", "scale"),
                    tuneLength = 10)

saveRDS(svm_Linear,file="svm_linear.rds")

svm_linear_pred <- predict(svm_Linear, validationData[,1:312])
confusionMatrix(svm_linear_pred, validationData$BUILDINGID)

#### DT for BUILD ####
dt_training_BUILDING <- rpart(BUILDINGID~., 
                              method="class", data=df_for_train[,c(1:312,316)])

df_for_train <- trainingData[trainset,]
df_for_test  <- trainingData[-trainset,]

# knn for floor
ctrl <- trainControl(method="repeatedcv", number=10 ,repeats = 3) 
model1knnfit_floor <- train(y=df_for_train$BF, x=df_for_train[,1:313], method = "knn", trcontrol = ctrl)
kk <- train(BF~.,data=df_for_train[,c(1:312,322)], method = "knn", trcontrol = ctrl)
#preProcess = c("center","scale"))

knnPredict1_floor <- predict(model1knnfit, newdata = validationData )
confusionMatrix(knnPredict1_floor, validationData$BF)

#### svn for FLOOR ####
svm_Linear_floor <- train(y=df_for_train$BF, x=df_for_train[,1:313], method = "svmLinear", trControl=ctrl,
                          tuneLength = 10)

save(svm_Linear_floor,file="modelo_svm_linnear_floor.rds")

svm_Linear_floor_pred <-  predict(svm_Linear_floor, validationData[,1:312])
confusionMatrix(svm_Linear_floor_pred, validationData$BF)

#### svn for floor splitted by building ####
#B0
library(kernlab)

Build0training_part <- createDataPartition(Build0training$BF, p= 0.1, list = FALSE)
Build0training_part_0 <- Build0training[Build0training_part,]
svm_Linear_floor_B0 <- train(FLOOR~.,Build0training[,c(1:312,322)],method = "svmLinear", trControl=ctrl,
                             tuneLength = 10)
#Build0training_part_0[,c(1:312,322)], Build0training_part_0$BF,method = "svmLinear", trControl=ctrl,
# tuneLength = 10)

save(svm_Linear_floor_B0, file = "modelo_svm_linear_floor_B0.rds")

svm_Linear_floor_B0_pred <-predict(svm_Linear_floor_B0, Build0validationfloor)
confusionMatrix(svm_Linear_floor_B0_pred, Build0validationfloor$predictbuild)

# SVM con kernel radial
svm_radial_floor_B0 <- train(Build0training_part_0[,c(1:145)], Build0training_part_0$BF,method = "lssvmRadial", trControl=ctrl,
                             tuneLength = 10)

save(svm_radial_floor_B0, file = "modelo_svm_radial_floor_B0.rds")

svm_radial_floor_B0_pred <-predict(svm_radial_floor_B0, Build0validation[,c(1:312,322)])
confusionMatrix(svm_radial_floor_B0_pred, Build0validation$BF)

#b1
Build1training_part <- createDataPartition(Build1training$BF, p= 0.1, list = FALSE)
Build1training_part_1 <- Build1training[Build1training_part,]
svm_Linear_floor_B1 <- train(Build1training_part_1[,c(1:312,322)],Build1training_part_1$BF,method = "svmLinear", trControl=ctrl,
                             tuneLength = 10)

svm_Linear_floor_B1_pred <-predict(svm_Linear_floor_B1, Build1validationfloor$BF )

confusionMatrix(svm_Linear_floor_B1_pred, Build1validationfloor[,c(1:312)])
save(svm_Linear_floor_B1, file = "modelo_svm_linear_floor_B1.rds")

#B2
svm_Linear_floor_B2 <- train(y=Build2training$BF, x=Build2training[,1:312], method = "svmLinear", trControl=ctrl,
                             preProcess = c("center", "scale"), tuneLength = 10)

svm_Linear_floor_B2_pred <-predict (svm_Linear_floor_B2, validationData[which(validationData$BUILDINGID == "2"),c(1:312)])
confusionMatrix(svm_Linear_floor_B2_pred, validationData$BF)
save(svm_Linear_floor_B2, file = "modelo_svm_linear_floor_B2.rds")
#### Errors analysis #

plot()

#### mrls for LAT #### 
LM_train_Latitude <- lm(LATITUDE ~ ., data=df_for_train[,c(1:312,314)])
LM_train_Latitude_pred <- predict(LM_train_Latitude)

postResample(LM_train_Latitude_pred, validationData$LATITUDE)

summary(LM_train_Latitude)
print(LM_train_Latitude)

#### mrls for LAT by B>BF ####
#B0
LM_train_Latitude_B0 <- lm(LATITUDE ~ ., data=Build1training[,c(1:312,314)])
LM_train_Latitude_pred_B0 <- predict(LM_train_Latitude_B0)

postResample(LM_train_Latitude_pred, validationData$LATITUDE)

summary(LM_train_Latitude)
print(LM_train_Latitude)

#B1
LM_train_Latitude_B1 <- lm(LATITUDE ~ ., data=Build1training[,c(1:312,314)])
LM_train_Latitude_pred_B1 <- predict(LM_train_Latitude_B1)

postResample(LM_train_Latitude_pred_B1, validationData$LATITUDE)

summary(LM_train_Latitude_B1)
print(LM_train_Latitude_B1)

#B2
LM_train_Latitude_B2 <- lm(LATITUDE ~ ., data=Build2training[,c(1:312,314)])
LM_train_Latitude_pred_B1 <- predict(LM_train_Latitude_B1)

postResample(LM_train_Latitude_pred_B1, validationData$LATITUDE)

summary(LM_train_Latitude_B1)
print(LM_train_Latitude_B1)



#### mrls for LONGITUDE #### 
LM_train_Longitude <- lm(LONGITUDE ~ ., data=df_for_train,c(1:313))
LM_train_Longitude_pred <- predict(LM_train_Longitude)
postResample(LM_train_Longitude_pred, validationData$LONGITUDE)
summary(LM_train_Longitude)
print(LM_train_Longitude)

