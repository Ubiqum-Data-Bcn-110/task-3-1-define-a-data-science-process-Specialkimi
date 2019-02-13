####instalpackages#### 

#install.packages("rattle")
#install.packages("corrplot")
#install.packages("caret")
#install.packages("arules")
#install.packages("matrix")
library(corrplot)
library(caret)
library(dplyr)
library(arules)
library(Matrix)

####loadingDF####
setwd("C:/Users/User/Desktop/tasks/M4T1")
load("/Users/jobtoday/Desktop/tasks /M4T1/galaxymatrix_all.Rdata")
load("/Users/jobtoday/Desktop/tasks /M4T1/iphonematrix_all.Rdata")

####joinning in one DF ####
newgalaxy <- galaxymatrix_all
newiphone <- iphonematrix_all

final_matrix <- cbind(newgalaxy, newiphone$iphoneSentiment)
"iphonesentiment"-> colnames(final_matrix)[61] 

####CORRmatrixandhist####
corrplot(cor(final_matrix), order ="hclust")
plot(final_matrix$`newiphone$iphoneSentiment`)
plot(final_matrix$iphonecampos)
plot(final_matrix$samsungcampos)

####fitcontrol####
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     )

set.seed(123)
mod <- train(newiphone$iphoneSentiment ~ ., data = final_matrix,
             method = "knn",
             tuneLength = 12,
             trControl = ctrl)

importance <- varImp(mod, scale=FALSE)
print(importance)
plot(importance)


####correlation####
descrCor <- cor(final_matrix)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .80) 
str(highlyCorDescr)
plot(highlyCorDescr)


####newdf no 80####
newfinalmatrix <- final_matrix[,-highlyCorDescr]

####preprocess large iphone ####
disfixed7 <- discretize(newfinalmatrix$iphonesentiment, "fixed", 
                        categories= c(-Inf, -50, -10, -1, 1, 10, 50, Inf)   , 
                        labels = c("Very Negative", "Negative",
                                                                                         "Somewhat Negative", "Neutral",
                                                                                         "Somewhat Positive", "Positive",
                                                                                         "Very Positive"))



str(disfixed7)
summary(disfixed7)

plot(disfixed7)
newfinalmatrix$iphonesentiment <- disfixed7  

####removing zero var atributes #####
#newfinalmatrix_iphone <- newfinalmatrix[,-c(4,10,17,23)]
#newfinalmatrix_iphone <- newfinalmatrix_iphone[,-c(8,9,22)]
#newfinalmatrix_iphone <- newfinalmatrix_iphone[,-c(16,17,20)]


set.seed(123)

intrainmatrix_iphone <- createDataPartition(newfinalmatrix$iphonesentiment, p= 0.2, list = FALSE)
intrainmatrix_galaxy <- createDataPartition(newfinalmatrix$galaxySentiment, p= 0.2, list = FALSE)

####svm for iphone####
finalmatrix_train_iphone <- newfinalmatrix[intrainmatrix_iphone,]
finalmatrix_test_iphone <- newfinalmatrix[-intrainmatrix_iphone,]



ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")

finalmatrix_train_iphone <- finalmatrix_train_iphone[,-c(1,2,4,6,7,8,9,10,11,12,13,14,16,17,18,19,20,22,23,24,25,27)]

finalmatrix_test_iphone <- finalmatrix_test_iphone[,-c(1,2,4,6,7,8,9,10,11,12,13,14,16,17,18,19,20,22,23,24,25,27))]

svm_newfinalmatrix_train_1 <- train(y=finalmatrix_train_iphone$iphonesentiment, x= finalmatrix_train_iphone[,c(1:6)], method = "svmLinear", trControl=ctrl,
                             preProcess = c("center", "scale"), tuneLength = 10)

svm_newfinalmatrix_train_1_pred <-predict(svm_newfinalmatrix_train_1, finalmatrix_test_iphone[,c((1:16)])

plot(svm_newfinalmatrix_train_1_pred)


####svm for galaxy####
set.seed(123)

#intrainmatrix_iphone <- createDataPartition(newfinalmatrix$iphonesentiment, p= 0.1, list = FALSE)
#intrainmatrix_galaxy <- createDataPartition(newfinalmatrix$galaxySentiment, p= 0.1, list = FALSE)

#finalmatrix_train_iphone <- newfinalmatrix[intrainmatrix_iphone,]
#finalmatrix_test_iphone <- newfinalmatrix[-intrainmatrix_iphone,]

#finalmatrix_train_galaxy <- newfinalmatrix[intrainmatrix_galaxy,]
#finalmatrix_test_galaxy <- newfinalmatrix[-intrainmatrix_galaxy,]

finalmatrix_train_galaxy <- newfinalmatrix[intrainmatrix_galaxy,]
finalmatrix_test_galaxy <- newfinalmatrix[-intrainmatrix_galaxy,]

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")
####svm for galaxy ####

svm_newfinalmatrix_train_1_galaxy <- train(y=finalmatrix_train_galaxy$galaxySentiment, x= finalmatrix_train_galaxy[,c(1,2,6,9,8,12,14,16,18,20,24,25,27,30)], method = "svmLinear", trControl=ctrl,
                                    preProcess = c("center", "scale"), tuneLength = 10)

svm_newfinalmatrix_train_1_pred_galaxy <-predict(svm_newfinalmatrix_train_1_galaxy, finalmatrix_test_galaxy[,c(1,2,6,9,8,12,14,16,18,20,24,25,27,30)])

plot(svm_newfinalmatrix_train_1_pred_galaxy)




####knn####
LM_final_matrix <- lm(newfinalmatrix$iphonesentiment ~ ., data = newfinalmatrix[,c(1,3,4,5,7,9,10,11,13,15,17, 19,21,22,23,26,28,29)])
LM_train_Longitude_B0_pred_val <- predict(LM_final_matrix , Build0validation_NEW[,c(1:139)])

metricas_error_lon_B0_val_lm <- postResample(LM_train_Longitude_B0_pred_val, Build0validation_NEW[,140])
hist(LM_train_Longitude_B0_pred_val-Build0validation_NEW[,140],
     xlab = "Errores del modelo",
     ylab = "Frequencia",
     main = "Longitud en Building B0, validation set")


summary(LM_train_Longitude_B0_NEW)
print(LM_train_Longitude_B0_NEW)


