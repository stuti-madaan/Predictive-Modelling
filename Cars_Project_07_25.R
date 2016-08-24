##Read Data
# 
cars_data = read.csv("E:/R Directory/csv_files/Cars.csv")
cars_X_out_data = read.csv("E:/R Directory/csv_files/Cars_X_out.csv")
# 
# ## Data Processing ##
# 
tempdata1<- cars_data

tempdata1[which(tempdata1$trim=='450'),'trim']<-'unsp'
tempdata1[which(tempdata1$color=='Yellow'),'color']<-'unsp'
tempdata1[which(tempdata1$displacement=='8.0 L'),'displacement']<-'unsp'
tempdata1[which(tempdata1$state %in% c('AK','DC','SD')),'state']<-'unsp'
tempdata1[which(tempdata1$soundSystem %in% c('Alpine','Boston Acoustic')),'soundSystem']<-'unsp'

#removing factors from data
tempdata1$trim<- factor(tempdata1$trim)
tempdata1$color<- factor(tempdata1$color)
tempdata1$displacement<- factor(tempdata1$displacement)
tempdata1$state<- factor(tempdata1$state)
tempdata1$soundSystem<- factor(tempdata1$soundSystem)

#Removing columns X and subTrim
tempdata2<- tempdata1[,-c(1,3)]



## Data Sampling
set.seed(123)
ind = sample(1:nrow(tempdata2),10000)
train=tempdata2[-ind,]
test = tempdata2[ind,]

## Using Random Forest
library(randomForest)
set.seed(2)
pred = randomForest(price~.,data=train, mtry = 4,importance=T,ntree= 500,nodesize= 10 )
pred

yhat<- predict(pred,newdata = test)
sqrt(mean((yhat-test$price)^2))

##Using Boosting
library (gbm)
set.seed (2)
boost.boston =gbm(price~.,data=train, distribution="gaussian", n.trees =3000 , interaction.depth =4,shrinkage= 0.013)

yhat2<- predict(boost.boston,newdata = test,n.trees=3000)

sqrt(mean((yhat2-test$price)^2))

sqrt(mean((((yhat2+yhat)/2)-test$price)^2))

##### Building Final Model on the whole data ####

##Using Random Forest
library(randomForest)
set.seed(2)
pred_final = randomForest(price~.,data=tempdata2, mtry = 4,importance=T,ntree= 300,nodesize =10)
pred_final
save(pred_final,file = "E:/R Directory/RF_final_nochanges_change_para2.rda")

##Using Boosting

library (gbm)
set.seed (1)
boost.final =gbm(price~.,data=tempdata2, distribution="gaussian", n.trees =3000 , interaction.depth =4,shrinkage= 0.013)
boost.final
save(boost.final,file = "E:/R Directory/Boost_final_nochanges_change_para.rda")

##Processing test Data
test_data = cars_X_out_data

#Removing columns X and subTrim
test_data2<- test_data[,-c(3)]
test_data2$state<- as.character(test_data2$state)
test_data2[which(test_data2$state=='VT'),'state']<- 'unsp'
test_data2$state<- as.factor(test_data2$state)


##Predictions on Test
pred_final2 = load(file = "E:/R Directory/Boost_final_nochanges_change_para.rda")
boost.final2 = load(file = "E:/R Directory/Boost_final_nochanges_change_para.rda")

#correction required for RF##
# throws the error: 
# Error in predict.randomForest(pred_final, newdata = test_data2, mtry = 4) : 
#New factor levels not present in the training data

#levels(test_data2$color)<- levels(tempdata2$color)
#levels(test_data2$displacement)<- levels(tempdata2$displacement)
#levels(test_data2$fuel)<- levels(tempdata2$fuel)
#levels(test_data2$region)<- levels(tempdata2$region)
#levels(test_data2$wheelType)<- levels(tempdata2$wheelType)
#levels(test_data2$wheelSize)<- levels(tempdata2$wheelSize)

## Estimate of Random Forest
yhat<- predict(pred_final,newdata = test_data2,mtry=4)

## Estimate of GBM
yhat2<- predict(get(boost.final2),newdata = test_data2,n.trees=3000,type="response")

# BUild Esemble of Random Forest and Gradient Boosting
final_prediction = ((yhat2+yhat)/2)

test_data2$rf_op<- yhat
write.csv(test_data2,"E:/R Directory/csv_files/model_predict_test.csv")
# import carlos's file
Actual_price = read.table("E:/R Directory/csv_files/Cars_Price2_out.txt")
Predicted_price = read.csv("E:/R Directory/csv_files/model_predict_test.csv",header=T)


#Actual Prices to be provided by Carlos
RMSE= sqrt(mean((Predicted_price$rf_op-Actual_price)^2))
