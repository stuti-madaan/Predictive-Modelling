##Read Data
cars_data = read.csv("E:/R Directory/csv_files/Cars.csv")
cars_X_out_data = read.csv("E:/R Directory/csv_files/Cars_X_out.csv")


#Data Processing ##

tempdata1<- cars_data

#Replace all 'unsp' values by NA
tempdata1[tempdata1=="unsp"]<-NA

#Substitute missing values by most frequenct observations 
tempdata1[which(is.na(tempdata1$color)),'color']<- 'Black'
tempdata1[which(is.na(tempdata1$displacement)),'displacement']<- '6.0 L'
tempdata1[which(is.na(tempdata1$fuel)),'fuel']<- 'Gasoline'
tempdata1[which(is.na(tempdata1$state)),'state']<- 'CA'
tempdata1[which(is.na(tempdata1$region)),'region']<- 'SoA'
tempdata1[which(is.na(tempdata1$trim)),'trim']<- 550    

#Removing columns X and subTrim
tempdata2<- tempdata1[,-c(1,3)]

#Convert Year to factor
tempdata2[,5]<-as.factor(tempdata2[,5])

#more than 50% missing values substituted back to 'unsp'
tempdata2[which(is.na(tempdata2$soundSystem)),'soundSystem']<- 'unsp'
tempdata2[which(is.na(tempdata2$wheelSize)),'wheelSize']<- 'unsp'
tempdata2[which(is.na(tempdata2$wheelType)),'wheelType']<- 'unsp'
tempdata2[which(tempdata2$mileage>= 177995.6),'mileage']<- 177996 

set.seed(123)
ind = sample(1:nrow(tempdata2),10000)
train=tempdata2[-ind,]
test = tempdata2[ind,]

##Using Random Forest
library(randomForest)
pred = randomForest(price~trim+isOneOwner+color+fuel+region+soundSystem+state+
                      wheelType+wheelSize+featureCount+condition+year+mileage+
                      displacement,data=train, mtry = 4,importance=T,ntree= 500)
pred

yhat<- predict(pred,newdata = test)
sqrt(mean((yhat-test$price)^2))


pred2= randomForest(price~year+mileage+condition+displacement+trim+soundSystem+featureCount+state , data=train,mtry=3,ntree=500)
yhat2<- predict(pred2,newdata = test)
sqrt(mean((yhat2-test$price)^2))
