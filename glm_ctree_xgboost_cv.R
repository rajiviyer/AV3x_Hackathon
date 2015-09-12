library(xgboost)
library(party)
library(caTools)
library(ROCR)

setwd("C:/Rajiv/R/bank")
train = read.csv("train.csv")

split = sample.split(train$Disbursed,SplitRatio=0.7)
traincv = subset(train,split==TRUE)
testcv = subset(train,split==FALSE)

train = traincv
train$type <- "train"
test = testcv
test$type <- "test"
full <- rbind(train,test)
full$City <- as.character(full$City)

DD <- strptime(full$DOB,"%d-%b-%y")
DD$year <- ifelse(DD>Sys.time(),DD$year-100,DD$year)
full$DOB <- as.Date(DD)
full$Age <- as.numeric(round((as.Date(Sys.time()) - full$DOB)/365))

DD <- strptime(full$Lead_Creation_Date,"%d-%b-%y")
DD$year <- ifelse(DD>Sys.time(),DD$year-100,DD$year)
full$Lead_Creation_Date <- as.Date(DD)
full$Lead_Creation_Days <- as.numeric(round((as.Date(Sys.time()) - full$Lead_Creation_Date)))
full$City_Type <- "Z"
full[full$City %in% c("Bangalore","Hyderabad","Delhi","Bengaluru","Mumbai","Chennai","Kolkata"),]$City_Type <- "X"
full[full$City %in% c("Vijayawada","Warangal","Vishakhapatnam","Guntur","Guwahati","Patna","Chandigarh","Durg","Bhilai","Raipur","Ahmedabad","Rajkot","Jamnagar","Vadodara","Surat","Faridabad","Srinagar","Jammu","Jamshedpur","Dhanbad","Ranchi","Belgaum","Hubli","Dharwad","Mangalore","Mysore","Kozhikode","Kochi","Thiruvananthapuram","Gwalior","Indore","Bhopal","Jabalpur","Amravati","Nagpur","Aurangabad","Nashik","Bhiwandi","Pune","Solapur","Kolhapur","Cuttack","Bhubaneswar","Amritsar","Jalandhar","Ludhiana","Pondicherry","Bikaner","Jaipur","Jodhpur","Kota","Salem","Tiruppur","Coimbatore","Tiruchirappalli","Madurai","Moradabad","Meerut","Ghaziabad","Aligarh","Agra","Bareilly","Lucknow","Kanpur","Allahabad","Gorakhpur","Varanasi","Dehradun","Asansol"),]$City_Type <- "Y"
full$City_Type = as.factor(full$City_Type)
full$Source <- as.factor(as.character(full$Source))

full$Age_Class <- "E"
full[full$Age>0 & full$Age <=24,]$Age_Class <- "A"
full[full$Age>=25 & full$Age <=40,]$Age_Class <- "B"
full[full$Age>40 & full$Age <=55,]$Age_Class <- "C"
full[full$Age>55 & full$Age <=70,]$Age_Class <- "D"
full$Age_Class <- as.factor(full$Age_Class)

train <- full[full$type=="train",]
test <- full[full$type=="test",]

logreg <- glm(Disbursed~Gender+City_Type+DOB+Loan_Tenure_Applied+Mobile_Verified+Var5+Var1+Filled_Form+Device_Type+Var4+Age_Class,data=train,family="binomial")
train$Disbursed <- as.factor(train$Disbursed)

ctreemod <- ctree(Disbursed~Gender+City_Type+Loan_Tenure_Applied+Mobile_Verified+Var5+Var1+Filled_Form+Device_Type+Var4,data=train)
predlogreg <- predict(logreg,type="response",newdata=test)
predctree <- predict(ctreemod,type="prob",newdata=test)


train = traincv
train$type <- "train"
test = testcv
test$type <- "test"

full <- rbind(train,test)
full$City <- as.character(full$City)

DD <- strptime(full$DOB,"%d-%b-%y")
DD$year <- ifelse(DD>Sys.time(),DD$year-100,DD$year)
full$DOB <- as.Date(DD)
full$Age <- as.numeric(round((as.Date(Sys.time()) - full$DOB)/365))

DD <- strptime(full$Lead_Creation_Date,"%d-%b-%y")
DD$year <- ifelse(DD>Sys.time(),DD$year-100,DD$year)
full$Lead_Creation_Date <- as.Date(DD)
full$Lead_Creation_Days <- as.numeric(round((as.Date(Sys.time()) - full$Lead_Creation_Date)))
full$City_Type <- 3
full[full$City %in% c("Bangalore","Hyderabad","Delhi","Bengaluru","Mumbai","Chennai","Kolkata"),]$City_Type <- 1
full[full$City %in% c("Vijayawada","Warangal","Vishakhapatnam","Guntur","Guwahati","Patna","Chandigarh","Durg","Bhilai","Raipur","Ahmedabad","Rajkot","Jamnagar","Vadodara","Surat","Faridabad","Srinagar","Jammu","Jamshedpur","Dhanbad","Ranchi","Belgaum","Hubli","Dharwad","Mangalore","Mysore","Kozhikode","Kochi","Thiruvananthapuram","Gwalior","Indore","Bhopal","Jabalpur","Amravati","Nagpur","Aurangabad","Nashik","Bhiwandi","Pune","Solapur","Kolhapur","Cuttack","Bhubaneswar","Amritsar","Jalandhar","Ludhiana","Pondicherry","Bikaner","Jaipur","Jodhpur","Kota","Salem","Tiruppur","Coimbatore","Tiruchirappalli","Madurai","Moradabad","Meerut","Ghaziabad","Aligarh","Agra","Bareilly","Lucknow","Kanpur","Allahabad","Gorakhpur","Varanasi","Dehradun","Asansol"),]$City_Type <- 2
full$Source <- as.factor(as.character(full$Source))

full$Age_Class <- 5
full[full$Age>0 & full$Age <=24,]$Age_Class <- 1
full[full$Age>=25 & full$Age <=40,]$Age_Class <- 2
full[full$Age>40 & full$Age <=55,]$Age_Class <- 3
full[full$Age>55 & full$Age <=70,]$Age_Class <- 4

full$Gender <- as.numeric(full$Gender)
full$DOB <- as.numeric(full$DOB)
full$Lead_Creation_Date <- as.numeric(full$Lead_Creation_Date)
full$Salary_Account <- as.numeric(full$Salary_Account)
full$Mobile_Verified <- as.numeric(full$Mobile_Verified)
full$Var1 <- as.numeric(full$Var1)
full$Filled_Form <- as.numeric(full$Filled_Form)
full$Device_Type <- as.numeric(full$Device_Type)
full$Var2 <- as.numeric(full$Var2)
full$Source <- as.numeric(full$Source)
full[full$City=="",]$City <- "Others"
full$City <- as.numeric(as.factor(full$City))
full$Employer_Name <- NULL

train <- full[full$type=="train",]
test <- full[full$type=="test",]

traindisbursed <- train$Disbursed
train$ID <- NULL
train$LoggedIn <- NULL
train$Disbursed <- NULL

testid <- test$ID
test$ID <- NULL
test$LoggedIn <- NULL
test$Disbursed <- NULL
mtest <- data.matrix(test)



mtrain <- data.matrix(train)

xgbmod <- xgboost(data=mtrain, label=traindisbursed,max.depth=3, eta=1, nround=5,nthread=2, objective="binary:logistic",missing=NaN)
predxgboost <- predict(xgbmod,newdata=mtest,missing=NaN)


#pred <- (predlogreg + sapply(predctree,"[[",2) + predxgboost)/3
pred <- 0.3*predlogreg + 0.3*sapply(predctree,"[[",2) + 0.4*predxgboost
#pred <- 0.3*predlogreg + 0.7*predxgboost

ROCRPred <- prediction(pred,testcv$Disbursed)
auc <- as.numeric(performance(ROCRPred,"auc")@y.values)
