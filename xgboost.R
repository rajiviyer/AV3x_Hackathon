library(xgboost)
library(party)

setwd("C:/Rajiv/R/bank")
train = read.csv("train.csv")
test = read.csv("test.csv")
test$Disbursed <- -99
test$LoggedIn <- -99
full <- rbind(train,test)
full$City <- as.character(full$City)

DD <- strptime(full$DOB,"%d-%b-%y")
DD$year <- ifelse(DD>Sys.time(),DD$year-100,DD$year)
full$DOB <- as.Date(DD)
full$Age <- as.numeric(round((as.Date(Sys.time()) - full$DOB)/365))
full$BMonth <- months(full$DOB)
full$BWeekday <- weekdays(full$DOB)
full$BYear <- format(full$DOB, "%Y")


DD <- strptime(full$Lead_Creation_Date,"%d-%b-%y")
DD$year <- ifelse(DD>Sys.time(),DD$year-100,DD$year)
full$Lead_Creation_Date <- as.Date(DD)
full$Lead_Creation_Days <- as.numeric(round((as.Date(Sys.time()) - full$Lead_Creation_Date)))

full$Lead_Month <- months(full$Lead_Creation_Date)
full$Lead_Weekday <- weekdays(full$Lead_Creation_Date)
full$Lead_Year <- format(full$Lead_Creation_Date, "%Y")


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
#full$Employer_Name <- NULL

train <- subset(full,Disbursed != -99)
test <- subset(full,Disbursed == -99)

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

xgbcv <- xgb.cv(data=mtrain, label=traindisbursed,max.depth=5, eta=0.01,nrounds=500,nfold=4,objective="binary:logistic",showsd=T,metrics=list('auc','rmse'),stratified = T,verbose=T,subsample=0.7,missing=NaN)

xgbmod <- xgboost(data=mtrain, label=traindisbursed,max.depth=5, eta=0.01, nround=500,nfold=4,nthread=2, objective="binary:logistic",missing=NaN)
pred <- predict(xgbmod,newdata=mtest,missing=NaN)

outfile<-data.frame("ID"=testid,Disbursed=pred)
write.csv(outfile,"xgboost.csv",row.names=FALSE)

