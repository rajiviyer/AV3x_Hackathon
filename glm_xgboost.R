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
DD$year <- ifelse(DD$year>99,DD$year-100,DD$year)
full$DOB <- as.Date(DD)
full$Age <- as.numeric(round((as.Date(Sys.time()) - full$DOB)/365))
full$BMonth <- as.factor(months(full$DOB))
full$BWeekday <- as.factor(weekdays(full$DOB))
full$BYear <- as.factor(format(full$DOB, "%Y"))


DD <- strptime(full$Lead_Creation_Date,"%d-%b-%y")
DD$year <- ifelse(DD$year>99,DD$year-100,DD$year)
full$Lead_Creation_Date <- as.Date(DD)
full$Lead_Creation_Days <- as.numeric(round((as.Date(Sys.time()) - full$Lead_Creation_Date)))

full$Lead_Month <- as.factor(months(full$Lead_Creation_Date))
full$Lead_Weekday <- as.factor(weekdays(full$Lead_Creation_Date))
full$Lead_Year <- as.factor(format(full$Lead_Creation_Date, "%Y"))


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


train <- subset(full,Disbursed != -99)
test <- subset(full,Disbursed == -99)

logreg <- glm(Disbursed~Gender+City_Type+DOB+BWeekday+Lead_Weekday+Lead_Month+Loan_Tenure_Applied+Mobile_Verified+Var5+Var1+Filled_Form+Device_Type+Var4+Age_Class,data=train,family="binomial")
predlogreg <- predict(logreg,type="response",newdata=test)

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
full$BMonth <- as.factor(months(full$DOB))
full$BWeekday <- as.factor(weekdays(full$DOB))
full$BYear <- as.factor(format(full$DOB, "%Y"))


DD <- strptime(full$Lead_Creation_Date,"%d-%b-%y")
DD$year <- ifelse(DD>Sys.time(),DD$year-100,DD$year)
full$Lead_Creation_Date <- as.Date(DD)
full$Lead_Creation_Days <- as.numeric(round((as.Date(Sys.time()) - full$Lead_Creation_Date)))

full$Lead_Month <- as.factor(months(full$Lead_Creation_Date))
full$Lead_Weekday <- as.factor(weekdays(full$Lead_Creation_Date))
full$Lead_Year <- as.factor(format(full$Lead_Creation_Date, "%Y"))


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
#full$DOB <- as.numeric(full$DOB)
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
full$DOB <- NULL
full$Var2 <- NULL
full$Var1 <- NULL
full$Device_Type <- NULL


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

xgbmod <- xgboost(data=mtrain, label=traindisbursed,max.depth=3, eta=1, nround=5,nthread=2, objective="binary:logistic",missing=NaN)
predxgboost <- predict(xgbmod,newdata=mtest,missing=NaN)

pred <- 0.2*predlogreg + 0.8*predxgboost
outfile<-data.frame("ID"=testid,Disbursed=pred)
write.csv(outfile,"glm_xgboost_ensemble.csv",row.names=FALSE)