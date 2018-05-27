library(Boruta)
library(rFerns)
library(ranger)
library(dplyr)
install.packages("fpp")
library(fpp)

library(randomForest)

RetailDataOrig<-read.csv(file.choose())
Retaildata<-RetailDataOrig

#EDA on the dataset
head(Retaildata)
tail(Retaildata)
str(Retaildata)
summary(Retaildata)

#postal code has 41296 missing values >85%, Removing the column
Retaildata$postal_code<-NULL
#Removing Row_id as it is insignificant
Retaildata$row_id<-NULL
#Removing Order ID as we have the order prefix extracted as a seperate feature in the dataset
Retaildata$order_id<-NULL
#Removing Customer_name as we have uniquely identified customers on the feature customer_id
Retaildata$customer_name<-NULL
#Removing Product_name as we have uniquely identified customers on the feature product_id
Retaildata$product_name<-NULL
#Renaming some columns for easy column accessing
colnames(Retaildata)
colnames(Retaildata)[21]="ShippingDays"
colnames(Retaildata)[22]="Export_Dom"
colnames(Retaildata)[23]="PerUnitSP" #Per Unit Sales Price
colnames(Retaildata)[24]="NetSalesWithReturn" #Net Sales = Sales - Return
colnames(Retaildata)[25]="PerUnitNSP" #Per unit Net Sales Price
colnames(Retaildata)[26]="NetDiscountedPrice" #NetDiscountedPrice = NetSalesWithReturn * Discount
colnames(Retaildata)[27]="NetDiscountPerUnit" #NetDiscountPerUnit = PerUnitNSP * Discount
#Rest of the data doesnt have missing values 

#converting values to profit loss factors
Profit_Loss=c() #Declaring a list containing the factors,currently an empty list.
Retaildata$Actual.Profit
for(i in 1:length(Retaildata$Actual.Profit)) {
  if(Retaildata$Actual.Profit[i]<0) { Profit_Loss[i]="Loss"} else {Profit_Loss[i]="Profit"} 
}
Retaildata$Profit_Loss<-as.factor(Profit_Loss)

#checking integrity of the newly created list
head(Profit_Loss)
head(Retaildata$Actual.Profit)
tail(Profit_Loss)
tail(Retaildata$Actual.Profit)
Retaildata$Profit_Loss<-as.factor(Profit_Loss)

#Random Forest Trial Run
#Checking for importance of variables
bestmtry<-tuneRF(Retaildata,Retaildata$Profit_Loss,stepFactor = 1.2,improve=0.01,trace = T,plot = T)
str(bestmtry)
#Error in randomForest.default(x, y, mtry = mtryStart, ntree = ntreeTry,  : 
#Can not handle categorical predictors with more than 53 categories.
#Some of our current categorical variables have more than 53 categories
#Removing few categories that have more than 53 categories

summary(Retaildata)
RetailDataRFTest<-Retaildata[,c(3,5,10,12,16,18:23,27:31,32)]
bestmtry<-tuneRF(RetailDataRFTest,RetailDataRFTest$Profit_Loss,stepFactor = 1.2,improve=0.01,trace = T,plot = T)
str(bestmtry)

model1<-randomForest(RetailDataRFTest$Profit_Loss~.,data=RetailDataRF,tree=1000,mtry=4,importance=TRUE,proximity=TRUE)
#Model cannot run as  Its not able to allocate space of 19 GB on RAM
#> model1<-randomForest(RetailDataRF$Profit_Loss~.,data=RetailDataRF,tree=1000,mtry=4,importance=TRUE,proximity=TRUE)
#Error: cannot allocate vector of size 19.6 Gb


#Reducing the size of the dataframe by removing correlated variables
summary(RetailDataRF)
RetailDataNonFact<-RetailDataRF
RetailDataNonFact$ship_mode<-RetailDataRF$ship_mode
#converting Ship_mode to Numerical
for(i in 1:length(RetailDataNonFact$ship_mode)) {
  if(RetailDataNonFact$ship_mode[i]=="First Class") { RetailDataNonFact$shipmodeVal[i]="1"} 
  else if(RetailDataNonFact$ship_mode[i]=="Second Class") { RetailDataNonFact$shipmodeVal[i]="2"}
  else if(RetailDataNonFact$ship_mode[i]=="Standard Class") { RetailDataNonFact$shipmodeVal[i]="3"}
  else {RetailDataNonFact$shipmodeVal[i]="4"}
}
#Remove Factor ShipMode as it is converted to numeric
RetailDataNonFact$ship_mode<-NULL
str(RetailDataNonFact)
summary(RetailDataNonFact)
RetailDataNonFact$shipmodeVal<-as.numeric(RetailDataNonFact$shipmodeVal)


#Convert Segment to Values
for(i in 1:length(RetailDataNonFact$segment)) {
  if(RetailDataNonFact$segment[i]=="Consumer") { RetailDataNonFact$segmentVal[i]="1"} 
  else if(RetailDataNonFact$segment[i]=="Corporate") { RetailDataNonFact$segmentVal[i]="2"}
  else {RetailDataNonFact$segmentVal[i]="3"}
}
RetailDataNonFact$segmentVal<-as.numeric(RetailDataNonFact$segmentVal)
typeof(RetailDataNonFact$segmentVal)
is.factor(RetailDataNonFact$segment)
is.factor(as.factor(RetailDataNonFact$segmentVal))

#As the file s too large for the computer handle Random Forest.
#Breaking the dataset into different parts
RetailDataRFTest

set.seed(555)
ind <- sample(2, nrow(RetailDataRFTest),
              replace = TRUE,
              prob = c(0.5, 0.5))
Partition1 <- RetailDataRFTest[ind==1,]
Partition2 <- RetailDataRFTest[ind==2,]

#Partitioning again
ind <- sample(2, nrow(Partition1),
              replace = TRUE,
              prob = c(0.5, 0.5))
Partition3<-Partition1[ind==1,]
Partition4<-Partition1[ind==2,]

ind <- sample(2, nrow(Partition2),
              replace = TRUE,
              prob = c(0.5, 0.5))
Partition5<-Partition2[ind==1,]
Partition6<-Partition2[ind==2,]
str(Partition3)

#Running on first 25% 
model1<-randomForest(Partition3$Profit_Loss~.,data=Partition3,tree=1000,mtry=4,
                     importance=TRUE,proximity=TRUE)
summary(model1)
print(model1)
#To check importance of each feature
round(importance(model1),2)
#Plot each feature importace
varImpPlot(model1)

#Running on second 25% 
model2<-randomForest(Partition4$Profit_Loss~.-Partition4$Actual.Profit,data=Partition4,tree=1000,mtry=4,
                     importance=TRUE,proximity=TRUE)
summary(model2)
print(model2)
#To check importance of each feature
round(importance(model2),2)
#Plot each feature importace
varImpPlot(model2)


RegTestDataOrig<-read.csv(file.choose())
str(RegTestDataOrig)
set.seed(555)
ind <- sample(2, nrow(RegTestDataOrig),
              replace = TRUE,
              prob = c(0.5, 0.5))
RegPartition1 <- RegTestDataOrig[ind==1,]
RegPartition2 <- RegTestDataOrig[ind==2,]

#Partitioning again
ind <- sample(2, nrow(RegPartition1),
              replace = TRUE,
              prob = c(0.5, 0.5))
RegPartition3<-RegPartition1[ind==1,]
RegPartition4<-RegPartition1[ind==2,]

ind <- sample(2, nrow(RegPartition2),
              replace = TRUE,
              prob = c(0.5, 0.5))
RegPartition5<-RegPartition2[ind==1,]
RegPartition6<-RegPartition2[ind==2,]
str(RegPartition3)



###Changing time format
typeof(RegPartitionAcc$ship_date)


RegPartitionAcc$ship_date<-as.Date(RegPartitionAcc$ship_date)
?strptime

#Date_Formatting
library(scales)
library(lubridate)
RegPartitionAcc$ship_date<-as.Date(format(as.Date(RegPartitionAcc[,c(4)],"%d-%m-%Y", "%Y-%m-%d")))
typeof(RegPartitionAcc)
my_date_format <- function()
{
  function(x)
  {
    m <- format(x,"%b")
    y <- format(x,"%Y")
    ifelse(duplicated(y),m,paste(m,y))
  }
}

#Partitioning data and running model according to sub-categories
PlotForDays<-function(df) {     #Plot for Days
  plot(df$Actual.Profit,type = "l")
  p<-ggplot(data = df,
            aes(df$ship_date, df$Actual.Profit)) +
    stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "line") +  # or "line"
    scale_x_date(
      labels = date_format("%Y-%m"),
      date_breaks = "year") +
    theme(axis.text.x=element_text(angle=90, hjust=1))
  p + theme(axis.text.x = element_text(angle = 60, hjust = 1))
  p+labs(x = "Year/Month",y="Profit/Loss")
}

RegModel<-function(SubCat) {    #Regression model
RegTestDf<-RegTestDataOrig[RegTestDataOrig$sub_category==SubCat,] #partitioning data
RegTestDf$ship_date<-as.Date(format(as.Date(RegTestDf[,c(4)],"%d-%m-%Y", "%Y-%m-%d")))
DfModel<-lm(RegTestDf$Actual.Profit~+RegTestDf$Order.Prefix+RegTestDf$Month.of.Shipment+RegTestDf$discount+RegTestDf$order_priority+RegTestDf$region,RegTestDf)
print(summary(DfModel))
PlotForDays(RegTestDf)
}

plot(DfModel)


df=data.frame()
###Aggregate by days
#SalesOverMonths<- function(SubCat) {
#df<-RegTestDataOrig[RegTestDataOrig$sub_category==SubCat,] #partitioning data
#bymonth <- aggregate(cbind(df$Actual.Profit)~month(df$ship_date)+year(df$ship_date),
 #                    data=df,FUN=sum)
#bymonthTS<-ts(bymonth$V1,frequency=12,start = c(2012,1),end= c(2016,1))
#print(bymonthTS)
#}
#install.packages("anytime")
SalesOverYears<- function(SubCat) {
  df<-RegTestDataOrig[RegTestDataOrig$sub_category==SubCat,]#partitioning data
  bymonth <- aggregate(cbind(df$Actual.Profit)~month(df$ship_date)+year(as.Date(df$ship_date,format="%d-%m-%Y")),data=df,FUN=sum)
  bymonthTS<-ts(bymonth$V1,frequency=12,start = c(2012,1),end= c(2015,12))
  print(bymonthTS)
  plot(diff(bymonthTS))               #Graph over the years
  abline(reg=lm(bymonthTS~time(bymonthTS)))
  plot(aggregate(bymonthTS,FUN=mean))
  #boxplot(bymonthTS~cycle(bymonthTS))#Check Seasonality
  #adf.test(bymonthTS,nlag = NULL)
  #acf(bymonthTS)
  #pacf(bymonthTS)
  #fit <- arima(bymonthTS, c(0,0,1), seasonal = list(order=c(0,0,1), period=12))
  #pred1 <- predict(fit, n.ahead=2*12)
  #pred1
  #ts.plot(bymonthTS,pred1)
}

nrow(RegTestDataOrig)

#########Profit_Loss Plots
##Shows the Linear Regression Model Summary of Each Sub-Category and the Sales trends for days /years

RegModel("Accessories")
SalesOverYears("Accessories")
RegModel("Appliances")
SalesOverYears("Appliances")
RegModel("Art")
SalesOverYears("Art")
RegModel("Binders")
SalesOverYears("Binders")
RegModel("Bookcases")
SalesOverYears("Bookcases")
RegModel("Chairs")
SalesOverYears("Chairs")
RegModel("Copiers")
SalesOverYears("Copiers")
RegModel("Envelopes")
SalesOverYears("Envelopes")
RegModel("Fasteners")
SalesOverYears("Fasteners")
RegModel("Furnishings")
SalesOverYears("Furnishings")
RegModel("Labels")
SalesOverYears("Labels")
RegModel("Machines")
SalesOverYears("Machines")
RegModel("Paper")
SalesOverYears("Paper")
RegModel("Phones")
SalesOverYears("Phones")
RegModel("Storage")
SalesOverYears("Storage")
RegModel("Supplies")
SalesOverYears("Supplies")
RegModel("Tables")
SalesOverYears("Tables")

#################################
######     MARKET WISE
#################################

MarketRegModel<-function(SubCat) {    #Regression model
  RegTestDf<-RegTestDataOrig[RegTestDataOrig$market==SubCat,] #partitioning data
  RegTestDf$ship_date<-as.Date(format(as.Date(RegTestDf[,c(4)],"%d-%m-%Y", "%Y-%m-%d")))
  DfModel<-lm(RegTestDf$Actual.Profit~+RegTestDf$Order.Prefix+RegTestDf$Month.of.Shipment+RegTestDf$discount+RegTestDf$order_priority+RegTestDf$region+RegTestDf$sub_category,RegTestDf)
  print(summary(DfModel))
  PlotForDays(RegTestDf)
}

MarketSalesOverYears<- function(SubCat) {
  df<-RegTestDataOrig[RegTestDataOrig$market=="Africa",]#partitioning data
  bymonth <- aggregate(cbind(df$Actual.Profit)~month(df$ship_date)+year(as.Date(df$ship_date,format="%d-%m-%Y")),data=df,FUN=sum)
  bymonthTS<-ts(bymonth$V1,frequency=12,start = c(2012,1),end= c(2015,12))
  print(bymonthTS)
  plot(diff(bymonthTS))               #Graph over the years
  abline(reg=lm(bymonthTS~time(bymonthTS)))
  plot(aggregate(bymonthTS,FUN=mean))
  
  #TimeSeries
  
  #boxplot(bymonthTS~cycle(bymonthTS))#Check Seasonality
  #adf.test(bymonthTS,nlag = NULL)
  #acf(bymonthTS)
  #pacf(bymonthTS)
  #fit <- arima(bymonthTS, c(0,0,1), seasonal = list(order=c(0,0,1), period=12))
  #pred1 <- predict(fit, n.ahead=2*12)
  #pred1
  #ts.plot(bymonthTS,pred1)
}

MarketRegModel("Africa")
MarketSalesOverYears("Africa")
RegModel("Asia Pacific")
MarketSalesOverYears("Asia Pacific")
RegModel("Europe")
MarketSalesOverYears("Europe")
RegModel("LATAM")
MarketSalesOverYears("LATAM")
RegModel("USCA")
MarketSalesOverYears("USCA")
