##Installing and Loading Required Packages
install.packages("RJSONIO")
install.packages("RCurl")
install.packages("sqldf")
install.packages("mice")
install.packages("VIM")
library(RJSONIO)
library(RCurl)
library(plyr)
library(stringr)
library(utils)
library(sqldf)
library(mice)
library(VIM)
library(lattice)
library(ggplot2)

##Getting Data in Json Format

raw_data <- getURL("http://ist.gmu.edu/~hpurohit/courses/ait582-proj-data-spring16.json")

##converting Data to CSV format

data <- fromJSON(raw_data)
length(data)
raw_data <- do.call(rbind, data)
raw_data=data.frame(raw_data,row.names = NULL)

##writing it to a csv file

write.csv(final_data, "raw_data.csv")

##Extracting Metadata from DESCRIPTION and assigning to AGE & GENDER

head(raw_data)
data=raw_data[-1,]
head(data)
rownames(data) <- 1:nrow(data)
splitdat = do.call("rbind", strsplit(as.character(data$DESCRIPTION), ";"))
GENDER=str_extract(string =data$DESC,pattern = "(Mr|Miss|Mrs|Master)\\.")
mydata=cbind(data,GENDER,splitdat)
mydata[[9]] <- as.numeric(as.character(mydata[[9]]))
mydata=rename(mydata, c("1"="DESC","2"="AGE"))
mydata$DESCRIPTION=NULL
mydata$DESC=NULL
head(mydata)
sum(is.na(mydata$GENDER))

##Removing data whose GENDER is N/A

mydata=subset(mydata,!is.na(GENDER))
rownames(mydata) <- 1:nrow(mydata)

##Generating dataframe

Mr=sqldf("select * from mydata where GENDER='Mr.'")
Mrs=sqldf("select * from mydata where GENDER='Mrs.'")
Master=sqldf("select * from mydata where GENDER='Master.'")
Miss=sqldf("select * from mydata where GENDER='Miss.'")
mydata=sqldf("select * from Mr union select * from Mrs union select * from Master union select * from Miss")

##Missing AGE imputation by Predictive Mean Matching(pmm) Seperatly for each cateogry using SQL
#For Mr

Mr=sqldf("select * from mydata where GENDER='Mr.'")
temp_Mr=mice(Mr,m=1,method = 'pmm')
temp_Mr$imp$AGE
Mr=complete(temp_Mr,1)
Mr=sqldf(c("update Mr set GENDER='MALE'","select * from Mr"))

#For Mrs

Mrs=sqldf("select * from mydata where GENDER='Mrs.'")
temp_Mrs=mice(Mrs,m=1,method = 'pmm')
temp_Mrs$imp$AGE
Mrs=complete(temp_Mrs,1)
Mrs=sqldf(c("update Mrs set GENDER='FEMALE'","select * from Mrs"))

#For Master

Master=sqldf("select * from mydata where GENDER='Master.'")
temp_Master=mice(Master,m=1,method = 'pmm')
temp_Master$imp$AGE
Master=complete(temp_Master,1)
Master=sqldf(c("update Master set GENDER='MALE'","select * from Master"))

#For Miss

Miss=sqldf("select * from mydata where GENDER='Miss.'")
temp_Miss=mice(Miss,m=1,method = 'pmm')
temp_Miss$imp$AGE
Miss=complete(temp_Miss,1)
Miss=sqldf(c("update Miss set GENDER='FEMALE'","select * from Miss"))

#Combining all data

mydata=sqldf("select * from Mr union select * from Mrs union select * from Master union select * from Miss")
write.csv(mydata, "mydata.csv")

#Writing data to load in weka&Tablue
mydata=read.csv("mydata.csv")
head(mydata)
mydata=mydata[,-1]
head(mydata)

#######BASIC STATISTICS**********

summary(mydata)
abc=sqldf("select * from mydata where FARE>200")
histogram(mydata$SUCCESS,mydata$SEATCLASS)
boxplot(mydata)
plot(mydata)


######################END#######################################




