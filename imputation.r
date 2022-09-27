library('dplyr')
library(VIM)
library(mice)
library(ggplot2)
library(ggpubr)

data<-read.csv("BrewdogNew.csv", header=TRUE, stringsAsFactors = T)
#Identifying Missing Data
complete.cases(data)
!complete.cases(data)
aggr(data,prop=FALSE,numbers=TRUE)

#Finding pattern in missing data 
marginplot(data[c("ABV","EBC")], pch=c(20),
           col=c("darkgray", "red", "blue"))

#Finding Data Types 
class(data$ABV) #Numeric 
class(data$IBU) #Numeric 
class(data$OG)#Numeric 
class(data$EBC)#Numeric 
class(data$AttenuationLevel)#Numeric 
class(data$FermentationTempCelsius) #Integer
class(data$Yeast) #Factor 
class(data$Name) #Factor 

#Finding Correlation between data numerical data
cor(data[,2:7], use = "pairwise.complete.obs")

#Finding Correlation between mixed data type
#Dummy Variable for Yeast 
data$Yeast
data_new<-data%>%
  mutate(Yeast1=ifelse(Yeast=='Wyeast 1056 - American Ale', 1,0),Yeast2=ifelse(Yeast=='Wyeast 1272 - American Ale II', 1,0),Yeast3=ifelse(Yeast=='Wyeast 2007 - Pilsen Lager', 1,0), Yeast4=ifelse(Yeast=='Wyeast 3711 - French Saison', 1,0))

#Correlation of EBC with Yeast 
cor(data_new$EBC,data_new$Yeast1,use = "pairwise.complete.obs") #-0.2588591
cor(data_new$EBC,data_new$Yeast2,use = "pairwise.complete.obs") #0.3993766
cor(data_new$EBC,data_new$Yeast3,use = "pairwise.complete.obs") #-0.1377001
cor(data_new$EBC,data_new$Yeast4,use = "pairwise.complete.obs") #-0.1264151

#Correlation of ABV with Yeast
cor(data_new$ABV,data_new$Yeast1,use = "pairwise.complete.obs") # -0.4511592
cor(data_new$ABV,data_new$Yeast2,use = "pairwise.complete.obs") # 0.5486395
cor(data_new$ABV,data_new$Yeast3,use = "pairwise.complete.obs") # -0.14055696
cor(data_new$ABV,data_new$Yeast4,use = "pairwise.complete.obs") #all data misisng for Wyeast 3711 - French Saison

#Multiple Imputation for ABV
mi<-mice(data,m=5, maxit=10)
mi$imp
multiple_imputation<-complete(mi)
mean(multiple_imputation$ABV) 

#Data validation_ABV
ABV_validate<-merge(multiple_imputation,data, by="Name") 
var.test(ABV_validate$ABV.x, ABV_validate$ABV.y)
t.test(ABV_validate$ABV.x, ABV_validate$ABV.y)



#Simple imputation for EBC
#Checking Distribution
hist(data$EBC)
ggqqplot(data$EBC)

#Simple imputation using median 
simple_imputation<- data
simple_imputation$EBC[is.na(simple_imputation$EBC)]<-median(simple_imputation$EBC, na.rm = TRUE)
simple_imputation[is.na(simple_imputation$EBC)]
mean(simple_imputation$EBC)

#Data validation_EBC
EBC_validate<-merge(simple_imputation,data, by="Name") 
var.test(EBC_validate$EBC.x, EBC_validate$EBC.y)
t.test(EBC_validate$EBC.x, EBC_validate$EBC.y)

#New data set creation
ABV<-multiple_imputation%>% select(Name, ABV)
EBC<-simple_imputation%>% select(Name, IBU,OG, EBC, PH,AttenuationLevel, FermentationTempCelsius, Yeast )
Final<- merge(ABV,EBC, by="Name")
write.table(Final, file = "Final BrewDog.csv", row.names=F,sep = ",")
mean(Final$ABV)
mean(Final$EBC)
