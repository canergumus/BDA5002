# Get the working directory. If needed, you can set the working directory to another folder.
#getwd()
setwd("/Users/canergumus/desktop/BDA5002")

# Read the Data from a directory  
EuropeanSalesData<-read.csv("EuropeanSales.csv",header=T)

#Show attributes  
attributes(EuropeanSalesData)

#Dropped country field for checking correlation. (characteristic value)
EuropeanSalesDropped <- subset(EuropeanSalesData, select = -c(Country))

#install lib for correlation visualization
install.packages("ggplot2")
install.packages("corrplot")
library("corrplot")

#Visualization correlation chart and matrix
cor(EuropeanSalesDropped)
corrplot.mixed(cor(EuropeanSalesDropped),
               order="hclust", tl.col="black")

#There is a high correlation between Population and Computer sales and
#SalesPerCapita & GDPperHead - EducationSpending
#Checking their plot
plot(EuropeanSalesData$GDPperHead,EuropeanSalesData$SalesPerCapita,
     ylab="Sales", xlab="GDP")
plot(EuropeanSalesData$EducationSpending,EuropeanSalesData$SalesPerCapita,
     ylab="Sales", xlab="Education Spending")

#Plot all measure in the dataset
plot(EuropeanSalesDropped)

#LR first model for Computer Sales
#Model was created with Population - GDPperHead - SalesPerCapita
#R-Square values are 0,777 and 0,738, high and no much difference. 
#P and t values high, exception is population
ModelCS1 <- lm(ComputerSales ~ Population + GDPperHead + SalesPerCapita, data=EuropeanSalesData)
summary(ModelCS1)

#Model attributes and coefficients  
attributes(ModelCS1)
ModelCS1$coefficients

#################

#LR second model for Computer Sales
#Model was created with all variables
#R-Square values are 0,779 and 0,705, high but difference is higher than first model. 
#P and t values high, exception is population. Residual standart is lower than first model which is good point
ModelCS2 <- lm(ComputerSales ~ UnemploymentRate + EducationSpending + Population + SalesPerCapita + GDPperHead, data=EuropeanSalesData)
summary(ModelCS2)

#ModelComSales1 is performing better than the second model.

#Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(ModelCS1)
#"Normal Q-Q" and "Residuals vs Leverage" shows that 20th and 21st records are outlier.

#################################################################

#LR first model for SalesperCapita
#Model was created with Population - GDPperHead - SalesPerCapita
#R-Square values are 0,621 and 0,495, average and avg difference. 
#P and t values are high
ModelSC1 <- lm(SalesPerCapita  ~ Population + GDPperHead + ComputerSales, data=EuropeanSalesData)
summary(ModelSC1)

#Model attributes and coefficients  
attributes(ModelSC1)
ModelSC1$coefficients

#################

#LR second  model for SalesperCapita
#Model was created with all variables
#R-Square values are 0,390 and 0,282, low R-square values, also gap is high. 
#P and t values are high, Residual error is higher than first model
ModelSC2 <- lm(SalesPerCapita  ~ UnemploymentRate + EducationSpending + Population + ComputerSales + GDPperHead, data=EuropeanSalesData)
summary(ModelSalesCap2)

#ModelSalesCap1 is performing better than the second one.

#Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(ModelSC1)
#"Normal Q-Q" and "Residuals vs Leverage" shows that 6th and 20th records are outlier.

#As a result of tested models, for the Computer Sales, ModelComSales1 performed better and
#for the SalesperCapita, ModelSalesCap1 performed better.
