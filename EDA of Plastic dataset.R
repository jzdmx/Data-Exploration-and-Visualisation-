#Loading libraries
library(corrplot)
library(ggplot2)
library(dplyr)
library(sm)

# An overview of the data
str(plastic)
summary(plastic)
head(plastic) 
class(plastic)

view(plastic)
head(plastic)

### Q1 ###

# Checking for missing values
# identify count of NAs in data frame
sum(is.na(plastic))

# identify location of NAs in vector
colSums(is.na(plastic))

# recode missing values with the mean
plastic$PlasticWaste[is.na(plastic$PlasticWaste)] <- mean(plastic$PlasticWaste, na.rm = TRUE)
plastic$MismanagedPW[is.na(plastic$MismanagedPW)] <- mean(plastic$MismanagedPW, na.rm = TRUE)
sum(is.na(plastic))

# Histgram for the two types of plastic waste
hist(plastic$PlasticWaste, breaks = 50, main="Histgram for Plastic Waste")
hist(plastic$MismanagedPW, breaks = 50, main="Histgram for Mismanaged Plastic Waste")

# Boxplot for the two types of plastic waste
boxplot(plastic$PlasticWaste,main="Boxplot for Plastic Waste")
boxplot(plastic$MismanagedPW, main="Boxplot for Mismanaged Plastic Waste")

###  Q2 ###
# the distributions of plastic waste by region
boxplot(plastic$PlasticWaste~plastic$Region, 
        col=c("red","orange","yellow","green","cyan","blue"),
        main="Region distribution among plastic waste")
boxplot(plastic$MismanagedPW~plastic$Region,
        col=c("red","orange","yellow","green","cyan","blue"),
        main="Region distribution among mismanaged plastic waste")

# the distributions of plastic waste by income status
boxplot(plastic$PlasticWaste~plastic$IncomeStatus, notch = TRUE,
        col=c("blue","gold","green","red"),
        main="Income distribution among plastic waste")
boxplot(plastic$MismanagedPW~plastic$IncomeStatus, notch = TRUE,
        col=c("blue","gold","green","red"),
        main="Income distribution among mismanaged plastic waste")

### Q3 ###
# the distributions of plastic waste by income status
sm.density.compare(plastic$PlasticWaste, plastic$Region, xlab= "Region")
pw_legend=factor(plastic$Region, labels =c("East Asia & Pacific",
                 "Europe & Central Asia","Latin America & Caribbean",
                 "Middle East & North Africa","North America",
                 "South Asia","Sub-Saharan Africa"))
colfill <- c(2:(2+length(levels(pw_legend))))
legend(locator(1), levels(pw_legend), fill = colfill)             

# The relationship between PlasticWaste and 

IncSt <- xtabs(~IncomeStatus, data=plastic) ## save the table to `offs`
barplot(IncSt, col='skyblue')

?barplot




