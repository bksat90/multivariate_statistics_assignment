# install excel, dplyr packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ICSNP")
# loading the libraries
library(readxl)
library(dplyr)
library(ICSNP)

# reading the file from the excel; columns only A:G have data
myFile <- "C:/Users/bksat/Documents/mvs_assignment/Customers.xlsx"
custData <- read_excel(myFile, range = cell_cols("A:G"))

head(custData)


# adding new age group column based on age
custData <- custData %>%
                mutate(
                    AgeGroup = case_when(
                    Age <= 25 ~ "25 and under",
                    ((Age > 25) & (Age <= 50)) ~ "26 to 50",
                    ((Age > 50) & (Age <= 75)) ~ "51 to 75",
                    Age >= 75 ~ "76 and over",
                ))
# display top 10 values after adding new column
head(custData, 10)


# sample mean vector corresponds to the four quantitative independent variables
# extract required columns
quantVar <- custData[c(3, 4, 6, 7)]
sampleMeanVector <- as.matrix(colMeans(quantVar),
                              nrow = length(quantVar),
                              ncol = 1,
                              byrow = T)
sampleMeanVector


# Greatest level of variation
S <- var(quantVar)
S
# Answer s2 for Income and Score (2536.113758 and 808.227563 respectively)
# are high. ie. Variance of these factors are high. So they have highest 
# deviation from the mean

####### highest correlation
# correlation matrix
R <- cor(S)
R
# Answer income and size have higher positive correlation 0.9506432


####### population mean vector is equal to given vector
givenMean <- c(88.0, 50.0, 4.5, 3.2)

HotellingsT2(quantVar, mu = givenMean)

#data:  quantVar
#T.2 = 0.060313, df1 = 4, df2 = 396, p-value = 0.9933
#alternative hypothesis: true location is not equal to c(88,50,4.5,3.2)

######### two-sample profile analysis plot
# male group
sampleMale <- custData[custData$Gender == "Male", ]
quantMale <- sampleMale[c(3, 4, 6, 7)]
maleMean <- as.matrix(colMeans(quantMale),
                                nrow = length(quantMale),
                                ncol = 1,
                                byrow = T)
maleMean

  
# female group
sampleFemale <- custData[custData$Gender == "Female", ]
quantFemale <- sampleFemale[c(3, 4, 6, 7)]

femaleMean <- as.matrix(colMeans(quantFemale),
                      nrow = length(quantFemale),
                      ncol = 1,
                      byrow = T)
femaleMean

# profile plot
pbg(profileData[, 2:5], profileData$Gender,
    original.names = T, profile.plot = T)


# one way MANOVA between four age groups
head(custData)
cust.man <- manova(cbind(Income, Score, Shops, Size) ~ AgeGroup, data = custData)
cust.man

summary(cust.man, test = "Wilks")

#From the above Wilks test, p-value is found as 0, which implies strong evidence against null hypothesis of equal population mean vectors amongst different age group

summary.aov(cust.man)
# Income and size have lesser p values


#### discriminant analysis

#### Two way MANOVA
cust.two.man <- manova(cbind(Income, Score, Shops, Size) ~ AgeGroup*Gender,
                       data = custData)
cust.two.man

summary(cust.two.man, test = "Wilks")

cust.two.man.add <- manova(cbind(Income, Score, Shops, Size) ~ AgeGroup+Gender,
                       data = custData)
cust.two.man.add

summary(cust.two.man.add, test = "Wilks")





