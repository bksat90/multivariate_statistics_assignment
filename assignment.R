# install excel, dplyr packages
install.packages("readxl")
install.packages("dplyr")
# loading the libraries
library(readxl)
library(dplyr)

# reading the file from the excel; columns only A:G have data
myFile <- "C:/Users/bksat/Documents/mvs_assignment/Customers.xlsx"
custData <- read_excel(myFile, range = cell_cols("A:G"))

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


####### highest correlation
# covariance matrix
S <- var(quantVar)
# correlation matrix
R <- cor(S)
R
# Answer income and size have higher positive correlation 0.9506432


####### 