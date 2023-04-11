# install excel, dplyr packages
install.packages("readxl")
install.packages("dplyr")
# loading the libraries
library(readxl)
library(dplyr)

# reading the file from the excel; columns only A:G have data
myfile <- "C:/Users/bksat/Documents/mvs_assignment/Customers.xlsx"
custdata <- read_excel(myfile, range = cell_cols("A:G"))

# adding new age group column based on age
custdata <- custdata %>%
                mutate(
                    AgeGroup = case_when(
                    Age <= 25 ~ "25 and under",
                    ((Age > 25) & (Age <= 50)) ~ "26 to 50",
                    ((Age > 50) & (Age <= 75)) ~ "51 to 75",
                    Age >= 75 ~ "76 and over",
                ))

