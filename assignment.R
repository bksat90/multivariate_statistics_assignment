# install excel package
install.packages("readxl")
# loading the library
library(readxl)

# reading the file from the excel; columns only A:G have data
myfile <- "C:/Users/bksat/Documents/mvs_assignment/Customers.xlsx"
cust.data <- read_excel(myfile, range = cell_cols("A:G"))
