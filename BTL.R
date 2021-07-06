library(dplyr)

data <- read.csv('C:/Users/Admin/Desktop/XSTK/gia_nha.csv', TRUE, ",")

new_DF <- data.frame(data$price, data$sqft_living15, data$floors,
                     data$condition, data$sqft_above, data$sqft_living)

