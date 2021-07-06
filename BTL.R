library(dplyr)

data <- read.csv('C:/Users/Administrator/Desktop/SXTK/gia_nha.csv', TRUE, ",")

new_DF <- data.frame(data$price, data$sqft_living15, data$floors,
                     data$condition, data$sqft_above, data$sqft_living)

new_DF[is.na(new_DF)] <- 0
which(is.na(new_DF))
