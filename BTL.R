library(dplyr)

data <- read.csv('C:/Users/Admin/Desktop/XSTK/gia_nha.csv', TRUE, ",")

new_DF <- data.frame(data$price, data$sqft_living15, data$floors,
                     data$condition, data$sqft_above, data$sqft_living)

colnames(new_DF) <- c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")

#D???m các giá tr??? b??? Missing
colSums(is.na(new_DF))
which(is.na(new_DF$price)) #V??? trí c???a các Missing value

#Vì s??? li???u b??? Missing r???t bé so v???i t???ng d??? li???u nên d??? li???u missing là b???ng 0
#ho???c b??? lo???i b???

new_DF <- new_DF %>% filter(!is.na(new_DF))

#Chuy???n d??? li???u c???n tính v??? d???ng log
new_DF$price <- log(new_DF$price)
new_DF$sqft_living15 <- log(new_DF$sqft_living15)
new_DF$sqft_above <- log(new_DF$sqft_above)
new_DF$sqft_living <- log(new_DF$sqft_living)

#Trung bình
mean = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, mean)

#Trung v???
median = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, median)

#D??? l???ch chu???n
sd = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, sd)

#Giá tr??? l???n nh???t
max = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, max)

#Giá tr??? nh??? nh???t
min = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, min)

#Xu???t k???t qu??? du???i d???ng b???ng
analyze <- data.frame(mean, median, sd, max, min)
analyze

#B???ng th???ng kê s??? lu???ng
floor <- table(new_DF$floors)
floor

condition <- table(new_DF$condition)
condition

#D??? th??? phân ph???i bi???n price
hist(new_DF$price, main = "HISTOGRAM OF PRICE", xlab="Price", ylim = c(0,8000))

