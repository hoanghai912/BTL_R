library(dplyr)

data <- read.csv('C:/Users/Admin/Desktop/XSTK/gia_nha.csv', TRUE, ",")

#2 -
## a---------
new_DF <- data.frame(data$price, data$sqft_living15, data$floors,
                     data$condition, data$sqft_above, data$sqft_living)

colnames(new_DF) <- c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")

## b---------
#Dem cac gia tri Missing
colSums(is.na(new_DF))
which(is.na(new_DF$price)) #Vi tri cua cac Missing value

#Vi du lieu bi Missing rat be so voi tong du lieu nen ta co the set du lieu
#ve 0 hoac loai bo du lieu

new_DF <- new_DF %>% filter(!is.na(new_DF))


#3
## a-------------------
#Chuyen du lieu can tinh ve dang log
new_DF$price <- log(new_DF$price)
new_DF$sqft_living15 <- log(new_DF$sqft_living15)
new_DF$sqft_above <- log(new_DF$sqft_above)
new_DF$sqft_living <- log(new_DF$sqft_living)

## b------------------
#Trung binh
mean = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, mean)

#Trung vi
median = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, median)

#Do lech chuan
sd = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, sd)

#Gia tri lon nhat
max = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, max)

#Gia tri nho nhat
min = apply(new_DF[,c("price", "sqft_living15", "sqft_above", "sqft_living")], 2, min)

#Xuat ket qua duoi dang bang
analyze <- data.frame(mean, median, sd, max, min)
analyze

## c-----------------
#Bang thong ke so luong
floor <- table(new_DF$floors)
floor

condition <- table(new_DF$condition)
condition

## d-----------------
#Do thi phan phoi bien price
hist(new_DF$price, main = "HISTOGRAM OF PRICE", xlab="Price", ylim = c(0,8000))

## e-----------------
#Phan phoi cua bien price cho tung nhom phan loai
boxplot(new_DF$price ~ new_DF$floors, 
        main = "Do thi phan phoi cua bien Price theo bien Floor",
        xlab = "Floor",
        ylab = "Price",
        col = "cadetblue2",
        border = "cornflowerblue")

boxplot(new_DF$price ~ new_DF$condition, 
        main = "Do thi phan phoi cua bien Price theo bien Condition",
        xlab = "Condition",
        ylab = "Price",
        col = "cadetblue2",
        border = "cornflowerblue")

## f-----------------
#Phan phoi cua bien price lan luot theo cac bien
pairs(new_DF$price ~ new_DF$sqft_living15,
      main = "Phan phoi cua bien Price theo bien Sqft_living15",
      labels = c("Price", "Sqft_living15"))

pairs(new_DF$price ~ new_DF$sqft_above,
      main = "Phan phoi cua bien Price theo bien Sqft_above",
      labels = c("Price", "Sqft_above"))  

pairs(new_DF$price ~ new_DF$sqft_living,
      main = "Phan phoi cua bien Price theo bien Sqft_living",
      labels = c("Price", "Sqft_living"))  
