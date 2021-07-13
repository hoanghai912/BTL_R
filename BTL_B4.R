library(dplyr)
library(nortest)
library(car)
# 1-
## a----------
load("C:/Users/Admin/Desktop/XSTK/flights.rda")

## b---------
newFlights <- data.frame(flights$carrier, flights$origin, flights$dep_time,
                         flights$arr_time, flights$dep_delay, flights$arr_delay)

colnames(newFlights) <- c("carrier", "origin", "dep_time",
                          "arr_time", "dep_delay", "arr_delay")

## c---------
NA_value <- data.frame(colSums(is.na(newFlights)) / dim(newFlights)[1])
names(NA_value) <- "Percent"

#Phuong phap de xuat: Loai bo gia tri bi Missing
newFlights <- newFlights %>% na.omit(newFlights)

## d---------
comau <- mean <- sd <- min <- max <- Qu1st <- median <-Qu3rd <- vector()
carrier_name <- as.vector(unique(newFlights$carrier))

for (i in 1:length(carrier_name))
{
  data <- newFlights %>% filter(newFlights$carrier == carrier_name[i])
  dep_delay <- data$dep_delay
  comau <- append(comau, dim(data)[1])
  mean <- append(mean, mean(dep_delay))
  sd <- append(sd, sd(dep_delay))
  min <- append(min, min(dep_delay))
  max <- append(max, max(dep_delay))
  Qu1st <- append(Qu1st, summary(dep_delay)[2])
  Qu3rd <- append(Qu3rd, summary(dep_delay)[5])
  median <- append(median, median(dep_delay))
}

statics <- data.frame(comau, mean, sd, min, max, Qu1st, median, Qu3rd)
rownames(statics) <- carrier_name

## d------------
boxplot(newFlights$dep_delay ~ newFlights$carrier,
        main = "Do thi thoi gian khoi hanh tre cua tung hang hang khong",
        xlab = "Carrier",
        ylab = "Dep_delay",
        col = "cadetblue2",
        border = "cornflowerblue")
#Tim khoang tu phan vi 25% va 75%
Quatines <- quantile(newFlights$dep_delay, prob = c(0.25,0.75))
interquartile_range = IQR(newFlights$dep_delay)

#Loc du lieu outliers
new_box_data = subset(newFlights, newFlights$dep_delay > (Quatines[1] - 1.5*interquartile_range)
                            & newFlights$dep_delay < (Quatines[2] + 1.5*interquartile_range))

boxplot(new_box_data$dep_delay ~ new_box_data$carrier,
        main = "Do thi thoi gian khoi hanh tre cua tung hang hang khong",
        xlab = "Carrier",
        ylab = "Dep_delay",
        col = "cadetblue2",
        border = "cornflowerblue")

#2--
pdx <- subset(newFlights, newFlights$origin=="PDX") #Loc chuyen bay den tu Portland
##a
##b
## c----------
#Do thi QQ-plot
anova_ow <- aov(dep_delay ~ carrier, data = pdx)
plot(anova_ow, 2)


#Kiem dinh Anderson-Darling
anova_res <- residuals(object = anova_ow)
ad.test(anova_res) 

#Kiem dinh Levene
suppressWarnings( leveneTest(dep_delay ~ carrier, data = pdx) )

## d----------
#phan tich anova
summary(anova_ow)

