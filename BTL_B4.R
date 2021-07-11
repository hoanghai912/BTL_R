library(dplyr)
# 1-
## a---------
#Nhap du lieu
load("/home/admins/Desktop/XSTK/flights.rda")

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

