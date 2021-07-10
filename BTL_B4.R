# 1-
## a---------
#Nhap du lieu
load("C:/Users/Administrator/Desktop/SXTK/flights.rda")

## b---------
newFlights <- data.frame(flights$carrier, flights$origin, flights$dep_time,
                         flights$arr_time, flights$dep_delay, flights$arr_delay)

colnames(newFlights) <- c("carrier", "origin", "dep_time",
                          "arr_time", "dep_delay", "arr_delay")

## c---------



