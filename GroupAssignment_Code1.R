setwd("~/EGYETEM/Mesterképzés-KözgazdaságiElemző/1. félév/Econometrics/Groupwork")

library(readxl)
library(ggplot2)
library(lmtest)




FuelPrices <- read_excel("2025.04.11-04.17.xlsx")

table(FuelPrices$Company)
typeof(FuelPrices$Company)

table(FuelPrices$Address)

FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]

as.vector(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]))

mean(as.vector(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5])))

as.data.frame(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]))[,1]

#így működik valami
mean(as.data.frame(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]))[,1])

FuelPrices$Diesel_average <- 0

mean(as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],5]))[,1])
FuelPrices$Address[3]

#átlagoljuk a dízelt
for (i in 1:nrow(FuelPrices)) {
  FuelPrices$Diesel_average[i] <- mean(as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[i],5]))[,1])
}


as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],7]))[,1]


#átlagoljuk a benzint
FuelPrices$Gasoline_average <- 0

mean(as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],6]))[,1])
as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],6]))

for (i in 1:nrow(FuelPrices)) {
  FuelPrices$Gasoline_average[i] <- mean(as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[i],6]))[,1])
}

as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],8]))[,1]

FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14."

nrow(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",])

nrow(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],])

FuelPrices_filtered <- FuelPrices
FuelPrices_filtered$filter <- 1

typeof(FuelPrices$Date)

table(FuelPrices_filtered$Date)[7]
trick <- nrow(FuelPrices_filtered)-table(FuelPrices_filtered$Date)[7]



for (i in 1:trick) {
  if(nrow(FuelPrices[FuelPrices$Address==FuelPrices$Address[i],])>1) {FuelPrices_filtered$filter[i]=0
                                                                      }
}


FuelPrices_filtered <- FuelPrices_filtered[FuelPrices_filtered$filter==1,]

sum(table(FuelPrices_filtered$Address))

#kitörlöm a felesleges öszlopokat (dátum, adott napi árak és a filter oszlop)
FuelPrices_filtered <- FuelPrices_filtered[,c(2,3,4,7,8)]
