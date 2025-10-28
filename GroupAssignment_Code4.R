setwd("~/EGYETEM/Mesterképzés-KözgazdaságiElemző/1. félév/Econometrics/Groupwork")

#install.packages("stringr")

library(readxl)
library(ggplot2)
library(lmtest)
library(readr)
library(dplyr)
library(stargazer)
library(stringr)
library(stats)


FuelPrices <- read_excel("2025.04.11-04.17.xlsx")

table(FuelPrices$Company)
typeof(FuelPrices$Company)

table(FuelPrices$Address)

FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]

as.vector(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]))

mean(as.vector(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5])))

as.data.frame(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]))[,1]

#something works now
mean(as.data.frame(c(FuelPrices[FuelPrices$Address=="Szeghalom, Kinizsi u. 12-14.",5]))[,1])

FuelPrices$Diesel_average <- 0

mean(as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],5]))[,1])
FuelPrices$Address[3]

#take the average of the diesel
for (i in 1:nrow(FuelPrices)) {
  FuelPrices$Diesel_average[i] <- mean(as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[i],5]))[,1])
}


as.data.frame(c(FuelPrices[FuelPrices$Address==FuelPrices$Address[3],7]))[,1]


#take the average of the gasoline
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

#remove the unnecessary columns
FuelPrices_filtered <- FuelPrices_filtered[,c(2,3,4,7,8)]

#Mözs is part of Tolna, so we will use the Tolna data for the Tolna-Mözs settlement
FuelPrices_filtered[1097,1] <- "Tolna"



#importing extra information


ExtraInfo <- read_delim("dgh_download_2025(Localities 01.01.2025.csv", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                                      skip = 2)



#merge(FuelPrices_filtered, ExtraInfo, by.x = "Settlement", by.y = "Locality name")

ExtraInfo <- ExtraInfo[order(ExtraInfo$`Locality name`),]



FuelPrices_filtered1 <- FuelPrices_filtered
FuelPrices_filtered1 <- FuelPrices_filtered1[order(FuelPrices_filtered1$Settlement),]


#I don't want to talk about this
ExtraInfo[395,1] <- "Budapest I. kerület"
ExtraInfo[396,1] <- "Budapest II. kerület"
ExtraInfo[397,1] <- "Budapest III. kerület"
ExtraInfo[398,1] <- "Budapest IV. kerület"
ExtraInfo[399,1] <- "Budapest V. kerület"
ExtraInfo[400,1] <- "Budapest VI. kerület"
ExtraInfo[401,1] <- "Budapest VII. kerület"
ExtraInfo[402,1] <- "Budapest VIII. kerület"
ExtraInfo[403,1] <- "Budapest IX. kerület"
ExtraInfo[404,1] <- "Budapest X. kerület"
ExtraInfo[405,1] <- "Budapest XI. kerület"
ExtraInfo[406,1] <- "Budapest XII. kerület"
ExtraInfo[407,1] <- "Budapest XIII. kerület"
ExtraInfo[408,1] <- "Budapest XIV. kerület"
ExtraInfo[409,1] <- "Budapest XV. kerület"
ExtraInfo[410,1] <- "Budapest XVI. kerület"
ExtraInfo[411,1] <- "Budapest XVII. kerület"
ExtraInfo[412,1] <- "Budapest XVIII. kerület"
ExtraInfo[413,1] <- "Budapest XIX. kerület"
ExtraInfo[414,1] <- "Budapest XX. kerület"
ExtraInfo[415,1] <- "Budapest XXI. kerület"
ExtraInfo[416,1] <- "Budapest XXII. kerület"
ExtraInfo[417,1] <- "Budapest XXIII. kerület"


#"fixing" some typeos
ExtraInfo[825,1] <- "Fűle"
ExtraInfo[830,1] <- "Fűlöpszállás"
ExtraInfo[837,1] <- "Fűzesabony"
ExtraInfo[2263,1] <- "Püspökladány"
ExtraInfo[2322,1] <- "Révfűlöp"
ExtraInfo[2502,1] <- "Sűkösd"
ExtraInfo[2504,1] <- "Sűmeg"
ExtraInfo[2812,1] <- "Tiszafűred"



CombinedData <- merge(FuelPrices_filtered1, ExtraInfo, by.x = "Settlement", by.y = "Locality name")


table(CombinedData$`Locality legal status`)
table(CombinedData$Company)
companyinfo <- as.data.frame(table(CombinedData$Company))
companyinfo[companyinfo$Freq>10,]
companyinfo[companyinfo$Freq>25,]

CombinedData$Company_25cut <- "Other"

#if(CombinedData$Company=="Mobil Petrol"){CombinedData$Company_25cut <- "Mobil Petrol"}


CombinedData$Company_25cut <- ifelse(CombinedData$Company=="Mobil Petrol",CombinedData$Company_25cut <- "Mobil Petrol",
                                     CombinedData$Company_25cut <- ifelse(CombinedData$Company=="Mol",CombinedData$Company_25cut <- "Mol",
                                                                          CombinedData$Company_25cut <- ifelse(CombinedData$Company=="Omv",CombinedData$Company_25cut <- "Omv",
                                                                                                               CombinedData$Company_25cut <- ifelse(CombinedData$Company=="Orlen",CombinedData$Company_25cut <- "Orlen",
                                                                                                                                                    CombinedData$Company_25cut <- ifelse(CombinedData$Company=="Shell",CombinedData$Company_25cut <- "Shell",CombinedData$Company_25cut <- "Other")))))

table(CombinedData$Company_25cut)

typeof(CombinedData$`Area (hectare)`)
typeof(CombinedData$`Resident population`)
CombinedData$`Area (hectare)` <- str_remove_all(CombinedData$`Area (hectare)`," ")


CombinedData$`Locality legal status` <- as.factor(CombinedData$`Locality legal status`)
CombinedData$Company_25cut <- as.factor(CombinedData$Company_25cut)
CombinedData$`Area (hectare)` <- as.numeric(CombinedData$`Area (hectare)`)
CombinedData$`Resident population` <- as.numeric(CombinedData$`Resident population`)


model_gas_1 <- lm(Gasoline_average ~ Company_25cut+`Locality legal status`+ `Area (hectare)`+`Resident population`, data = CombinedData)
summary(model_gas_1)
#maybe remove area because it probably correlates heavily with population

model_gas_2 <- lm(Gasoline_average ~ Company_25cut+`Locality legal status`+`Resident population`, data = CombinedData)
summary(model_gas_2)
#maybe a bit better. can we still improve it?

stargazer(model_gas_1,model_gas_2, type = "text")

#diesel models

model_diesel_1 <- lm(Diesel_average ~ Company_25cut+`Locality legal status`+ `Area (hectare)`+`Resident population`, data = CombinedData)
summary(model_diesel_1)
#maybe remove area because it probably correlates heavily with population

model_diesel_2 <- lm(Diesel_average ~ Company_25cut+`Locality legal status`+`Resident population`, data = CombinedData)
summary(model_diesel_2)
#maybe a bit better. can we still improve it?

stargazer(model_diesel_1,model_diesel_2, type = "text")

#fixing the combined data

CombinedData[421:422,1] <- "Füle"
CombinedData[423:424,1] <- "Fülöpszállás"
CombinedData[425:427,1] <- "Füzesabony"
CombinedData[901,1] <- "Révfülöp"
CombinedData[953,1] <- "Sükösd"
CombinedData[955:958,1] <- "Sümeg"
CombinedData[1113:1115,1] <- "Tiszafüred"


#simplifing factors

aggregate(Gasoline_average ~ `Locality legal status`,
          data = CombinedData,
          FUN = mean, na.rm = TRUE)

CombinedData$Locality_simple <- factor(
  ifelse(CombinedData$`Locality legal status` %in% 
           c("fővárosi kerület", "megyei jogú város", "megyeszékhely, megyei jogú város"), "nagyváros",
         ifelse(CombinedData$`Locality legal status` %in% 
                  c("nagyközség", "község"), "község", "város")),
  levels = c("nagyváros", "város", "község")
)

model_gas_3 = lm(Gasoline_average ~ Company_25cut+ Locality_simple+ `Area (hectare)`+`Resident population`, data = CombinedData)
summary(model_gas_3)

#trying new variables

CombinedData$Pop_density <- CombinedData$`Resident population` / CombinedData$`Area (hectare)`
CombinedData$`Number of dwellings` <- as.numeric(CombinedData$`Number of dwellings`)
CombinedData$People_per_household <- CombinedData$`Resident population` / CombinedData$`Number of dwellings`

model_gas_4 = lm(Gasoline_average ~ Company_25cut + Locality_simple + Pop_density + People_per_household, data = CombinedData)
summary(model_gas_4)
#new variables don't seem to add too much.

#stations per settlement
station_counts <- aggregate(Address ~ Settlement, data = CombinedData, FUN = length)
names(station_counts)[2] <- "Stations_per_settlement"

CombinedData <- merge(CombinedData, station_counts, by = "Settlement", all.x = TRUE)

CombinedData$Stations_per_1000 <- CombinedData$Stations_per_settlement / CombinedData$`Resident population`*1000

model_gas_5 = lm(Gasoline_average ~ Company_25cut + Locality_simple + People_per_household + Stations_per_1000, data = CombinedData)
summary(model_gas_5)
#this is significant! but the coefficient is positive which doesn't seem intuitive

#adding new data

wage <- read.csv("Average monthly gross earnings of full-time employees (residence of the employees)_2025.csv",
                 sep = ";", stringsAsFactors = FALSE)

CombinedData <- merge(CombinedData, wage[,c(2,5)],
                      by.x = "District name",
                      by.y = "JARAS_NEV",
                      all.x = TRUE)


#fixing Budapest
bpwg <- wage[wage$JARAS_NEV=="Budapest",5]
bpwg<- as.numeric(gsub(",", ".", gsub(" ", "", bpwg)))

CombinedData$Wage <- as.numeric(gsub(",", ".", gsub(" ", "", CombinedData$VALUE)))
CombinedData$Wage[89:259] <- bpwg

model_gas_6 = lm(Gasoline_average ~ Company_25cut + Locality_simple + People_per_household + Stations_per_1000 + Wage, data = CombinedData)
summary(model_gas_6)
#distinct average wage is not significant

cars <- read.csv("Passenger cars per thousand capita_2024.csv",
                 sep = ";", stringsAsFactors = FALSE)
cars$VALUE <- as.numeric(cars$VALUE)

cars[1084,3] <- "Budapest"

cars_settlement <- cars[, c("TELEP_NEV", "VALUE")]
names(cars_settlement) <- c("Settlement", "Cars_per_1000")
cars_district <- aggregate(VALUE ~ JARAS_NEV, data = cars, FUN = mean, na.rm = TRUE)
names(cars_district) <- c("District name", "Cars_per_1000_district")
CombinedData <- merge(CombinedData, cars_settlement,
                      by = "Settlement", all.x = TRUE)


CombinedData <- merge(CombinedData, cars_district,
                      by = "District name", all.x = TRUE)

bpcr <- cars_district[20,2]
CombinedData$Cars_per_1000[89:259] <- bpcr
CombinedData$Cars_per_1000_district[89:259] <- bpcr

model_gas_7 = lm(Gasoline_average ~ Company_25cut + Locality_simple + People_per_household + Stations_per_1000 + Cars_per_1000, data = CombinedData)
summary(model_gas_7)
model_gas_8 = lm(Gasoline_average ~ Company_25cut + Locality_simple + People_per_household + Stations_per_1000 + Cars_per_1000_district, data = CombinedData)
summary(model_gas_8)
#they are not significant
#actually model_gas_7 is significant on 10%, altough it's not much. Up to us if we want to keep it as a control

#to me this seems to be the best so far:
summary(model_gas_5)


is.na(CombinedData$Cars_per_1000)
table(is.na(CombinedData$Cars_per_1000))
#as it turns out there is no data about Felcsút in the cars dataset CombinedData[66,39]
median(as.data.frame(cars[cars$JARAS_NEV=="Bicskei",])[,7], na.rm = TRUE)
#in case we want/need to fill the missing data, remove the #s from the two lines below

#CombinedData[66,39] <- median(as.data.frame(cars[cars$JARAS_NEV=="Bicskei",])[,7], na.rm = TRUE)
#model_gas_7 = lm(Gasoline_average ~ Company_25cut + Locality_simple + People_per_household + Stations_per_1000 + Cars_per_1000, data = CombinedData)

#It is recommended for the models to have the same amount of observations
AIC(model_gas_5,model_gas_7)
BIC(model_gas_5, model_gas_7)
