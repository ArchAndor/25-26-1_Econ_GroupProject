setwd("~/EGYETEM/Mesterképzés-KözgazdaságiElemző/1. félév/Econometrics/Groupwork")

#install.packages("stringr")

library(readxl)
library(ggplot2)
library(lmtest)
library(readr)
library(dplyr)
library(stargazer)
library(stringr)


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
ExtraInfo[2263,1] <- "Pűspökladány"
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


model1 <- lm(Gasoline_average ~ Company_25cut+`Locality legal status`+ `Area (hectare)`+`Resident population`, data = CombinedData)
summary(model1)
#maybe remove area because it probably correlates heavily with population

model2 <- lm(Gasoline_average ~ Company_25cut+`Locality legal status`+`Resident population`, data = CombinedData)
summary(model2)
#maybe a bit better. can we still improve it?

stargazer(model1,model2, type = "text")
