# loading libraries
library(readr)
library(gmodels)
library(class)
library(ggplot2)

# load all files
CS <- read_csv("Documents/4th_Sem/Wayfair-Hacks/Hackathon Case and Data/Carrier Stations.csv")
TTD <- read_csv("Documents/4th_Sem/Wayfair-Hacks/Hackathon Case and Data/Transit Time and Distance.csv")
TDC <- read_csv("Documents/4th_Sem/Wayfair-Hacks/Hackathon Case and Data/Transit Distance and Cost.csv")
D <- read_csv("Documents/4th_Sem/Wayfair-Hacks/Hackathon Case and Data/Demand.csv")
PA <- read_csv("Documents/4th_Sem/Wayfair-Hacks/Hackathon Case and Data/Product Attributes.csv")

# loading sources
source("Documents/4th_Sem/Wayfair-Hacks/my_functions.R")
source("Documents/4th_Sem/Biz_Intel/BabsonAnalytics.R")

D3 = sqldf("SELECT d.ItemNumber, d.Zipcode,	d.Units, p.CubicInches, p.ProductCategory, 
           p.Weight,	p.OriginZip,	p.Forecast2018,	p.PointProductMarginDollars 
           FROM D d INNER JOIN PA p ON d.ItemNumber = p.ItemNumber")

CS$PostalCode = as.integer(CS$PostalCode)
D3$Zipcode = as.integer(D3$Zipcode)
D3$OriginZip = as.integer(D3$OriginZip)

D4 = sqldf("SELECT cs.CarrierStationID AS DestId, d3.ItemNumber, d3.Zipcode,	d3.Units,	d3.CubicInches,
           d3.Weight,	d3.OriginZip,	d3.Forecast2018, d3.PointProductMarginDollars, d3.ProductCategory
           FROM CS cs INNER JOIN D3 d3 ON cs.PostalCode = d3.Zipcode")

D5 = sqldf("SELECT cs.CarrierStationID AS OrigId, d4.DestId, d4.ItemNumber, d4.Zipcode,	d4.Units,	d4.CubicInches,
           d4.Weight,	d4.OriginZip,	d4.Forecast2018, d4.PointProductMarginDollars, d4.ProductCategory
           FROM CS cs INNER JOIN D4 d4 ON cs.PostalCode = d4.OriginZip")

Db1 = sqldf("SELECT d5.OrigId as OriginStation, d5.DestId AS DestinationStation FROM
            D5 d5")

# kNN and Linear Model on TTD
train = TTD
train$TransitDays = NULL

Db1$Zones = kNN(Zones ~ OriginStation + DestinationStation, train, test = Db1, k=7)

train = TTD
train$Zones = as.factor(train$Zones)
model1 = lm(TransitDays ~ .,train)
model1 = step(model1)

Db1$TransitDays = predict(model1, Db1)
Db1$TransitDays = round(Db1$TransitDays)

D6 = D5
D6$Zones = Db1$Zones
D6$TransitDays = Db1$TransitDays

# Linear Model on TDC
# manage
TDC$Zone = as.factor(TDC$Zone)

# slice
N = nrow(TDC)

trainingSize = round(N)
trainingCases = sample(N,trainingSize)
training = TDC[trainingCases,]

test = TDC[-trainingCases,]

# build
model2 = lm(Cost ~ ., data = training)

model2 = step(model2)

D7 = sqldf("SELECT d6.Weight AS Weight, d6.Zones AS Zone FROM D6 d6")

# predict
D6$Cost = predict(model2,D7)

D6$Cost = D6$Cost + 6.807
D6$ProductCategory = as.factor(D6$ProductCategory)

write.csv(D6, "data_final.csv")