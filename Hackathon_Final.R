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

# load all sources
source("Documents/4th_Sem/Wayfair-Hacks/my_functions.R")
source("Documents/4th_Sem/Biz_Intel/BabsonAnalytics.R")

D = nextYearDemand(D,1)

PA = sqldf("SELECT * FROM PA GROUP BY ItemNumber")

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

write.csv(D6, "df_sample.csv")

# Slicing of the actual data based on Weight
D01 = divideVolbySize(D6,00000,10000)
D12 = divideVolbySize(D6,10000,20000)
D23 = divideVolbySize(D6,20000,30000)
D34 = divideVolbySize(D6,30000,40000)
D45 = divideVolbySize(D6,40000,50000)
D56 = divideVolbySize(D6,50000,60000)
D67 = divideVolbySize(D6,60000,70000)
D78 = divideVolbySize(D6,70000,80000)
D89 = divideVolbySize(D6,80000,90000)
D90 = divideVolbySize(D6,90000,100000)

D_Zip = sqldf("SELECT * FROM D01 UNION
              SELECT * FROM D12 UNION
              SELECT * FROM D23 UNION
              SELECT * FROM D34 UNION
              SELECT * FROM D45 UNION
              SELECT * FROM D56 UNION
              SELECT * FROM D67 UNION
              SELECT * FROM D78 UNION
              SELECT * FROM D89 UNION
              SELECT * FROM D90")

write.csv(D_Zip, "D_Zip_final_v0.csv")

# Slicing of the actual data based on Zones

Dz1 = sqldf("SELECT * FROM D6 WHERE Zipcode >= 00000 AND Zipcode < 30000")

Dz2 = sqldf("SELECT * FROM D6 WHERE Zipcode >= 30000 AND Zipcode < 50000")

Dz3 = sqldf("SELECT * FROM D6 WHERE Zipcode >= 50000 AND Zipcode < 80000")

Dz4 = sqldf("SELECT * FROM D6 WHERE Zipcode >= 80000 AND Zipcode < 100000")

Dz5 = sqldf("SELECT * FROM D6 WHERE Zipcode >= 00000 AND Zipcode < 50000")

Db6 = Dz1

train = TTD
train$TransitDays = NULL

Db6$Zones = NULL
Db6$TransitDays = NULL
#Db6$OrigId = 215
#Db6$OrigId = 388
#Db6$OrigId = 665
#Db6$OrigId = 849
#Db6$OrigId = 935

#Db6$OrigId = 258
Db6$OrigId = 171
#Db6$OrigId = 220
#Db6$OrigId = 373
#Db6$OrigId = 665
#Db6$OrigId = 846
#Db6$OrigId = 946
#Db6$OrigId = 924

test = sqldf("SELECT db6.OrigId, db6.DestId FROM Db6 db6")

Db6$Zones = kNN(Zones ~ OriginStation + DestinationStation, train, test, k=7)

train = TTD
train$Zones = as.factor(train$Zones)
model1 = lm(TransitDays ~ .,train)
model1 = step(model1)

test = sqldf("SELECT db6.OrigId AS OriginStation, db6.DestId AS DestinationStation, 
             db6.Zones FROM Db6 db6")

Db6$TransitDays = predict(model1, test)
Db6$TransitDays = round(Db6$TransitDays)

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

Db7 = sqldf("SELECT db6.Weight AS Weight, db6.Zones AS Zone FROM Db6")

# predict
Db6$Cost = predict(model2,Db7)

Db6$Cost = Db6$Cost + 6.807