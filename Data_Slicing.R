# Data Slicing

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

write.csv(D_Zip, "D_Zip_final.csv")