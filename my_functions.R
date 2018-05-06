nextYearDemand <- function(mydata1, n = 1.5){
  mydata2 = sqldf("SELECT DISTINCT (ItemNumber || ' ' || Zipcode) AS UniqueId, Units FROM mydata1 GROUP BY UniqueId")
  
  mydata2$Demand2018 = mydata2$Units * n
  mydata3 = sqldf("SELECT substr(UniqueId, 1,charindex(' ',UniqueId)) AS ItemNumber,
                  LTRIM(substr(UniqueId, charindex(' ',UniqueId),length(UniqueId)-charindex(' ',UniqueId)+1)) AS Zipcode, 
                  Demand2018 AS Units FROM mydata2")
  mydata3$ItemNumber = trimws(mydata3$ItemNumber, which = c("right"))
  return(mydata3)
}

divideVolbySize <- function(my_data1, l=00000, u=10000){
  my_data1$l = l
  my_data1$u = u
  my_data2 = sqldf("SELECT * FROM my_data1 data1 WHERE data1.Zipcode >= l AND data1.Zipcode < u")
  
  my_small_table = sqldf("SELECT sum((data2.CubicInches)*(data2.Units)) AS my_small FROM my_data2 data2 WHERE data2.Weight < 60")
  my_sixty_table = sqldf("SELECT sum((data2.CubicInches)*(data2.Units)) AS my_sixty FROM my_data2 data2 WHERE data2.Weight = 60")
  my_bulk_table = sqldf("SELECT sum((data2.CubicInches)*(data2.Units)) AS my_bulk FROM my_data2 data2 WHERE data2.Weight > 60")
  
  output = data.frame(small = my_small_table$my_small, sixty = my_sixty_table$my_sixty, 
                      bulk = my_bulk_table$my_bulk)
  
  return(output)
}