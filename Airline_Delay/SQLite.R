
library(RSQLite) 
install.packages("data.table") 
library(data.table)
fileName <- "Airlines.db"
db <- dbConnect(SQLite(), dbname = fileName) 
initExtension(db)

#try for a full  case
tableCL <- c(rep("integer",8), "factor","integer","logical","integer",
	"integer","logical","integer","integer","factor","factor","integer","logical",
	 "logical","integer","logical","integer",rep("logical",5))

for (i in 1987:2008){
  unzip_name <- paste("bunzip2 -c ", i,".csv.bz2",sep="")
  tmp <- fread(unzip_name, header = TRUE, colClasses=tableCL)
  dbWriteTable(conn = db, name = "Airlines", value = tmp, append=TRUE, row.names = FALSE)
  print(i)
}

dbListTables(db)
#"Airlines"
dbListFields(db, "Airlines")
# [1] "Year"              "Month"             "DayofMonth"       
# [4] "DayOfWeek"         "DepTime"           "CRSDepTime"       
# [7] "ArrTime"           "CRSArrTime"        "UniqueCarrier"    
# [10] "FlightNum"         "TailNum"           "ActualElapsedTime"
# [13] "CRSElapsedTime"    "AirTime"           "ArrDelay"         
# [16] "DepDelay"          "Origin"            "Dest"             
# [19] "Distance"          "TaxiIn"            "TaxiOut"          
# [22] "Cancelled"         "CancellationCode"  "Diverted"         
# [25] "CarrierDelay"      "WeatherDelay"      "NASDelay"         
# [28] "SecurityDelay"     "LateAircraftDelay"
file.size("Airlines.db")
#9399877632

#some about SQLite
db_date_new <- dbSendQuery(db, "UPDATE Airlines SET CRSDepTime = floor(CRSDepTime/100)")
db_filter <- dbSendQuery(db, 
	"CREATE VIEW airline_data1 AS SELECT * FROM Airlines WHERE DepDelay != 'NA'")

system.time(db_table <-dbSendQuery(db,
"CREATE TABLE table1 AS SELECT UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime,
sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) AS per30,
sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) AS per60,
sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) AS per80,
COUNT (*) AS total 
FROM airline_data1 GROUP BY UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime"))

# user  system elapsed 
# 674.776  32.260 742.649 

##indexing here
system.time(db_index <- dbSendQuery(db, 
	"CREATE INDEX indices ON Airlines (UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime)"))
#    user  system elapsed 
# 506.852  38.468 580.333 

#The index is something which the optimizer picks up "automagically 
#- ideally you don't need to force select an index.
system.time(db_filter2 <- dbSendQuery(db, 
	"CREATE VIEW airline_data2 AS SELECT * FROM Airlines WHERE DepDelay != 'NA'"))
  #  user  system elapsed 
  # 0.004   0.000   0.002 

#comparing with the time using in previous part, I found that the running time after indexing is 
#highly improved. It's about half of previous one.
#Well, indexing process costs lots of time though. So, if there is no further selecting processes
#indexing does not make difference from non-indexing methods
system.time(db_table2 <-dbSendQuery(db,
"CREATE TABLE table2 AS SELECT UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime,
sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) AS per30,
sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) AS per60,
sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) AS per180,
COUNT (*) AS total 
FROM airline_data2 GROUP BY UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime"))

#    user  system elapsed 
# 245.876  53.984 403.838 

system.time(top_delay30 <- dbSendQuery(db, 
	"SELECT * FROM table2 WHERE total >= 150 ORDER BY per30 DESC LIMIT 5"))
fetch(top_delay30, 5)

#   UniqueCarrier Dest Origin Month DayOfWeek CRSDepTime     per30     per60
# 1            WN  HOU    DAL     6         5         20 0.4125000 0.1750000
# 2            WN  DAL    HOU     2         5         19 0.4039735 0.1192053
# 3            WN  HOU    DAL     4         5         20 0.3800000 0.2066667
# 4            WN  HOU    DAL     6         5         21 0.3750000 0.1447368
# 5            WN  DAL    HOU     6         5         19 0.3680982 0.1533742
#        per180 total
# 1 0.000000000   160
# 2 0.006622517   151
# 3 0.020000000   150
# 4 0.000000000   152
# 5 0.000000000   163
dbClearResult(top_delay30)

top_delay60 <- dbSendQuery(db, 
	"SELECT * FROM table2 WHERE total >= 150 ORDER BY per60 DESC LIMIT 5")
fetch(top_delay60, 5)

#   UniqueCarrier Dest Origin Month DayOfWeek CRSDepTime     per30     per60
# 1            UA  SFO    LAX    12         5         11 0.3641975 0.2222222
# 2            WN  HOU    DAL     4         5         20 0.3800000 0.2066667
# 3            UA  SFO    LAX    10         5         16 0.3178808 0.1986755
# 4            UA  SFO    LAX    12         5         18 0.3375000 0.1937500
# 5            AA  LAX    ORD     1         4          0 0.2817680 0.1878453
#       per180 total
# 1 0.00617284   162
# 2 0.02000000   150
# 3 0.00000000   151
# 4 0.01250000   160
# 5 0.03314917   181


top_delay180 <- dbSendQuery(db, 
	"SELECT * FROM table2 WHERE total >= 150 ORDER BY per180 DESC LIMIT 5")
fetch(top_delay180, 5)

#   UniqueCarrier Dest Origin Month DayOfWeek CRSDepTime     per30      per60
# 1            AA  ORD    BOS    12         2          0 0.1116751 0.06598985
# 2            AA  LGA    ORD    12         3          0 0.2033898 0.11299435
# 3            AA  DFW    ORD     1         4          0 0.1907895 0.11184211
# 4            AA  ORD    LGA    12         3          0 0.1027027 0.06486486
# 5            AA  LGA    ORD     1         4          0 0.2192513 0.11229947
#       per180 total
# 1 0.04568528   197
# 2 0.03954802   177
# 3 0.03947368   304
# 4 0.03783784   185
# 5 0.03743316   187
dbClearResult(top_delay180)
#[1] TRUE






















