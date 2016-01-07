#1
#put this into bash
#http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz
#this is to get individual file for years
#tar zxvf 1987-2008.csvs.tgz
install.packages("DBL")
install.packages("data.table")
require(DBL)
require(data.table)

library(RSQLite)
filename<-"airline.db"
drv<-dbDriver("SQLite")
db<-dbConnect(drv,dbname=filename)

#unzip a file for a year to see the classes of column in it.
tmp <- read.csv(bzfile("1987.csv.bz2"))
sapply(tmp, class)
tableCL <- c(rep("integer",8), "factor","integer","logical","integer",
	"integer","logical","integer","integer","factor","factor","integer","logical",
	 "logical","integer","logical","integer",rep("logical",5))
years <- seq(1987, 2008)

system.time(for (i in years){
	print(i)
	#
	nm<-paste("bunzip2 -c ",toString(i),".csv.bz2",sep="")
	dt <- fread(nm,header=TRUE, colClasses=tableCL)
	dbWriteTable(conn=db,value=dt,name="airlines_data",
		row.names=FALSE,append=TRUE)
})
rm(dt)

# user  system elapsed 
# 863.508  38.340 903.281 
#run this in bash
#ls -s -h airline.db
# 8.8G airline.db

#4
#take a look at fields in database:
dbListFields(db,"airlines_data")
#the following are the fields we are interested in 
#2."Month", 4."DayOfWeek", 6."CRSDepTime"  
#9. "UniqueCarrier"  17."Origin" 18."Dest"

#this is writen in bash
#f_index=(2,4,6,9,17,18)

# for year in {1987..2008};
#     do bunzip2 -c ${year}.csv.bz2 | cut -d',' -f$f_index | bzip2 > new${year}.csv.bz2 
#     done

#time bash filter.sh
#real	10m5.494s
#user	12m8.324s
#sys	0m24.248s

filename<-"airline2.db"
drv<-dbDriver("SQLite")
db2<-dbConnect(drv,dbname=filename)

tableCL2 <- c(rep("integer",3),rep("factor",3))

system.time(for (i in years){
	print(i)
	nm<-paste("bunzip2 -c new",toString(i),".csv.bz2",sep="")
	dt <- fread(nm,header=TRUE, colClasses=tableCL2)
	dbWriteTable(conn=db2,value=dt,name="airlines_data_preprocessed",
		row.names=FALSE,append=TRUE)
})
rm(dt)

#time using
#   user  system elapsed 
#246.288  11.060 257.901 




































