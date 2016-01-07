#login the EC2
#ssh -i ~/.ssh/stat243-fall-2015-ssh_key.pem ubuntu@52.32.206.176

#Using the database and index creating in the part 2
library(RSQLite)
library(parallel) # one of the core R packages
library(doParallel)
library(foreach)
library(iterators)

drv = dbDriver("SQLite")
db <- dbConnect(drv, dbname = "Airlines.db")
#it s reasonable to using month as the key for parallelizing, as we can expect
#that for each month, the number of flight is almost the same. By doing this, 
# each node will be assigned simiar load of work, which can optimize our 
#efficiency
qry <- "SELECT UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as per30,sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as per60,sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as per180,COUNT (*) AS total FROM airline_data2 where Month in"
count(DepDelay) as Time_Delay from year where DepDelay!='NA' and Month in 

taskFun_Month <- function(i){
    tmp<-paste(qry,"(",toString(i),',',toString(i+4),',',toString(i+4*2),")",
    	"GROUP BY UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime",sep='')
    db_tmp<-dbConnect(drv,dbname="Airlines.db")
    stat_tmp<-dbGetQuery(db_tmp,tmp)
    return(stat_tmp)
  }
system.time(
  res1 <- mclapply(1:4, taskFun_Month, mc.cores = 4) 
) 
#    user  system elapsed 
# 369.752  40.272 204.517 
