wget http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz
tar -xvzf 1987-2008.csvs.tgz


#to logout destroy
# cd /Users/Xinyue_star/Desktop/stat_243/ps6-R
# export AWS_ACCESS_KEY_ID=`grep aws_access_key_id stat243-fall-2015-credentials.boto | cut -d' ' -f3`
# export AWS_SECRET_ACCESS_KEY=`grep aws_secret_access_key stat243-fall-2015-credentials.boto | cut -d' ' -f3`

# cd ~/src/InClass/spark-1.5.1/ec2
# export mycluster=sparkvm-xinyue233
# ./spark-ec2 --region=us-west-2 --delete-groups destroy ${mycluster}

from operator import add
import numpy as np
lines = sc.textFile('/data/airline')
#filter out the header and NA values
def screen(vals):
	vals = vals.split(',')
	return (vals[15] != 'NA' and vals[15] != 'DepDelay')

lines = lines.filter(screen).repartition(96).cache()

#mapper: return to a pair fo values. (key value, return values)
def computeKeyValue(line):
	vals = line.split(',')
	#key is UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime
	vals[5] = vals[5][:-2]
	keyVals = '-'.join([vals[x] for x in [8,16,17,1,3,5]])
	count1 = 0
	count2 = 0
	count3 = 0
	count4 = 1
	if float(vals[15]) > 30:
		count1 = 1
	if float(vals[15]) > 60:
		count2 = 1
	if float(vals[15]) > 180:
		count3 = 1
	return(keyVals, np.asarray([count1, count2, count3, count4]))

mapIt = lines.map(computeKeyValue).reduceByKey(np.add)
mapIt.take(5)
# Those are unique combo of keys
# key is UniqueCarrier, Dest, Origin, Month, DayOfWeek, CRSDepTime
# [(u'EA-ATL-MLB-12-2-17', array([0, 0, 0, 9])), 
# (u'DL-ATL-DAY-11-6-9', array([ 0,  0,  0, 21])), 
# (u'WN-RDU-PHX-1-7-11', array([0, 0, 0, 3])), 
# (u'CO-IAH-MIA-11-1-21', array([ 2,  1,  0, 23])), 
# (u'AS-BUR-SEA-6-3-8', array([ 0,  0,  0, 12]))]

def comma_joint(line):
	vals = ','.join([str(line[0]), str(line[1][0]), str(line[1][1]), str(line[1][2]), str(line[1][3])])
	return(vals)

ret=mapIt.map(comma_joint)
ret.take(5)
# ['TW-STL-ICT-8-6-9,0,0,0,34', 
# 'WN-DEN-MDW-3-2-14,1,1,0,9', 
# 'WN-RDU-PHX-1-7-11,0,0,0,3', 
# 'HA-HNL-LIH-8-3-12,2,0,0,23', 
# 'UA-ORD-PWM-6-7-,0,0,0,9']

import timeit
currenttime=timeit.default_timer()
ret.repartition(1).saveAsTextFile('/data/xinyue233')
stoptime=timeit.default_timer()
stoptime-currenttime
#107.21423101425171

#copy the data in cloud to local
hadoop fs -copyToLocal /data/xinyue233 /mnt/airline
cd /mnt/airline
ls
# 1987-2008.csvs.tgz  1990.csv.bz2  1994.csv.bz2  1998.csv.bz2  2002.csv.bz2  2006.csv.bz2
# 1987.csv.bz2        1991.csv.bz2  1995.csv.bz2  1999.csv.bz2  2003.csv.bz2  2007.csv.bz2
# 1988.csv.bz2        1992.csv.bz2  1996.csv.bz2  2000.csv.bz2  2004.csv.bz2  2008.csv.bz2
# 1989.csv.bz2        1993.csv.bz2  1997.csv.bz2  2001.csv.bz2  2005.csv.bz2  guoyuchao
cd xinyue233/
ls
# part-00000  _SUCCESS
cat part-00000 | head -n5
# WN-SLC-LAS-4-2-15,1,1,0,44
# WN-ALB-BWI-6-6-10,1,0,0,29
# WN-DET-MDW-1-5-8,1,0,0,18
# OH-ATL-MHT-9-5-11,1,1,0,1
# US-JAX-CLT-9-4-13,2,2,0,58
