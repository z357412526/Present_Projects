#http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz
#tar zxvf 1987-2008.csvs.tgz
#ls -s -h airline.db

f_index=(2,4,6,9,17,18)
for year in {1987..2008};
    do bunzip2 -c ${year}.csv.bz2 | cut -d',' -f$f_index | bzip2 > new${year}.csv.bz2 
    done

#ls -s -h airline2.db