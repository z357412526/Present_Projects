export PATH=$PATH:/root/ephemeral-hdfs/bin/ 
hadoop fs -mkdir /data
hadoop fs -mkdir /data/airline
df -h
mkdir /mnt/airline
cd /mnt/airline

wget http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz
tar -xvzf 1987-2008.csvs.tgz

hadoop fs -copyFromLocal /mnt/airline/*bz2 /data/airline
# check files on the HDFS, e.g.:
hadoop fs -ls /data/airline
# get numpy installed
# there is a glitch in the EC2 setup that Spark provides -- numpy is not ins
yum install -y python27-pip python27-devel
pip-2.7 install 'numpy==1.9.2'  # 1.10.1 has an issue with a warning in medi
/root/spark-ec2/copy-dir /usr/local/lib64/python2.7/site-packages/numpy
# pyspark is in /root/spark/bin
export PATH=${PATH}:/root/spark/bin
# start Spark's Python interface as interactive session
pyspark