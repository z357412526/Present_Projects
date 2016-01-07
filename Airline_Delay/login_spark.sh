#this is path where your stat243-fall-2015-credentials.boto located
cd /Users/Xinyue_star/Desktop/stat_243/ps6-R
export AWS_ACCESS_KEY_ID=`grep aws_access_key_id stat243-fall-2015-credentials.boto | cut -d' ' -f3`
export AWS_SECRET_ACCESS_KEY=`grep aws_secret_access_key stat243-fall-2015-credentials.boto | cut -d' ' -f3`

#this is the path your download your spark
cd ~/src/InClass/spark-1.5.1/ec2
export NUMBER_OF_WORKERS=12
./spark-ec2 -k xinyue233@berkeley.edu:stat243-fall-2015 -i ~/.ssh/stat243-fall-2015-ssh_key.pem --region=us-west-2 -s ${NUMBER_OF_WORKERS} -v 1.5.1 launch sparkvm-xinyue233

./spark-ec2 -k xinyue233@berkeley.edu:stat243-fall-2015 -i ~/.ssh/stat243-fall-2015-ssh_key.pem --region=us-west-2 login sparkvm-xinyue233
