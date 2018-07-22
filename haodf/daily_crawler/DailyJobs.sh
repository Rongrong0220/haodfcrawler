#!/bin/bash
set -x 
# docker run -d -p 4444:4444 --shm-size=1.2g -e JAVA_OPTS=-Xmx1024m selenium/standalone-chrome:3.8.1-francium 
# JAVA_OPTS=-Xmx1024m
# docker run -d -p 4444:4444 -e JAVA_OPTS=-Xmx512m --name selenium-hub selenium/hub:3.8.1-francium 
docker restart 85665970d638 
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

logfile=$DIR/crawler.$(date +%Y%m%d).log

echo "begin" >> $logfile 
echo $(date) >> $logfile 

$DIR/DailyJobs.1.sh 3 > $logfile

sleep 60
$DIR/DailyJobs.2.sh 3 >> $logfile

sleep 60

echo "结果开始转码" >> $logfile
echo $(date) >> $logfile
for f in `ls $DIR/daily_data_$(date +%Y%m%d/)|grep csv`
do
#    echo ${f/csv/gbk.csv}
    piconv $DIR/daily_data_$(date +%Y%m%d/)$f -f utf8 -t gbk > $DIR/daily_data_$(date +%Y%m%d/)${f/csv/gbk.csv}

done
echo "结果转码成功 准备发邮件" >> $logfile
echo $(date) >> $logfile

Rscript $DIR/ResultMail.R
echo "end" >> $logfile
echo $(date) >> $logfile 
docker restart 85665970d638 

