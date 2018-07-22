#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#开始时间

begin=$(date +%s)
echo $(date)
group_num=$1


#批量函数

function run_crawler()

{
    Rscript $DIR/daily_2.R $1 $2

}


for ((i=1; i<=$group_num;))

do

    run_crawler $i $group_num &

i=$(expr $i + 1)

done

wait

#结束时间

end=$(date +%s)

spend=$(expr $end - $begin)

echo $(date)
echo "花费时间为$spend秒"
