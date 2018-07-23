#之前漏爬了医生的职称和所在医院的id，补上
library(curl)
library(jsonlite)
library(tidyverse)
library(rvest)
library(stringr)
#library(stringi)

#获取医生的id和医生总数
input_file = '/home/rong/Crawling/Doclist.mobile.0423.utf8.csv' 
doclist.all <- read.csv(input_file, header = TRUE,
                        stringsAsFactors = FALSE)
#doclist.all <- unique(doclist.all)
doctor.count <- nrow(doclist.all)

#随机取100个医生进行测试
#set.seed(1)
#x <- sample(c(1:12473), size = 10)
#x <- 9691
#result.data <- list()

time.record <- list() 
# i表示第i位医生 
for(i in 498:498){
#for(i in 12391:doctor.count){
  time.record[[i]] <- Sys.time()
  print(sprintf("Crawlering No.%s doctor at %s.", i, time.record[[i]]))  
  hosp.info.list.1 <- list()
  
  #加上医生的姓名和id
  hosp.info.list.1$doctor.name <- doclist.all$name[i] 
  hosp.info.list.1$doctor.id <- doclist.all$doctor.id[i]
  
  hosp.inf.source.html <- try(read_html(paste('https:', doclist.all$url[i], sep=''), encoding = 'gbk'))
  
  try_time2 = 0
  while (class(hosp.inf.source.html)=='try-error') {
    hosp.inf.source.html <- try(read_html(paste('https:', doclist.all$url[i], sep=''), encoding = 'gbk'))
    try_time2 = try_time2 +1
    print(sprintf("This is %s time for retrying...", try_time2))
    Sys.sleep(sample(c(1:10), size = 1)/10*try_time2)
    if(try_time2 > 10){
      break()
    }
    
    
  }
  
  
  xpath.hosp.url = '//*[@id="real_body"]/div[@class="hos_now_site"]/a[2]'
  xpath.doc.title = '//*[@id="normal_head_c1"]/div[@class="head_name"]/p/text()'
  
  
  #获取医生所在医院网址
  hosp_url <- hosp.inf.source.html %>%
    html_nodes(xpath = xpath.hosp.url) %>%
    html_attr('href') %>%
    trimws()
  
  
  hosp.info.list.1$hosp.id <- str_extract(hosp_url, pattern = '(?<=//m.haodf.com/touch/hospital/).+(?=.htm)')
  
  try_time = 0
  while (length(hosp.info.list.1$hosp.id)==0) {
    hosp_url <- hosp.inf.source.html %>%
      html_nodes(xpath = xpath.hosp.url) %>%
      html_attr('href') %>%
      trimws()
    hosp.info.list.1$hosp.id <- str_extract(hosp_url, pattern = '(?<=//m.haodf.com/touch/hospital/).+(?=.htm)')
    try_time = try_time +1
    print(sprintf("This is %s time for retrying...", try_time))
    Sys.sleep(sample(c(1:10), size = 1)/10*try_time)
    if(try_time > 10){
      break()
    }
    
  }
  
  
  #获取医生的职称
  hosp.info.list.1$doc.title <- hosp.inf.source.html %>%
    html_node(xpath = xpath.doc.title) %>%
    trimws()
  
  
  try_time1 = 0
  while (length(hosp.info.list.1$doc.title)==0) {
    hosp.info.list.1$doc.title <- hosp.inf.source.html %>%
      html_node(xpath = xpath.doc.title) %>%
      trimws()
    try_time1 = try_time1 +1
    print(sprintf("This is %s time for retrying...", try_time1))
    Sys.sleep(sample(c(1:10), size = 1)/10*try_time1)
    if(try_time1 > 10){
      break()
    }
    
  }
  
  hosp.info <- data.frame(hosp.info.list.1, stringsAsFactors = F) 
  
  
  write.table(hosp.info, '/home/rong/Crawling/doc.info.utf8.0508.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')  
  Sys.sleep(5)
}

