#以后写xpath得根据curl获取的网页的xpath写，不能直接用网页浏览器打开的xpath写，两者可能会有差异

options(scipen = 200)
args <- commandArgs(trailingOnly = TRUE)
order_id = as.integer(args[1])
total_times = as.integer(args[2])

if(length(args) < 2) {
  stop("输入参数数量应该两个，第x1部分, 共x2部分")
}

print(sprintf("Running %s of %s...", order_id, total_times))

library(rvest)
library(tidyverse)
library(stringr)
library(methods)
library(foreach)
library(curl)
library(jsonlite)
library(data.table)

# if mac or linux
# library(doMC)
# registerDoMC(cores=4)

# if windows
library(doParallel)
# #the following line will create a local 4-node snow cluster
# workers=makeCluster(4,type="SOCK")
# registerDoParallel(workers)

#order_id = 1
#total_times = 100000

input_file = 'haodf/consult_list_mobile_0331.csv'

if(.Platform$OS.type=="windows"){ 
    cmd<-system(paste("C:/RTools/bin/wc -l",input_file), intern=TRUE) 
    n <- as.integer(strsplit(cmd, " ")[[1]][1]) 
}else{
  ##建立连接
con <- file(description=input_file, open="r")
## 计算行数
com <- paste("wc -l ", input_file, " | awk '{ print $1 }'", sep="")
n <- system(command=com, intern=TRUE)
}

#consult.list.all <- fread(input_file, sep = ',', header = TRUE,
#                          stringsAsFactors = FALSE)
##先取100行测试
#consult.list.all <- consult.list.all[4559179:4559189,]
#consult.list <- unique(consult.list.all)
#write.csv(consult.list, 'consult_list_mobile_0331_afu.csv', na = '', row.names = F)


#清除变量，释放内存
#rm(consult.list.all)
#gc()
#consult.count <- nrow(consult.list)

if (!is.na(order_id)){
  group.count = total_times
  
}else{
  order_id = 1 
  group.count = 1000 
}
#计算每一块的行数
eachgroup.num = ceiling(n/group.count)
#计算每一块需要跳过的行数
skip_lines = eachgroup.num*(order_id-1)
if(order_id == 1){
  skip_lines = skip_lines + 1
}
#consult.list$batch_id <- rep(1:group.count, each = eachgroup.num, length = consult.count)
#consult.list.part <- consult.list %>% filter(batch_id == order_id)
#rm(consult.list)
#gc()


doctorCrawler <- function(dc.consult.each, retry_times = 10) {
  
  consult.info.1 = list()
  consult.info.1$doctor.name <- dc.consult.each$doctor.name
  consult.info.1$doctor.id <- dc.consult.each$doctor.id
  consult.info.1$link <- dc.consult.each$link
  consult.info.1$l_number <- dc.consult.each$l_number
  print(consult.info.1$doctor.id)
  print(consult.info.1$l_number)
  
  
  #if(dc.consult.each$link != '' & nchar(dc.consult.each$link)>0){
  if(!is.na(dc.consult.each$link)){
    if(nchar(dc.consult.each$link)> 0){
      #获取咨询的id
      dc.consult.id <- str_extract(string = dc.consult.each$link, pattern = regex("\\d+(?=.htm)")) 
    }
  }
  else{
    dc.consult.content <- data.frame(consult.info.1, stringsAsFactors = F) 
    return()
  }
  
  
  consult.start.url <- sprintf("https://m.haodf.com/touch/case/loadmoredetail?orderdesc=0&refid=%s&p=1", dc.consult.id)
  
  req <- curl_fetch_memory(consult.start.url)
  df = try(rawToChar(req$content) %>% fromJSON())
  try_time = 0
  while(class(df) == 'try-error'){
    req <- curl_fetch_memory(consult.start.url)
    df = try(rawToChar(req$content) %>% fromJSON())
    print(sprintf("This is the %s time for trying...", try_time))
    try_time = try_time + 1
    Sys.sleep(sample(c(1:10), size = 1)/10*try_time)
    if(try_time > retry_times){
      return()
      
    }
    
  }
  
  pages <- df$pageInfo$pages
  
  if(pages==0){
    return()
  }
  consult.content <- ''
  #outfile2 = file("consult.content.0401.html", encoding = 'UTF8', open = 'a+')
  
  #j表示该医生的咨询总页数
  for(j in 1 : pages){
    #for(j in 1 : 15){
    consult.url <- sprintf('https://m.haodf.com/touch/case/loadmoredetail?orderdesc=0&refid=%s&p=%s', dc.consult.id, j)
    #consult.url <- sprintf('https://m.haodf.com/touch/case/loadmoredetail?orderdesc=0&refid=4815727835&p=%s', j)
    #retry_times = 10
    req.1 <- curl_fetch_memory(consult.url)
    content1 <- try(rawToChar(req.1$content) %>% fromJSON())
    try_time1 = 0
    while(class(content1) == 'try-error'){
      req.1 <- curl_fetch_memory(consult.url)
      content1 <- try(rawToChar(req.1$content) %>% fromJSON())
      print(sprintf("This is the %s time for trying...", try_time1))
      try_time1 = try_time1 + 1
      Sys.sleep(sample(c(1:10), size = 1)/10*try_time1)
      if(try_time1 > retry_times){
        break()
      }
    }
    
    if(!class(content1) == 'try-error'){
      content_text <- content1$content
      consult.content <- paste(consult.content, content_text, collapse = ' ')
    }
    #writeLines(content_text, outfile2)
  }
  #close(outfile2)
  if(length(consult.content) == 0){
    return()
  }else if(!str_detect(consult.content, "<div")){
    return()
  }
  consult.url <- consult.content
  consult.cont.html <- read_html(consult.url, encoding = 'UTF-8') 
  consult.content <- consult.cont.html %>% 
    html_nodes(xpath = '/html/body/div[@class="feed-block"]') 
  
  
  xpath.time = './div[@class="feed-caption"]/strong/text()' 
  
  
  #xpath.patient = './div[@class="feed-caption"]/span/text()'
  #xpath.consult.cat = './div[@class="main-feed"]/div[@class="feed-patient-cot"]/p/strong/text()|./div[@class="card-theme1"]/div[@class="card1-box"]/span/text()|./div[@class="main-feed"]/div[@class="feed-address"]/text()'
  xpath.consult.cat = './div[@class="card-theme1"]/div[@class="card1-box"]/span/text()|./div[@class="main-feed"]/div[@class="feed-address"]/text()'
  #xpath.consult.cat1 = './div[@class="card-theme1"]/div[@class="card1-box"]/span/text()'
  #xpath.consult.cat2 = './div[@class="main-feed"]/div[@class="feed-address"]/text()'
  
  
  xpath.consult.start.finish = './div/section/div[@class="flow-division"]/span[@class="f-d-desc"]/text()'
  xpath.ask.cont = './div[@class="main-feed"]/div[@class="feed-patient-cot"]/p'
  #xpath.doc.name = './div[@class="feed-doctor"]/div/div[@class="feed-doctor-title"]/a/strong/text()'              
  xpath.doc.answ = './div[@class="feed-doctor"]/div[@class="feed-doctor-cot"]/div[@class="feed-doctor-block"]/div[@class="feed-doctor-content"]'
  xpath.pa.gift = './div[@class="feed-heart"]/div[@class="feed-heart-cot"]/div/span/text()'
  xpath.pa.letter = './div[@class="feed-mail"]/div[@class="feed-mail-cot"]/div/text()'
  
  #consult.content1 <- consult.cont.html %>% 
  #  html_nodes(xpath = '/html/body/div[@class="feed-block feed-new-block"]')
  #xpath1.time = './div[@class="feed-caption"]/strong/text()'
  #xpath.consult.cat1 = './div[@class="main-feed"]/div[@class="feed-patient-title"]/text()'
  
  
  
  consult.info.1$consult.time <- consult.content %>%
    html_node(xpath = xpath.time)%>%
    trimws()
  #对“今天”，“昨天”以及同年不显示年份的情况进行处理
  
  consult.info.1$consult.time <- gsub("今天", format(Sys.Date(), format = '%Y.%m.%d'), consult.info.1$consult.time) %>% gsub('/','.', .)
  consult.info.1$consult.time <- gsub("昨天", format(Sys.Date() - 1, format = '%Y.%m.%d'), consult.info.1$consult.time) %>% gsub('/','.', .)
  consult.info.1$consult.time[nchar(consult.info.1$consult.time) < 10 & (!is.na(consult.info.1$consult.time))] <- paste(lubridate::year(Sys.Date()), consult.info.1$consult.time[nchar(consult.info.1$consult.time) < 10& (!is.na(consult.info.1$consult.time))], sep = '.')
  
  #consult.info.1$consult.patient <- consult.content %>%
  #  html_node(xpath = xpath.patient) %>%
  #  trimws()
  
  consult.info.1$consult.cat <- consult.content %>%
    html_node(xpath = xpath.consult.cat) %>%
    trimws() %>%
    gsub(',|，', '', x = .)
  #consult.info.1$consult.cat1 <- consult.content %>%
  #  html_node(xpath = xpath.consult.cat1) %>%
  #  trimws()
  #consult.info.1$consult.cat2 <- consult.content %>%
  #  html_node(xpath = xpath.consult.cat2) %>%
  #  trimws()
  #consult.info.1$consult.cat3 <- consult.content1 %>%
  #  html_node(xpath = xpath.consult.cat1) %>%
  #  trimws()
  
  
  consult.info.1$consult.start.finish <- consult.content %>%
    html_node(xpath = xpath.consult.start.finish) %>%
    trimws()
  
  consult.info.1$consult.ask.cont <- consult.content %>%
    html_node(xpath = xpath.ask.cont) %>%
    html_text() %>%
    #unlist(.)%>%
    trimws() %>%
    gsub('"|“|”', '', x = .)
  #paste(x = .,collapse = '')
  
  #consult.info.1$consult.doc.name <- consult.content %>%
  #  html_node(xpath = xpath.doc.name) %>%
  #  trimws()
  consult.info.1$consult.doc.answ <- consult.content %>%
    html_node(xpath = xpath.doc.answ) %>%
    html_text() %>%
    #unlist(.)%>%
    trimws() %>%
    gsub('"|“|”', '', x = .)
  #paste(x = .,collapse = '')
  consult.info.1$consult.gift <- consult.content %>%
    html_node(xpath = xpath.pa.gift) %>%
    trimws()
  # 1 代表医生收到了礼物
  consult.info.1$consult.gift <- 1 * (!is.na(consult.info.1$consult.gift))
  consult.info.1$consult.letter <- consult.content %>%
    html_node(xpath = xpath.pa.letter) %>%
    trimws()
  #1 代表有感谢信，0代表无
  consult.info.1$consult.letter <- 1 * (!is.na(consult.info.1$consult.letter))
  consult.info.1$date <- Sys.Date()
  #consult.info.1$id <- consult.no
  
  consult.info.2 = lapply(consult.info.1, function(x) {if(length(x)==0) return(NA) else x})
  dc.consult.content <- data.frame(consult.info.2, stringsAsFactors = F) %>%
    dplyr::rename(
      time = consult.time,
      #patient = consult.patient,
      category = consult.cat,
      #category3 = consult.cat3,
      #category1 = consult.cat1,
      #category2 = consult.cat2,
      ask.content = consult.ask.cont,
      consult.strt.finish = consult.start.finish,
      #doc.name = consult.doc.name,
      doc.answer = consult.doc.answ,
      gift = consult.gift,
      letter = consult.letter
    )
  
  return(dc.consult.content)
  
  
}

doctorCrawlerLoop <- function(input_file, skip_lines, eachgroup.num) {
  #remDr$open(silent = T)
  #m <- nrow(consult_list)
  #result.list <- list()
  con <- file(description=input_file, open="r")
  
  
  
 
  
  ##找出当前结果文件，取时间最新的文件，找出最后的医生，下次运行从这个医生开始运行
  file.list <- list.files(getwd(), pattern = sprintf(".+part_%s_of_%s\\..+", order_id, group.count))
  #若向量的长度>0，说明之前跑过这一块存在
  if(length(file.list) > 0){
    #根据字符串大小，选出最新的文件
    input_file1 = max(file.list)
    #打印出这个文件名
    print(input_file1)
    #若为windows
    cmd1<-system(paste("C:/RTools/bin/wc -l",input_file1), intern=TRUE) 
    n1<-as.integer(strsplit(cmd1, " ")[[1]][1])
    ##建立连接
    con1 <- file(description=input_file1, open="r")
    ## 计算行数-linux
    #com1 <- paste("wc -l ", input_file1, " | awk '{ print $1 }'", sep="")
    #n1 <- system(command=com1, intern=TRUE)
    #consult.content.now <- fread(input_file1, sep = ',', header = TRUE,
    #                             stringsAsFactors = FALSE)
    
    #last.line <- scan(file=con1, nlines=1, skip = n1-1, quiet=TRUE, sep = ',')
    colname.line1 <- read.table(con1, nrow = 1, sep = ',', header = F, stringsAsFactors = F)
    colname.line2 <- as.matrix(colname.line1)
    colname.line <- as.vector(colname.line2)
    last.line <- read.table(con1, nrow = 1, skip = n1-2, sep = ',', stringsAsFactors = F)
    colnames(last.line) <- colname.line
    #colnames(last.line) <- first.line
    last.number <- last.line$l_number
    #restart.number <- as.integer(which(consult.list.part$l_number == last.number)+1)
    #consult.content.now1 <- consult.content.now[!consult.content.now$l_number == last.number, ]
    #write.csv(consult.content.now1, file = input_file1, row.names = F, na = '')
    #consult.list.part <- consult.list.part[restart.number:nrow(consult.list.part), ]
    skip_lines <- last.number
    finished <- eachgroup.num*order_id-last.number
    
   }else{
     finished <- eachgroup.num
   }
  
  i = 1
  first.line1 <- read.table(con, nrow = 1, sep = ',', header = F, stringsAsFactors = F)
  first.line2 <- as.matrix(first.line1)
  first.line <- as.vector(first.line2)
  
  dc.consult.each <- read.table(con, skip = skip_lines, nrow = 1, sep = ',', stringsAsFactors = F)
  colnames(dc.consult.each) <- first.line
  #print(dc.consult.each)
  result.list <- doctorCrawler(dc.consult.each)
  print(sprintf("Crawling %s of %s at %s", i, finished, Sys.time()))
  
  file_name = sprintf('consult_content.part_%s_of_%s.%s.csv', 
                      order_id, total_times, gsub(' |:|-', '.', Sys.time()))
  if(length(result.list)==0){
    result.list1 <- data.frame(doctor.name=character(0), doctor.id=character(0), link=character(0), l_number=numeric(0), time=character(0), category=character(0), 
                               consult.strt.finish=character(0), ask.contnet=character(0), doc.answer=character(0), gift=numeric(0), letter=numeric(0), date=character(0), stringsAsFactors = F)
    #c('doctor.name', 'doctor.id', 'link', 'l_number', 'time', 'category', 'consult.strt.finish', 'ask.contnet', 'doc.answer', 'gift', 'letter', 'date')
    write.table(result.list1, file = file_name, sep = ',',
                na = '', row.names = F, col.names = T, append = F)
  }
  write.table(result.list, file = file_name, sep = ',',
              na = '', row.names = F, col.names = T, append = F)
  
  for (i in 2: finished){
    #      if(!is.na(consult_list$link[i])){
    #result.list[[i]] <- doctorCrawler(consult_list[i,], i)
    
    dc.consult.each <- read.table(con, nrow = 1, sep = ',', stringsAsFactors = F)
    colnames(dc.consult.each) <- first.line
    #print(dc.consult.each)
    #现在不拼接，直接将调用的结果写出
    result.list <- doctorCrawler(dc.consult.each)
    print(sprintf("Crawling %s of %s at %s", i, finished, Sys.time()))
    if(length(result.list) > 0){
      write.table(result.list, file = file_name, sep = ',',
                  na = '', row.names = F, col.names = F, append = T)
      
    }
    
    #        }
    #      else
    
  }
  #result.df <- plyr::rbind.fill(result.list)
  #remDr$close()
  #return(result.df)
}


#result.data <- doctorCrawlerLoop(consult.list.part)
doctorCrawlerLoop(input_file, skip_lines, eachgroup.num)
#write.csv(result.data, file = sprintf('consult_content.part_%s_of_%s.%s.csv', 
#                                      order_id, total_times, gsub(' |:|-', '.', Sys.time())), 
#          na = '', row.names = F)

