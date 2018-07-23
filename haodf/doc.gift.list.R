library(curl)
library(jsonlite)
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)

#获取医生的id和医生总数
input_file = '/home/weirain/Projects/HC/Doclist.mobile.utf8.0423.csv' 
doclist.all <- read.csv(input_file, header = TRUE,
                        stringsAsFactors = FALSE)
#doclist.all <- unique(doclist.all)
doctor.count <- nrow(doclist.all)

#随机取100个医生进行测试
#set.seed(1)
#x <- sample(c(1:12473), size = 10)
#print(x)
#x <- 9691
#result.data <- list()

time.record <- list() 
# i表示第i位医生 
for(i in 1:doctor.count){
#for(i in x){
    #i = 1
    time.record[[i]] <- Sys.time()
    print(sprintf("Crawlering No.%s doctor at %s.", i, time.record[[i]]))  
    gift.list.1 <- list()
    
    #xpath.pages = '/html/body/div/table[last()]/tbody/tr/td/div/a[@class="p_text"]'
    #在表中加上医生的姓名和医生的id，以便之后的表间关联
    gift.list.1$doctor.name <- doclist.all$name[i] 
    gift.list.1$doctor.id <- doclist.all$doctor.id[i]
    #找到礼物列表的第一页并获取总页数
    gift.start.url <- sprintf('http://www.haodf.com/api/doctor/%s/ajaxgetpresentlist.htm?p=1', doclist.all$doctor.id[i])
    #gift.start.url <- 'http://www.haodf.com/api/doctor/DE4r0BCkuHzdehbX-b8hoLSNZn1O5/ajaxgetpresentlist.htm?p=1'
    #pages <- read_html(gift.start.url) %>%
    #    html_node(xpath = xpath.pages) %>%
    #    html_text() %>%
    #    trimws()
    
    req.1 <- curl_fetch_memory(gift.start.url)
    df1 = try(stri_conv(rawToChar(req.1$content), 'GBK', 'UTF-8'))
    #outfile = file("test.0429.html")
    #writeLines(df1, outfile)
    #close(outfile)
    try_time = 1
    while(class(df1) == 'try-error'){
        req.1 <- curl_fetch_memory(gift.start.url)
        df1 = try(stri_conv(rawToChar(req.1$content), 'GBK', 'UTF-8'))
        print(sprintf("This is the %s time for retrying...", try_time))
        try_time = try_time + 1
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time)
        if(try_time > 10){
            break()
        }
    }
    
    pages <- str_extract(df1, pattern = '(?<=共).+(?=页)') %>%
        str_extract('\\d+')
    try_time1 = 1
    while (is.na(pages)) {
        req.1 <- curl_fetch_memory(gift.start.url)
        df1 = try(stri_conv(rawToChar(req.1$content), 'GBK', 'UTF-8'))
        pages <- str_extract(df1, pattern = '(?<=共).+(?=页)') %>%
            str_extract('\\d+')
        print(sprintf("This is the %s time for retrying...", try_time1))
        try_time1 = try_time1 + 1
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time1)
        if(try_time1 > 10){
            break()
        }
    }
    if(is.na(pages)){
        next()
    }
    print(sprintf("This doctor has %s pages gift list!", pages))

    gift.list.all <- ''
    
    for(j in 1 : pages){
        print(sprintf("Crawling doctor's %s page gift list!", j))
        gift.url <- sprintf('http://www.haodf.com/api/doctor/%s/ajaxgetpresentlist.htm?p=%s', doclist.all$doctor.id[i], j)
        req <- curl_fetch_memory(gift.url)
        df = try(stri_conv(rawToChar(req$content), 'GBK', 'UTF-8'))
        try_time2 = 1
        while(class(df) == 'try-error'){
            req <- curl_fetch_memory(gift.url)
            df = try(stri_conv(rawToChar(req$content), 'GBK', 'UTF-8'))
            print(sprintf("This is the %s time for retrying...", try_time2))
            try_time2 = try_time2 + 1
            Sys.sleep(sample(c(1:10), size = 1)/10*try_time2)
            if(try_time2 > 10){
                break()
            }
        }
        
        gift.list.all <- paste(gift.list.all, df, collapse = ' ')
        
    }
    
    gift.time <- unlist(str_extract_all(gift.list.all, pattern = '时间：[0-9]{4}-[0-9]{2}-[0-9]{2}')) %>%
        gsub('时间：', '', .) 
     
    if(length(gift.time) == 0){
        next()
    }   
    gift.list.1$gift.time <- gift.time
       
    gift.list.1$date <- Sys.Date()
        # consult.2 = lapply(consult.info.1, function(x) {if(length(x)==0) return(NA) else x})
        
    dc.gift.list <- data.frame(gift.list.1, stringsAsFactors = F)
    
    write.table(dc.gift.list, 'doc.gift.list.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        #result.data[[i]] <- dc.article.list
}

