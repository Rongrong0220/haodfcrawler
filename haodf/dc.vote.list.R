library(curl)
library(jsonlite)
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)

#获取医生的id和医生总数
input_file = 'haodf/Doclist.mobile.0423.af.unique.csv' 
doclist.all <- read.csv(input_file, header = TRUE,
                        stringsAsFactors = FALSE)
doclist.all <- unique(doclist.all)
doctor.count <- nrow(doclist.all)

#随机�?100个医生进行测�?
#set.seed(1)
#x <- sample(c(1:12473), size = 10)
#x <- 9691
#result.data <- list()

time.record <- list() 
# i表示第i位医�? 
#for(i in 1:3){
for(i in 1: doctor.count){
    time.record[[i]] <- Sys.time()
    print(sprintf("Crawlering No.%s doctor at %s.", i, time.record[[i]]))  
    vote.list.1 <- list()
    #获取医生的id
    doctor.id <- str_extract(string = doclist.all$url[i], pattern = regex("(?<=//m.haodf.com/touch/doctor/).+(?=.htm)"))
    #在表中加上医生的姓名和医生的id，以便之后的表间关联
    vote.list.1$doctor.name <- doclist.all$name[i] 
    vote.list.1$doctor.id <- doctor.id 
    #获取每个医生的患者投票列表连�?
    dc.page.info <- curl_fetch_memory(paste0('https:',doclist.all$url[i]))
    dc.page = try(stri_conv(rawToChar(dc.page.info$content),'GBK', 'UTF-8'))
    #以防网络原因，设置重试次�?
    try_time2 = 0
    while(class(dc.page) == 'try-error'){
        dc.page.info <- curl_fetch_memory(paste0('https:',doclist.all$url[i]))
        dc.page = try(stri_conv(rawToChar(dc.page.info$content),'GBK', 'UTF-8'))
        try_time2 = try_time2 + 1
        print(sprintf("This is the %s time for retrying...", try_time2))
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time2)
        if(try_time2 > 10){
            break()
        }
    }
    print(str(dc.page))
    if (class(dc.page) == 'try-error'){
        #dc.vote.list <- data.frame(vote.list.1, stringsAsFactors = F) 
        #write.table(dc.vote.list, 'test.0418.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        next() 
    }

    vote.list.html <- str_extract(string = dc.page, pattern = regex("//m.haodf.com/jingyan/all-.+.htm"))
    #dc.pinyin <- str_extract(string = dc.page, pattern = regex("(?<=//m.haodf.com/jingyan/).+(?=.htm)"))
    #outfile = file("test.0417.html")
    #writeLines(dc.page, outfile)
    #close(outfile)
    
    try_time3 = 0
    while(is.na(vote.list.html)){
        dc.page.info <- curl_fetch_memory(paste0('https:',doclist.all$url[i]))
        dc.page = try(stri_conv(rawToChar(dc.page.info$content),'GBK', 'UTF-8'))
        vote.list.html <- str_extract(string = dc.page, pattern = regex("//m.haodf.com/jingyan/all-.+.htm"))
        try_time3 = try_time3 + 1
        print(sprintf("This is the %s time for retrying...", try_time3))
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time3)
        if(try_time3 > 10){
            break()
        }
    }
    if(is.na(vote.list.html)){
        #dc.vote.list <- data.frame(vote.list.1, stringsAsFactors = F) 
        #write.table(dc.vote.list, 'doc.vote.list.0418.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        next()
    }
    #获取医生的拼音和数字id
    #vote.list.html = 'https://m.haodf.com/jingyan/all-jiangzhaojun-2.htm'
    dc.vote.info <- curl_fetch_memory(paste0('https:',vote.list.html))
    dc.info = try(rawToChar(dc.vote.info$content))

    #outfile1 = file("test.0416.html")
    #writeLines(dc.info, outfile1)
    #close(outfile1)
    #以防网络原因，设置重试次�?
    try_time = 0
    while(class(dc.info) == 'try-error'){
        dc.vote.info <- curl_fetch_memory(vote.list.html)
        dc.info = try(rawToChar(dc.vote.info$content))
        try_time = try_time + 1
        print(sprintf("This is the %s time for retrying...", try_time))
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time)
        if(try_time > 10){
            break()
        }
    }
    if (class(dc.info) == 'try-error'){
        #dc.vote.list <- data.frame(vote.list.1, stringsAsFactors = F) 
        #write.table(dc.vote.list, 'doc.vote.list.0418.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        next() 
    }
    doc.pinyin <- str_extract(string = dc.info, pattern = regex("(?<=doctorPinyin:).+(?=,)")) %>% gsub('"','',x = .)
    doc.id.number <- str_extract(string = dc.info, pattern = regex("(?<=doctorId:).+")) %>% gsub('"','',x = .)
    print(doc.pinyin)
    print(doc.id.number)
    try_time4 = 0
    while(is.na(doc.pinyin)){
        dc.vote.info <- curl_fetch_memory(paste0('https:',vote.list.html))
        dc.info = try(rawToChar(dc.vote.info$content))
        doc.pinyin <- str_extract(string = dc.info, pattern = regex("(?<=doctorPinyin:).+(?=,)")) %>% gsub('"','',x = .)
        try_time4 = try_time4 + 1
        print(sprintf("This is the %s time for retrying...", try_time4))
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time4)
        if(try_time4 > 10){
            break()
        }
    }
    try_time5 = 0
    while(is.na(doc.id.number)){
        dc.vote.info <- curl_fetch_memory(paste0('https:',vote.list.html))
        dc.info = try(rawToChar(dc.vote.info$content))
        doc.id.number <- str_extract(string = dc.info, pattern = regex("(?<=doctorId:).+")) %>% gsub('"','',x = .)
        try_time5 = try_time5 + 1
        print(sprintf("This is the %s time for retrying...", try_time5))
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time5)
        if(try_time5 > 10){
            break()
        }
    }
    if(is.na(doc.pinyin) | is.na(doc.id.number)){
        next()
    }
    
    for(k in 0:1){
        #找到投票列表的第一页并获取总页�?
        print(sprintf("Crawling doctor's %s of 2 years ago", k))
        vote.start.url <- sprintf('https://m.haodf.com/ndoctor/ajaxshowlist?is2YearsAgo=%s&diseasename=%%E5%%85%%A8%%E9%%83%%A8&diseasekey=all&doctorPinyin=%s&doctorId=%s&pinyinRef=%s&sn=&diseaseId=&num=1&size=10', k, doc.pinyin, doc.id.number, doc.pinyin)
        # article.start.url <- 'https://m.haodf.com/touch/doctor/loadmorearticle?id=DE4r0BCkuHzduSEy9dicdu-554j7S&p=1'
        req.1 <- curl_fetch_memory(vote.start.url)
        df1 = try(rawToChar(req.1$content) %>% fromJSON())
        #以防网络原因，设置重试次�?
        try_time = 0
        while(class(df1) == 'try-error'){
            req.1 <- curl_fetch_memory(vote.start.url)
            df1 = try(rawToChar(req.1$content) %>% fromJSON())
            try_time = try_time + 1
            print(sprintf("This is the %s time for retrying...", try_time))
            Sys.sleep(sample(c(1:10), size = 1)/10*try_time)
            if(try_time > 10){
                break()
            }
        }
        if (class(df1) == 'try-error'){
            #dc.vote.list <- data.frame(vote.list.1, stringsAsFactors = F) 
            #write.table(dc.vote.list, 'doc.vote.list.0418.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
            next() 
        }
        pages <- df1$data$pageInfo$totalPage
        print(sprintf("This doctor has total %s pages!",pages))
        vote.list.url.all <- ''
        #outfile1 = file(sprintf("doc.mobile_%s.html", i), encoding = 'UTF8', open = 'a+')
        #j表示该医生的咨询总页�?
        
        #outfile1 = file("test1.0416.html")
        
        if(length(pages) == 0){
            #dc.vote.list <- data.frame(vote.list.1, stringsAsFactors = F) 
            #write.table(dc.vote.list, 'doc.vote.list.0418.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
            next()
        }else if(pages == 0){
            #dc.vote.list <- data.frame(vote.list.1, stringsAsFactors = F) 
            #write.table(dc.vote.list, 'doc.vote.list.0418.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
            next()
        }else{
            for(j in 1 : pages){
                print(sprintf("Crawlering No.%s doctor's page %s.", i, j)) 
                vote.url <- sprintf('https://m.haodf.com/ndoctor/ajaxshowlist?is2YearsAgo=%s&diseasename=%%E5%%85%%A8%%E9%%83%%A8&diseasekey=all&doctorPinyin=%s&doctorId=%s&pinyinRef=%s&sn=&diseaseId=&num=%s&size=10', k, doc.pinyin, doc.id.number, doc.pinyin, j)
                #   article.url <- sprintf('https://m.haodf.com/touch/doctor/loadmorearticle?id=DE4r08xQdKSLBZmGIpMEBi1InoYl&p=1')
                req <- curl_fetch_memory(vote.url)
                content1 <- try(rawToChar(req$content) %>% fromJSON())
                try_time1 = 0
                while(class(content1) == 'try-error'){
                    req <- curl_fetch_memory(vote.url)
                    content1 <- try(rawToChar(req$content) %>% fromJSON())
                    try_time1 = try_time1 + 1
                    print(sprintf("This is the %s time for retrying...", try_time1))
                    Sys.sleep(sample(c(1:10), size = 1)/10*try_time1)
                    if(try_time1 > 10){
                        break()
                    }
                }
                if(!class(content1) == 'try-error'){
                    vote.list.1$category <- content1$data$list$typeDesc
                    vote.list.1$effect <- content1$data$list$effect
                    vote.list.1$attitude <- content1$data$list$attitude
                    vote.list.1$reply.no <- content1$data$list$postCnt
                    vote.list.1$vote.time <- content1$data$list$time %>%
                        gsub(' .+', '', .)
                    vote.list.1$vote.time <- gsub("今天", Sys.Date(), vote.list.1$vote.time) 
                    vote.list.1$vote.time <- gsub("昨天", Sys.Date() - 1, vote.list.1$vote.time)
                    vote.list.1$vote.time[nchar(vote.list.1$vote.time) < 10 & (!is.na(vote.list.1$vote.time))] <- paste(lubridate::year(Sys.Date()), vote.list.1$vote.time[nchar(vote.list.1$vote.time) < 10 & (!is.na(vote.list.1$vote.time))], sep = '-')
                    vote.list.1$vote.url <- content1$data$list$jsdetailurl
                    dc.vote.list <- data.frame(vote.list.1, stringsAsFactors = F)
                    write.table(dc.vote.list, 'doc.vote.list.0418.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
                }
                
                #writeLines(content_text, outfile1)
            }
            #close(outfile1) 
            #result.data[[i]] <- dc.article.list
        }
        
    }
    
}