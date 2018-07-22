library(curl)
library(jsonlite)
library(tidyverse)
library(rvest)
library(stringr)

#获取医生的id和医生总数
input_file = 'haodf/Doclist.mobile.0423.af.unique.csv' 
doclist.all <- read.csv(input_file, header = TRUE,
                        stringsAsFactors = FALSE)
doclist.all <- unique(doclist.all)
doctor.count <- nrow(doclist.all)

#随机取100个医生进行测试
#set.seed(1)
#x <- sample(c(1:12473), size = 10)
#x <- 9691
#result.data <- list()

time.record <- list() 
# i表示第i位医生 
#for(i in 1:3){
for(i in 1:doctor.count){
    time.record[[i]] <- Sys.time()
    print(sprintf("Crawlering No.%s doctor at %s.", i, time.record[[i]]))  
    article.list.1 <- list()
    #获取医生的id，在后面用在文章列表链接中
    doctor.id <- str_extract(string = doclist.all$url[i], pattern = regex("(?<=//m.haodf.com/touch/doctor/).+(?=.htm)"))
    #在表中加上医生的姓名和医生的id，以便之后的表间关联
    article.list.1$doctor.name <- doclist.all$name[i] 
    article.list.1$doctor.id <- doctor.id 
    #找到文章列表的第一页并获取总页数
    article.start.url <- sprintf('https://m.haodf.com/touch/doctor/loadmorearticle?id=%s&p=1', doctor.id)
    # article.start.url <- 'https://m.haodf.com/touch/doctor/loadmorearticle?id=DE4rO-XCoLUEJ2qrbc1P6dS2aO&p=1'
    req.1 <- curl_fetch_memory(article.start.url)
    df1 = try(rawToChar(req.1$content) %>% fromJSON())
    #以防网络原因，设置重试次数
    try_time = 0
    while(class(df1) == 'try-error'){
        req.1 <- curl_fetch_memory(article.start.url)
        df1 = try(rawToChar(req.1$content) %>% fromJSON())
        try_time = try_time + 1
        print(sprintf("This is the %s time for retrying...", try_time))
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time)
        if(try_time > 10){
            break()
        }
    }
    if (class(df1) == 'try-error'){
        dc.article.list <- data.frame(article.list.1, stringsAsFactors = F) 
        #result.data[[i]] <- dc.article.list
        write.table(dc.article.list, 'doc.article.list.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        next() 
    }
    pages <- df1$pageInfo$pages
    article.list.url.all <- ''
    #outfile1 = file(sprintf("doc.mobile_%s.html", i), encoding = 'UTF8', open = 'a+')
    #j表示该医生的咨询总页数
    
    #outfile1 = file("article.list.0403.html", encoding = 'UTF8', open = 'a+')
    
    if(length(pages) == 0){
        dc.article.list <- data.frame(article.list.1, stringsAsFactors = F) 
        #result.data[[i]] <- dc.article.list
        write.table(dc.article.list, 'doc.article.list.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        next()
    }else if(pages == 0){
        dc.article.list <- data.frame(article.list.1, stringsAsFactors = F) 
        #result.data[[i]] <- dc.article.list
        write.table(dc.article.list, 'doc.article.list.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        next()
    }else{
        for(j in 1 : pages){
            print(sprintf("Crawlering No.%s doctor's page %s.", i, j)) 
            article.url <- sprintf('https://m.haodf.com/touch/doctor/loadmorearticle?id=%s&p=%s', doctor.id, j)
            #    article.url <- sprintf('https://m.haodf.com/touch/doctor/loadmorearticle?id=DE4r08xQdKSLBZmGIpMEBi1InoYl&p=%s', j)
            req <- curl_fetch_memory(article.url)
            content1 <- try(rawToChar(req$content) %>% fromJSON())
            try_time1 = 0
            while(class(content1) == 'try-error'){
                req <- curl_fetch_memory(article.url)
                content1 <- try(rawToChar(req$content) %>% fromJSON())
                try_time1 = try_time1 + 1
                print(sprintf("This is the %s time for retrying...", try_time1))
                Sys.sleep(sample(c(1:10), size = 1)/10*try_time1)
                if(try_time1 > 10){
                    break()
                }
            }
            if(!class(content1) == 'try-error'){
                content_text <- content1$content
                article.list.url.all <- paste(article.list.url.all, content_text, collapse = ' ')
            }
            
            #writeLines(content_text, outfile1)
        }
        #close(outfile1) 
        
        if(length(article.list.url.all) == 0){
            next()
        }else if(!str_detect(article.list.url.all, "<div")){
            next()
        }  
        
        article.url <- article.list.url.all
        articlelist.html <- read_html(article.url, encoding = 'UTF8') 
        # 猜想，如果先定位到 /html/body/div 
        articlelist <- articlelist.html %>% 
            html_nodes(xpath = '/html/body/li') 
        
        xpath.link  = './a'
        xpath.title = './a/p[@class="l-title"]/text()'
        xpath.label = './a/div[@class="l-label"]/span[@class="l-l-pay"]/text()'
        xpath.time = './a/div[@class="l-info"]/div[@class="l-i-publish fl"]/span/text()'
        xpath.read.account = './a/div[@class="l-info"]/div[@class="l-i-ear fr"]/span[@class="l-i-e-number"]/span/text()'
        
        article.list.1$article.link <- articlelist %>%
            html_node(xpath = xpath.link) %>%
            html_attr('href') %>%
            trimws()
        article.list.1$article.title <- articlelist %>%
            html_node(xpath = xpath.title) %>%
            trimws() # %>%
        # str_extract('[\u4e00-\u9fff]+')
        article.list.1$article.label <- articlelist %>%
            html_node(xpath = xpath.label) %>%
            trimws()
        article.list.1$article.time <- articlelist %>%
            html_node(xpath = xpath.time) %>%
            trimws()
        
        article.list.1$article.time <- gsub("今天", format(Sys.Date(), format = '%Y-%m-%d'), article.list.1$article.time) 
        article.list.1$article.time <- gsub("昨天", format(Sys.Date() - 1, format = '%Y-%m-%d'), article.list.1$article.time) 
        article.list.1$article.time[nchar(article.list.1$article.time) < 10 & (!is.na(article.list.1$article.time))] <- paste(lubridate::year(Sys.Date()), article.list.1$article.time[nchar(article.list.1$article.time) < 10 & (!is.na(article.list.1$article.time))], sep = '-')
        
        article.list.1$article.read.account <- articlelist %>%
            html_node(xpath = xpath.read.account) %>%
            trimws() 
        #if(str_detect(article.list.1$article.read.account, "万")){
        #  article.list.1$article.read.account <- str_extract_all(article.list.1$article.read.account, "[0-9]+")*10000
        #}  
        article.list.1$article.read.account[which(str_detect(article.list.1$article.read.account, "万"))] <- article.list.1$article.read.account[which(str_detect(article.list.1$article.read.account, "万"))] %>%
            gsub("万", "", .) %>%
            as.numeric(.)*10000
        
        article.list.1$date <- Sys.Date()
        # consult.2 = lapply(consult.info.1, function(x) {if(length(x)==0) return(NA) else x})
        
        dc.article.list <- data.frame(article.list.1, stringsAsFactors = F) %>%
            dplyr::rename(
                label = article.label,
                read_account = article.read.account
            )
        write.table(dc.article.list, 'doc.article.list.utf8.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')   
        
    }
}
