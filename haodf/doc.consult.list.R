library(curl)
library(jsonlite)
library(tidyverse)
library(rvest)
library(stringr)

#��ȡҽ����id��ҽ������
input_file = 'haodf/Doclist.mobile.0423.af.unique.csv' 
doclist.all <- read.csv(input_file, header = TRUE,
                        stringsAsFactors = FALSE)
doclist.all <- unique(doclist.all)
doctor.count <- nrow(doclist.all)

result.data <- list()
#i��ʾҽ��������

time.record <- list() 
# 88 error 
for(i in 1:doctor.count){
  time.record[[i]] <- Sys.time()
  print(sprintf("Crawlering No.%s doctor at %s.", i, time.record[[i]]))  
  consult.info.1 <- list()
  doctor.id <- str_extract(string = doclist.all$url[i], pattern = regex("(?<=//m.haodf.com/touch/doctor/).+(?=.htm)"))
  consult.info.1$doctor.name <- doclist.all$name[i] 
  consult.info.1$doctor.id <- doctor.id 
  #�ҵ���ѯ�ĵ�һҳ����ȡ��ҳ��
  consult.start.url <- sprintf('https://m.haodf.com/touch/case/loadmorecase?doctorid=%s&p=1', doctor.id)
  req.1 <- curl_fetch_memory(consult.start.url)
  df1 = try(rawToChar(req.1$content) %>% fromJSON())
  try_time = 0
  while(class(df1) == 'try-error'){
    req.1 <- curl_fetch_memory(consult.start.url)
    df1 = try(rawToChar(req.1$content) %>% fromJSON())
    print(sprintf("This is the %s time for retrying...", try_time))
    try_time = try_time + 1
    if(try_time > 5){
      break()
    }
  }
  if (class(df1) == 'try-error'){
    dc.consult.info <- data.frame(consult.info.1, stringsAsFactors = F) 
    result.data[[i]] <- dc.consult.info
    next() 
  }
  pages <- df1$pageInfo$pages
  consult.list.url.all <- ''
  #outfile1 = file(sprintf("doc.mobile_%s.html", i), encoding = 'UTF8', open = 'a+')
  #j��ʾ��ҽ������ѯ��ҳ��
  
  if(pages == 0){
    
    dc.consult.info <- data.frame(consult.info.1, stringsAsFactors = F) 
    result.data[[i]] <- dc.consult.info
    next()
    
  }
  else{
    for(j in 1 : pages){
      print(sprintf("Crawlering No.%s doctor's page %s.", i, j)) 
      consult.url <- sprintf('https://m.haodf.com/touch/case/loadmorecase?doctorid=%s&p=%s', doctor.id, j)
      req <- curl_fetch_memory(consult.url)
      content1 <- try(rawToChar(req$content) %>% fromJSON())
      try_time1 = 0
      while(class(content1) == 'try-error'){
        req <- curl_fetch_memory(consult.url)
        content1 <- try(rawToChar(req$content) %>% fromJSON())
        print(sprintf("This is the %s time for retrying...", try_time1))
        try_time1 = try_time1 + 1
        if(try_time1 > 5){
          break()
        }
      }
      content_text <- content1$content
      consult.list.url.all <- paste(consult.list.url.all, content_text, collapse = ' ')
      #writeLines(content_text, outfile1)
    }
    #close(outfile1) 
    
    zixun.url <- consult.list.url.all
    zixunlist.html <- read_html(zixun.url, encoding = 'UTF8') 
    # ���룬����ȶ�λ�� /html/body/div 
    zixunlist <- zixunlist.html %>% 
      html_nodes(xpath = '/html/body/div') 
    
    # zixunlist <- zixunlist.html %>% html_nodes(xpath = '/html/body/div')
    
    # ���ʱ��xpath Ҫд�����·���ˣ������� './a' ��ͷ�ˣ�
    # ��Ϊ�Ѿ����� /html/body/div�����
    
    xpath.link  = './a'
    xpath.title = './a/div[@class="not_ask"]/p/text()|./a/div[@class="ask"]/p/span/text()'
    #xpath.secret.title = './a/div[@class="ask"]/p/span/text()'
    xpath.cat = './a/div[1]/p/span[@class="moneyAsk"]'
    xpath.ill = './/*[@id="txt"]'
    xpath.area = './a/div[@class="bot"]/span[2]'
    xpath.time = './a/div[@class="bot"]/span[3]'
    xpath.talk = './a/div[@class="bot"]/span[4]'
    
    consult.info.1$consult.link <- zixunlist %>%
      html_node(xpath = xpath.link) %>%
      html_attr('href') %>%
      trimws()
    consult.info.1$consult.title <- zixunlist %>%
      html_node(xpath = xpath.title) %>%
      html_text() %>%
      trimws() # %>%
    # str_extract('[\u4e00-\u9fff]+')
    consult.info.1$consult.category <- zixunlist %>%
      html_node(xpath = xpath.cat) %>%
      html_text() %>%
      trimws()
    consult.info.1$consult.ill <- zixunlist %>%
      html_node(xpath = xpath.ill) %>%
      html_text() %>%
      trimws() %>%
      gsub(pattern = "������", replacement = '', x = .)
    consult.info.1$consult.pa.area <- zixunlist %>%
      html_node(xpath = xpath.area) %>%
      html_text() %>%
      trimws()
    consult.info.1$consult.time <- zixunlist %>%
      html_node(xpath = xpath.time) %>%
      html_text() %>%
      trimws()
    
    consult.info.1$consult.time <- gsub("����", format(Sys.Date(), format = '%Y.%m.%d'), consult.info.1$consult.time) %>% gsub('/','.', .)
    consult.info.1$consult.time <- gsub("����", format(Sys.Date() - 1, format = '%Y.%m.%d'), consult.info.1$consult.time) %>% gsub('/','.', .)
    consult.info.1$consult.time[nchar(consult.info.1$consult.time) < 10 & (!is.na(consult.info.1$consult.time))] <- paste(lubridate::year(Sys.Date()), consult.info.1$consult.time[nchar(consult.info.1$consult.time) < 10 & (!is.na(consult.info.1$consult.time))], sep = '.')
    
    consult.info.1$consult.talk <- zixunlist %>%
      html_node(xpath = xpath.talk) %>%
      html_text() %>%
      trimws()%>%
      str_extract(pattern = '\\d+') %>%
      as.integer()
    consult.info.1$date <- Sys.Date()
    # consult.2 = lapply(consult.info.1, function(x) {if(length(x)==0) return(NA) else x})
    
    dc.consult.info <- data.frame(consult.info.1, stringsAsFactors = F) %>%
      dplyr::rename(
        link = consult.link,
        title = consult.title,
        category = consult.category,
        illness = consult.ill,
        area = consult.pa.area,
        time = consult.time,
        talk.count = consult.talk
      )
    
    result.data[[i]] <- dc.consult.info
    
  }
  
  
}
result.df <- plyr::rbind.fill(result.data)
write.table(result.df, 'doc.consult.list.0323.csv', row.names = F, col.names = T, append = F, sep = ',', na = '') 