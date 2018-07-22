#先抓取所有医生的职称和所在的医院id，此代码在医院的id的基础上，抓取医院的相关信息
library(curl)
library(jsonlite)
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)

#获取医生的id和医生总数
input_file = 'haodf/doc.info.0509.csv' 
doclist.all <- read.csv(input_file, header = TRUE,
                        stringsAsFactors = FALSE)
#doclist.all <- unique(doclist.all)
hosp.all <- unique(doclist.all$hosp.id)
n <- length(hosp.all)

time.record <- list()

#for(i in 1:1){
for(i in 1:n){
    
    time.record[[i]] <- Sys.time()
    print(sprintf("Crawlering No.%s hospital at %s.", i, time.record[[i]]))  
    hosp.info.list.1 <- list()
    
    #加上医生的姓名和id
    hosp.info.list.1$hosp.id <- hosp.all[i] 
    
    
    #爬取医院主页的相关信息
    
    print(sprintf("This doctor's hospital id is %s", hosp.all[i]))
    #hosp_url <- '/hospital/DE4r0Fy0C9LuG0J2p0QVfMMV2YMZWydg7.htm'
    hosp.info.html <- try(read_html(paste('http://www.haodf.com/hospital/',hosp.all[i],'.htm' ,sep=""), encoding = 'gbk'))
    
    try_time2 = 0
    while (class(hosp.info.html) =='try-error') {
        hosp.info.html <- try(read_html(paste('http://www.haodf.com/hospital/',hosp.all[i],'.htm' ,sep=""), encoding = 'gbk'))
        try_time2 = try_time2 +1
        print(sprintf("This is %s time for retrying...", try_time2))
        Sys.sleep(sample(c(1:10), size = 1)/10*try_time2)
        if(try_time2 > 10){
            break()
        }
    }
    
    #xpath.hosp.name = '/html/body/doctype/div[@class="hos-info"]/div[@class="hos-info-base mt9 bgc posr"]/div[@class="hos-name posr ft19"]/p/text()'
    #xpath.hosp.level = '/html/body/doctype/div[@class="hos-info"]/div[@class="hos-info-base mt9 bgc posr"]/div[@class="hos-lable"]/span/text()'
    
    
    #xpath.all = '/html/body/doctype/div[@class="hos-info"]'
    #xpath.ths.letter.total = '//*[@id="gray"]/div[@class="container clearfix"]/div[@class="container-right"]/div[@class="m-hospital"]/div[@class="m-h-title m-h-title-note"]/span[@class="m-h-t-num"]'
    #xpath.ths.letter.total = '//*[@id="gray"]/div[@class="container clearfix"]/div[@class="container-right"]/div[@calss="m-hospital"]/div[1]'
    #xpath.ths.letter.total = '//*[@id="gray"]/div[7]'
    #xpath.hosp.size = '//*[@id="gray"]/div[@class="container clearfix"]/div[@class="container-left"]/div[@class="m-hospital"]/div[@class="m-h-title"]/span/text()'
    
    #hosp.info.list.1$hosp.name <- hosp.info.html %>%
    #    html_node(xpath = xpath.hosp.name) %>%
    #    trimws()
    #hosp.info.list.1$hosp.level <- hosp.info.html %>%
    #    html_nodes(xpath = xpath.hosp.level) %>%
    #    trimws() %>%
    #    paste(., collapse = ' ')
    
    #hosp.info.all <- hosp.info.html %>%
    #    html_nodes(xpath = xpath.all) %>%
    #    html_text() %>%
    #    trimws() %>%
    #    gsub('\\s+', ' ', .)
    
    #hosp.info.list.1$hosp.doctor.num <- str_extract(hosp.info.all, pattern = '[0-9].+(?=位医生)') %>%
    #    gsub(' ', '',.)
    #hosp.info.list.1$hosp.office.num <- str_extract(hosp.info.all, pattern = '(?<=医生).+(?=个科室)') %>%
    #    gsub(' ', '',.)
    #hosp.info.list.1$hosp.online.pa <- str_extract(hosp.info.all, pattern = '(?<=在线服务患者).+(?=名)') %>%
    #    gsub(' ', '',.)
    #hosp.info.list.1$thsletter.total <- str_extract(hosp.info.all, pattern = '(?<=获得).+(?=封感谢信)') %>%
    #    gsub(' ', '',.)
    
    xpath.hosp.name = '//*[@id="gray"]/div[@class="hospital-info"]/div[@class="h-i-container container"]/div[@class="h-i-c-top clearfix"]/div[@class="hospital-detail"]/div[@class="h-d-title"]/h1[@class="hospital-name"]/text()'
    xpath.hosp.level = '//*[@id="gray"]/div[@class="hospital-info"]/div[@class="h-i-container container"]/div[@class="h-i-c-top clearfix"]/div[@class="hospital-detail"]/div[@class="h-d-title"]/span[@class="hospital-label-item"]/text()'
    xpath.all = '/html/body/div[@class="hospital-info"]/div[@class="h-i-container container"]/div[@class="h-i-c-top clearfix"]/div[@class="hospital-influence"]'
    xpath.ths.letter.total = '//*[@id="gray"]/div[@class="container clearfix"]/div[@class="container-right"]/div[@class="m-hospital"]/div[@class="m-h-title m-h-title-note"]/span[@class="m-h-t-num"]'
    #xpath.ths.letter.total = '//*[@id="gray"]/div[@class="container clearfix"]/div[@class="container-right"]/div[@calss="m-hospital"]/div[1]'
    #xpath.ths.letter.total = '//*[@id="gray"]/div[7]'
    xpath.hosp.size = '//*[@id="gray"]/div[@class="container clearfix"]/div[@class="container-left"]/div[@class="m-hospital"]/div[@class="m-h-title"]/span/text()'
    
    hosp.info.list.1$hosp.name <- hosp.info.html %>%
        html_node(xpath = xpath.hosp.name) %>%
        trimws()
    hosp.info.list.1$hosp.level <- hosp.info.html %>%
        html_nodes(xpath = xpath.hosp.level) %>%
        trimws() %>%
        paste(., collapse = ' ')
    
    hosp.influence.all <- hosp.info.html %>%
        html_nodes(xpath = xpath.all) %>%
        html_text() %>%
        trimws() %>%
        gsub('\\s+', ' ', .)
    hosp.info.list.1$hosp.rank <- str_extract(hosp.influence.all, pattern = '(?<=关注度排名).+(?=累计访问量)')
    hosp.info.list.1$hosp.visit <- str_extract(hosp.influence.all, pattern = '(?<=累计访问量).+(?=次)') %>%
        gsub(' ', '',.)
    if(!is.na(hosp.info.list.1$hosp.visit)){
        if(str_detect(hosp.info.list.1$hosp.visit,"万")){
            hosp.info.list.1$hosp.visit <- hosp.info.list.1$hosp.visit %>%
                gsub('万', '', .) %>%
                as.numeric(.)*10000
        }
        
    }
    
    
    hosp.info.list.1$hosp.online.pa <- str_extract(hosp.influence.all, pattern = '(?<=在线服务患者).+(?=位)') %>%
        gsub(' ', '',.)
    hosp.info.list.1$hosp.good <- str_extract(hosp.influence.all, pattern = '(?<=好评).+(?=差评)') %>%
        gsub(' ', '',.)
    hosp.info.list.1$hosp.bad <- str_extract(hosp.influence.all, pattern = '(?<=差评).+(?=患者满意度)') %>%
        gsub(' ', '',.)
    hosp.info.list.1$pa.satisfaction <- str_extract(hosp.influence.all, pattern = '(?<=患者满意度).+(?=%)') %>%
        gsub(' ', '',.) %>%
        as.numeric(.)/100
    hosp.info.list.1$thsletter.total <- hosp.info.html %>%
        html_nodes(xpath = xpath.ths.letter.total) %>%
        html_text() %>%
        trimws() 
    hosp.size <- hosp.info.html %>%
        html_node(xpath = xpath.hosp.size) %>%
        #html_text() %>%
        trimws() 
    hosp.info.list.1$hosp.office.num <- str_extract(hosp.size, pattern = '(?<=科室).+(?=个)') %>%
        gsub(' ', '',.)
    hosp.info.list.1$hosp.doctor.num <- str_extract(hosp.size, pattern = '(?<=大夫).+(?=人)') %>%
        gsub(' ', '',.)
    hosp.info.list.1$date <- Sys.Date()
    hosp.info.list = lapply(hosp.info.list.1, function(x) {if(length(x)==0) return(NA) else x})
    
    hosp.info <- data.frame(hosp.info.list, stringsAsFactors = F) 
    write.table(hosp.info, 'hosp.list.info.csv', row.names = F, col.names = F, append = T, sep = ',', na = '')    
    Sys.sleep(5)
    
    
    
}
