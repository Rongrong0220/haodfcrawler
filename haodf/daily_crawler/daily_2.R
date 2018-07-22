
args <- commandArgs(trailingOnly = TRUE)
order_id = as.integer(args[1])
total_times = as.integer(args[2])

if(length(args) < 2) {
    stop("输入参数数量应该两个，第x1部分, 共x2部分")
}

print(sprintf("Running %s of %s...", order_id, total_times))

library(RSelenium)
library(rvest)
library(tidyverse)
library(stringr)
library(methods)
library(foreach)
library(data.table) 

# if mac or linux
library(doMC)
registerDoMC(cores=4)

# if windows
# library(doParallel)
# #the following line will create a local 4-node snow cluster
# workers=makeCluster(4,type="SOCK")
# registerDoParallel(workers)  
root_dir <- "haodf/" 

# 使用每天创建的文件夹方便把每天的数据放在一起
# 后面方便发送邮件 
date_today <- Sys.Date() 
work_dir_today <- sprintf("%sdaily_data_%s", root_dir, gsub('-', '', date_today))  
setwd(work_dir_today) 

input_file_pattern = sprintf('daily_1.+.csv', 
                             Sys.Date()) 
input_files = list.files(pattern = input_file_pattern) 

# dclist.all <- read.csv(input_file, header = TRUE,
#                        stringsAsFactors = FALSE)

file_count = length(input_files) 
dclist.all.list <- list() 
for (i in 1: file_count){
    dclist.all.list[[i]] <- read.csv(input_files[i], header = T, stringsAsFactors = F) 
    dclist.all.list[[i]]$batch_id <- i 
}
dclist.all.1 <- plyr::rbind.fill(dclist.all.list) 
# dclist.all.1 <- purrr::map_df(input_files, read.csv, header=T, stringsAsFactors=F)
doctor.count.raw <- nrow(dclist.all.1) 
dclist.all.1 <- unique(data.table(dclist.all.1), by = 'homepage', fromLast = T)  

dclist.all <- dclist.all.1 %>% filter(!is.na(homepage), nchar(homepage) != 0) 


doctor.count <- nrow(dclist.all) 

print(sprintf("共%s医生, 其中有homepage的有%s个.", 
              doctor.count.raw, doctor.count)) 

if (!is.na(order_id)){
    group.count = total_times
    
}else{
    order_id = 1 
    group.count = 1000 
}
eachgroup.num = ceiling(doctor.count/group.count)
if(group.count != file_count){
    dclist.all$batch_id <- rep(1:group.count, each = eachgroup.num, length = doctor.count) 
}
dclist.part <- dclist.all %>% filter(batch_id == order_id)



# Run a server for example using Docker
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1
# Use a debug image with a VNC viewer if you wish to view the browser
# docker run -d -p 5901:5900 -p 127.0.0.1:4445:4444 --link http-server selenium/standalone-firefox-debug:2.53.1
# See Docker vignette for more detail or run a Selenium Server manually.

# 如果在个人电脑上可以用chrome 肉眼可见速度 
prefs = list("profile.managed_default_content_settings.images" = 2L)
cprof <- list(chromeOptions = list(prefs = prefs))
remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444L
                      , browserName = "chrome"
                      , extraCapabilities = cprof
)

Sys.sleep(3) # give the binary a moment 


doctorCrawler_profile <- function(dc.hp.url, retry_times = 5) {
    remDr$navigate(dc.hp.url)
    remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);',
                        args = list("dummy"))
    dc.hp.source <- remDr$getPageSource()
    dc.hp.source.html <- read_html(dc.hp.source[[1]])
    xpath.name.job = '/html/body/div[2]/div[1]/div[2]/div[2]/div[1]/h3'
    xpath.hospital = '/html/body/div[2]/div[1]/div[2]/div[2]/div[2]/div/p/a[1]'
    
    
    dc.name <- dc.hp.source.html %>%
        html_node(xpath = xpath.name.job) %>%
        html_text() %>%
        trimws() %>%
        str_extract('[\u4e00-\u9fff]+')
    
    
    try_time = 0
    while(is.na(dc.name)){
        remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);',
                            args = list("dummy"))
        dc.hp.source <- remDr$getPageSource()
        dc.hp.source.html <- read_html(dc.hp.source[[1]])
        dc.name <- dc.hp.source.html %>%
            html_node(xpath = xpath.name.job) %>%
            html_text() %>%
            trimws() %>%
            str_extract('[\u4e00-\u9fff]+')
        
        try_time = try_time + 1
        if (try_time > retry_times){
            break()
        }
    }
    
    hospital <- dc.hp.source.html %>%
        html_nodes(xpath = xpath.hospital) %>%
        html_text() %>%
        trimws()
    personal.stats <- dc.hp.source.html %>%
        html_nodes(css = 'ul.space_statistics') %>%
        html_text() %>%
        trimws()
    atrb <- dc.hp.source.html %>%
        html_nodes(css = 'ul.doc_info_ul1') %>%
        html_node(css = 'a.blue') %>%
        html_text() %>%
        trimws()
    dc.group <- dc.hp.source.html %>%
        html_nodes(css = 'p.fr.fs.gray3.pr10.pt10') %>%
        html_node(css = 'span.orange1') %>%
        html_text() %>%
        trimws()
    ol.consult <- dc.hp.source.html %>%
        html_nodes(css = 'a.zixun') %>%
        html_node(css = 'span.d-s-icon') %>%
        html_text() %>%
        trimws()
    familydoc <- dc.hp.source.html %>%
        html_node(css = 'a.familydoc') %>%
        html_text() %>%
        trimws()
    oder.doc <- dc.hp.source.html %>%
        html_node(css = 'a.guahao') %>%
        html_text() %>%
        trimws()
    team.doc <- dc.hp.source.html %>%
        html_node(css = 'a.teamdoc') %>%
        html_text() %>%
        trimws()
    
    dc.stats.1 <- unlist(str_split(trimws(personal.stats), pattern = '\\n'))
    dc.hp.stats.1 <- str_split_fixed(dc.stats.1, "：", 2)
    dc.hp.stats.2 <- as.data.frame(dc.hp.stats.1, stringsAsFactors = F) %>%
        set_names('item', 'values') %>%
        mutate(item = gsub(' ', '', item))
    #dc.hp.stats.2 <- data.frame(
    #item = dc.hp.stats.1[, 1],
    #values = dc.hp.stats.1[, 2],
    #stringsAsFactors = F)  # %>% setNames(c('item', 'value'))
    dc.hp.stats <- dcast(data.table(dc.hp.stats.2), 1~item, value.var = 'values') %>%
        dplyr::select(-.)
    
    dc.hp.stats$doc.name <- dc.name
    dc.hp.stats$doc.hosp <- hospital
    dc.hp.stats$attribution <- atrb
    dc.hp.stats$online.consult <- ol.consult
    dc.hp.stats$personal.dc <- familydoc
    dc.hp.stats$oder <- oder.doc
    dc.hp.stats$doc.group <- dc.group
    dc.hp.stats$doc.team <- team.doc
    dc.hp.stats$time <- Sys.time()
    
    
    # 用列表方便使用 lapply 来统一检查问题，长度为0补NA
    doc.hp = lapply(dc.hp.stats, function(x) {if(length(x)==0) return(NA) else x})
    #names(dc.hp) <- c("visit.total", "visit.yesterday", "article", "total.patient", "exam.yesterday", "exam.wechat", "exam.total",
    #"patient.vote", "open.time","name", "attribution", "online.consult", "personal.dc", "oder", "group", "time" )
    dc.hp <- data.frame(doc.hp, stringsAsFactors = F) %>%
        dplyr::rename(
            visit.total = `总访问`,
            visit.yesterday = `昨日访问`,
            article = `总文章`,
            total.patient = `总患者`,
            exam.yesterday = `昨日诊后报到患者`,
            exam.wechat = `微信诊后报到患者`,
            exam.total = `总诊后报到患者`,
            patient.vote = `患者投票`,
            tl.num = `感谢信`,
            gift.num = `心意礼物`,
            ol.time = `上次在线`,
            open.time = `开通时间`
        )
    dc.hp.1 <- dc.hp %>%
        mutate(
            doc.homepage = dc.hp.url,
            visit.total = as.integer(str_extract(gsub(',', '', visit.total), '\\d+')),
            visit.yesterday = as.integer(str_extract(gsub(',', '', visit.yesterday), '\\d+')),
            article = as.integer(str_extract(gsub(',', '', article), '\\d+')),
            total.patient = as.integer(str_extract(gsub(',', '', total.patient), '\\d+')),
            exam.yesterday = as.integer(str_extract(gsub(',', '', exam.yesterday), '\\d+')),
            exam.wechat = as.integer(str_extract(gsub(',', '', exam.wechat), '\\d+')),
            exam.total = as.integer(str_extract(gsub(',', '', exam.total), '\\d+')),
            patient.vote = as.integer(str_extract(gsub(',', '', patient.vote), '\\d+')),
            tl.num = as.integer(str_extract(gsub(',', '', tl.num), '\\d+')),
            gift.num = as.integer(str_extract(gsub(',', '', gift.num), '\\d+'))
        )
    
    return(dc.hp.1)
}


doctorCrawlerLoop <- function(doc_list, start_time, craw_fun, order = order_id,
                              total = total_times, retry_times = 7) {
    doctorCrawler <- craw_fun
    remDr$open(silent = T)
    m <- nrow(doc_list)
    result.list <- list()
    for (i in 1:m){
        false_time = 0
        try_time = 0
        result.list[[i]] <- try(doctorCrawler(doc_list$homepage[i]))
        while(class(result.list[[i]]) == 'try-error') {
            result.list[[i]] <- try(doctorCrawler(doc_list$homepage[i]))
            try_time = try_time + 1
            if(try_time > 5) {
                # 之前以为只有 time out 一种问题，
                # 经过测试存在 浏览器未响应的问题，
                # 所以这里如果抓取5次没有结果，就重新打开个浏览器
                try(remDr$close())
                remDr$open(silent = T)
            }
            if(try_time > retry_times){
                break()
            }
        }
        if(!class(result.list[[i]]) == 'try-error') {
            print(sprintf("Crawling %s of %s at %s, session %s", i, m, Sys.time(), order_id))
            write.table(result.list[[i]],
                        file = sprintf("daily.data.2.%s.of.%s.begins.%s.csv",
                                       order, total, gsub(' |:|-', '.', start_time)),
                        row.names = F, col.names = F,
                        append = T, sep = ',')
        }else{
            false_time = false_time + 1
            print(sprintf("Failed to Crawl %s of %s at %s, session %s", i, m, Sys.time(), order_id))
        }
        if (false_time > 15){
            break()
        }
        if (i %% 300 == 0){
            try(remDr$close())
            remDr$open(silent = T)
            gc() 
        }
    }
    result.list.1 <- lapply(result.list,
                            function(x) {
                                if(is.data.frame(x)){
                                    return(x)
                                }else{
                                    return(NULL)
                                }}
    )
    result.df <- plyr::rbind.fill(result.list.1)
    remDr$close()
    return(result.df)
}

doctorCrawlerMC <- function(doc_list, group_num = 4) {
    doc_list <- unique(doc_list)
    doc_count = nrow(doc_list)
    print(sprintf("There are %s unique doctors", doc_count))
    doc_list$batch_id = rep(1:group_num, length = doc_count)
    
    result.df <- foreach(d = 1:group_num,
                         .verbose = T,
                         .combine = rbind
                         
    ) %dopar%
        (filter(doc_list, batch_id == d) %>% doctorCrawlerLoop())
    return(result.df)
}

start_time <- Sys.time()
result.data <- doctorCrawlerLoop(dclist.part, start_time = start_time,
                                 craw_fun = doctorCrawler_profile)
end_time <- Sys.time()
write.csv(result.data,
          file = sprintf('daily_2.part_%s_of_%s.%s.csv',
                         order_id, total_times, gsub(' |:|-', '.', Sys.time())),
          na = '', row.names = F)
