
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

# if mac or linux
library(doMC)
registerDoMC(cores=4)

# if windows
# library(doParallel)
# #the following line will create a local 4-node snow cluster
# workers=makeCluster(4,type="SOCK")
# registerDoParallel(workers) 

# 整个项目的根文件夹(代码所在文件夹) 
root_dir <- "haodf/" 

# 每天创建一个文件夹方便把每天的数据放在一起
# 后面方便发送邮件 
date_today <- Sys.Date() 
work_dir_today <- sprintf("%sdaily_data_%s", root_dir, gsub('-', '', date_today)) 

if(!dir.exists(work_dir_today)){
    dir.create(work_dir_today, recursive = T) 
}
setwd(work_dir_today) 

input_file = sprintf("%s%s", root_dir, 'Dclist_1228_all_1.utf8.csv')  

dclist.all <- read.csv(input_file, header = TRUE,
                       stringsAsFactors = FALSE)

dclist.all <- unique(dclist.all)
doctor.count <- nrow(dclist.all)

if (!is.na(order_id)){
    group.count = total_times
    
}else{
    order_id = 1 
    group.count = 1000 
}
eachgroup.num = ceiling(doctor.count/group.count)
dclist.all$batch_id <- rep(1:group.count, each = eachgroup.num, length = doctor.count)
dclist.part <- dclist.all %>% filter(batch_id == order_id)


# Run a server for example using Docker
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1
# Use a debug image with a VNC viewer if you wish to view the browser
# docker run -d -p 5901:5900 -p 127.0.0.1:4445:4444 --link http-server selenium/standalone-firefox-debug:2.53.1
# See Docker vignette for more detail or run a Selenium Server manually.

prefs = list("profile.managed_default_content_settings.images" = 2L)
cprof <- list(chromeOptions = list(prefs = prefs))
remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444L
                      , browserName = "chrome"
                      , extraCapabilities = cprof
)

# 提前在其他脚本里打开它
# pJS <- wdman::phantomjs(port = as.integer(4567))
# eCap <- list(phantomjs.page.settings.userAgent
#              = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36",
#              phantomjs.page.settings.loadImages = FALSE)
# # remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = eCap)
# 
# remDr <- remoteDriver(remoteServerAddr = "localhost"
#                       , port = 4567L
#                       # , browserName = "chrome"
#                       , browserName = "phantomjs"
#                       , extraCapabilities = eCap
# )
Sys.sleep(3) # give the binary a moment 


doctorCrawler_info <- function(dc.inf.url, retry_times = 5) {
    doctor.1 = list()
    remDr$navigate(dc.inf.url)
    remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);',
                        args = list("dummy"))
    dc.inf.source <- remDr$getPageSource()
    dc.inf.source.html <- read_html(dc.inf.source[[1]])
    xpath.name = '//*[@id="bp_doctor_about"]/div/div[1]/div[1]/div[1]/h1/a'
    xpath.hosp = '//*[@id="bp_doctor_about"]/div/div[2]/div/table[1]/tbody/tr/td[3]/a'
    xpath.personal_url = '//*[@id="bp_doctor_about"]/div/div[2]/div/div[1]/span[3]/a'
    #xpath.job = '//*[@id="truncate_DoctorSpecialize"]'
    xpath.health.num = '//*[@id="toptr_type_all"]/div[1]/div[3]/span'
    #xpath.pa.url = '//*[@id="toptr_type_all"]/div[1]/div[3]/a'
    xpath.thanksLetters = '//*[@id="bp_doctor_about"]/div/div[2]/div/table[1]/tbody/tr/td[4]/a[1]'
    xpath.gift = '//*[@id="bp_doctor_about"]/div/div[2]/div/table[1]/tbody/tr/td[4]/a[2]'
    #xpath.personal_url = '//*[@id="bp_doctor_about"]/div/div[2]/div/div[1]/span[3]/a'
    # xpath.hot = '//*[@id="bp_doctor_about"]/div/div[2]/div/div[2]/div/div[1]/p[1]/text()'
    # 2018-01-17 修正 有些医生的热度位置不太一样
    xpath.hot = '//*[@id="bp_doctor_about"]/div/div[2]/div/div/div/div[1]/p[1]/text()'
    #xpath.tr.sa = '//*[@id="bp_doctor_about"]/div/div[2]/div/div[2]/div/div[2]/p[1]/span[1]'
    #xpath.att.sa = '//*[@id="bp_doctor_about"]/div/div[2]/div/div[2]/div/div[2]/p[2]/span[1]'
    xpath.pa.count = '//*[@id="bp_doctor_about"]/div/div[2]/div/div[2]/div/div[2]/p[1]/span[2]'
    xpath.pa.tw = '//*[@id="bp_doctor_about"]/div/div[2]/div/div[2]/div/div[2]/p[2]/span[2]'
    xpath.visit.num = '//*[@id="hitcnt"]'
    
    doctor.1$dc.name <- dc.inf.source.html %>%
        html_node(xpath = xpath.name) %>%
        html_text() %>%
        trimws()
    
    try_time = 0
    while(is.na(doctor.1$dc.name)){
        remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);',
                            args = list("dummy"))
        dc.inf.source <- remDr$getPageSource()
        dc.inf.source.html <- read_html(dc.inf.source[[1]])
        doctor.1$dc.name <- dc.inf.source.html %>%
            html_node(xpath = xpath.name) %>%
            html_text() %>%
            trimws()
        
        try_time = try_time + 1
        if (try_time > retry_times){
            break()
        }
    }
    #医生所在医院和科室
    doctor.1$dc.hosp <- dc.inf.source.html %>%
        html_node(xpath = xpath.hosp) %>%
        html_text() %>%
        trimws()
    #科室介绍链接
    #doctor.1$dc.hosp.url <- dc.inf.source.html %>%
    #html_node(xpath = xpath.hosp) %>%
    #html_attr('href') %>%
    #trimws()
    
    #医生的职称
    # dc.job <- dc.inf.source.html %>%
    #     html_node(xpath = xpath.job) %>%
    #     html_text() %>%
    #     trimws()
    
    # dc.info.table <- dc.inf.source.html %>%
    #     html_node(xpath = '//*[@id="bp_doctor_about"]/div/div[2]/div/table[1]') %>%
    #     html_table(fill = TRUE)
    # dc.job <- dc.info.table[2, 3]
    
    # 2017-12-29 23:16:00 有头像的网页 html_table 出错， 改用正刚表达式
    #doctor.1$dc.job <- dc.inf.source.html %>%
    #html_node(xpath = '//*[@id="bp_doctor_about"]/div/div[2]/div/table[1]') %>%
    #str_extract(regex("职　　称：.*\n.*")) %>%
    #str_extract(regex(">(.*?)<")) %>%
    #gsub('<|>', '', x = .)
    
    #医生个人主页链接
    doctor.1$dc.home.url <- dc.inf.source.html %>%
        html_nodes(xpath = xpath.personal_url) %>%
        html_text() %>%
        trimws()
    
    #看病经验总数
    doctor.1$dc.health.num <- dc.inf.source.html %>%
        html_node(xpath = xpath.health.num) %>%
        html_text() %>%
        gsub(pattern = "[(|)]", replacement = '', x = .) %>%
        as.integer()
    # 管道命令下，默认上一步输出的结果是第一个参数，
    # 但是这里的gsub 第三个参数才是 x, 所以用 x = . 一个点代替上步传过来的值
    
    # health.num <- str_extract(dc.health.num[1], '\\d+')
    # health.num <- as.integer(health.num)
    
    #医生感谢信总数
    doctor.1$dc.tl.num <- dc.inf.source.html %>%
        html_node(xpath = xpath.thanksLetters) %>%
        html_text() %>%
        trimws() %>%
        str_extract(pattern = '\\d+') %>%
        as.integer()
    #医生礼物总数
    doctor.1$dc.gift.num <- dc.inf.source.html %>%
        html_node(xpath = xpath.gift) %>%
        html_text() %>%
        trimws() %>%
        str_extract(pattern = '\\d+') %>%
        as.integer()
    #医生近两年热度
    doctor.1$dc.hot <- dc.inf.source.html %>%
        html_node(xpath = xpath.hot) %>%
        html_text() %>%
        trimws() %>%
        as.numeric()
    #帮助过病患累计数
    doctor.1$dc.pa.count <- dc.inf.source.html %>%
        html_node(xpath = xpath.pa.count) %>%
        html_text() %>%
        trimws() %>%
        str_extract('\\d+') %>%
        as.integer()
    
    #近两周帮助患者数
    doctor.1$dc.pa.tw <- dc.inf.source.html %>%
        html_node(xpath = xpath.pa.tw) %>%
        html_text() %>%
        trimws() %>%
        str_extract('\\d+') %>%
        as.integer()
    
    #医生信息页累计访问人数
    doctor.1$dc.visit.num <- dc.inf.source.html %>%
        html_node(xpath = xpath.visit.num) %>%
        html_text() %>%
        trimws() %>%
        str_extract('\\d+') %>%
        as.integer()
    #加上一列抓取的时间
    doctor.1$time <- Sys.time()
    
    # 用列表方便使用 lapply 来统一检查问题，长度为0补NA
    doctor.2 = lapply(doctor.1, function(x) {if(length(x)==0) return(NA) else x})
    
    dc.inf <- as.data.frame(doctor.2, stringsAsFactors = F) %>%
        dplyr::rename(
            doc.name = dc.name,
            doc.hosp = dc.hosp,
            #faculty.url = dc.hosp.url,
            #job = dc.job,
            homepage = dc.home.url,
            see.ex.count = dc.health.num,
            #see.ex.url = dc.pa.url,
            tl.num = dc.tl.num,
            #tl.url = dc.tl.url,
            gift.num = dc.gift.num,
            #gift.url = dc.gift.url,
            #homepage = dc.home.url,
            hot = dc.hot,
            #treat.sa = dc.tr.sa,
            #attitude.sa = dc.att.sa,
            total.patient = dc.pa.count,
            patient.tw = dc.pa.tw,
            info.visit.total = dc.visit.num
        )
    return(dc.inf)
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
                        file = sprintf("daily.data.1.%s.of.%s.begins.%s.csv",
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
    
    # 如果抓取某个医生的数据出现错误，抓取到的就是一个 error而不是 data.frame
    # 因此需要过滤掉，直接把对应的错误转成 NULL，不影响整体list的合并
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
                                 craw_fun = doctorCrawler_info)
end_time <- Sys.time()
write.csv(result.data, file = sprintf('daily_1.part_%s_of_%s.%s.csv',
                                      order_id, total_times, gsub(' |:|-', '.', Sys.time())),
          na = '', row.names = F)