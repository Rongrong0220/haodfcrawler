library(xml2)
library(rvest)
library(RSelenium)
library(stringr)
remDr <- remoteDriver(remoteServerAddr='localhost',port=4444L,
                      browserName='chrome')
remDr$open(silent = TRUE)
#爬取高血压医生列表的页数
url_dclist = 'http://www.haodf.com/jibing/gaoxueya/daifu_all_all_all_all_all.htm'
remDr$navigate(url_dclist)
remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);', args = list("dummy"))
setTimeout(type = 'page load', milliseconds = 10000)
web_dclist <- remDr$getPageSource()
dclist.page <- read_html(web_dclist[[1]]) %>% 
        html_nodes(xpath = '//*[@id="disease"]/div/div[1]/div[4]/div[2]/div/div/a[7]/span/font') %>% 
        html_text() %>% 
        trimws()
dclist.page <- as.integer(dclist.page)
#爬取所有高血压医生的名字、医院和信息页网址
dc.list.pp <- list()
a = 0
pb <- txtProgressBar(style = 3)
for(j in 1:dclist.page){
        url.all <- sprintf('http://www.haodf.com/jibing/gaoxueya/daifu_all_all_all_all_all_%d.htm', j)
        remDr$navigate(url.all)
        remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);', args = list("dummy"))
        websrc.dclist <- remDr$getPageSource()
        websrc.dc.all <- websrc.dclist[[1]]
        dc.name.pp <- read_html(websrc.dc.all) %>% 
                html_nodes(xpath = '//*[@id="disease"]/div/div[1]/div[4]/ul/li/div/div[2]/div[2]/p[1]/a[1]') %>% 
                html_text() %>% 
                trimws()
        dc.hosp.pp <- read_html(websrc.dc.all) %>%
                html_nodes(xpath = '//*[@id="disease"]/div/div[1]/div[4]/ul/li/div/div[2]/div[2]/p[1]/a[2]/span') %>%
                html_text() %>%
                trimws()
        dc.url.pp <- read_html(websrc.dc.all) %>% 
                html_nodes(xpath = '//*[@id="disease"]/div/div[1]/div[4]/ul/li/div/div[2]/div[2]/p[1]/a[1]') %>% 
                html_attr('href') %>% 
                trimws() 
        dc.list <- data.frame(name = dc.name.pp, hospital = dc.hosp.pp, homepage = dc.url.pp)
        dc.list.pp[[j]] <- dc.list
        a = a + 1
        setTxtProgressBar(pb, j/dclist.page)
}
close(pb)
data.dclist <- plyr::rbind.fill(dc.list.pp)
 
#删除医生列表中重复的医生
library(tidyverse)
dup.data <- data.dclist %>% 
        group_by(name, homepage) %>% 
        filter(row_number() == 1) %>%
        ungroup()
write.table(dup.data, file = "Dclist_1228_all.csv", append = TRUE, sep = ",", row.names = FALSE) 


