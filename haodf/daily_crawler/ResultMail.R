
# 先定位到文件夹
root_dir <- "/haodf" 

# 后面方便发送邮件
date_today <- Sys.Date()
work_dir_today <- sprintf("%sdaily_data_%s", root_dir, gsub('-', '', date_today))
setwd(work_dir_today)


library(mailR)

mail.text <- sprintf("Hi xxx, \n    这是今天的数据(来自云服务器).请查收.\n%s", date_today)


result.files = list.files(pattern = "daily_.+.gbk.csv")
mail.attach <- sprintf("daily_data_%s.zip", gsub('-', '', date_today))
zip(zipfile = mail.attach, files = result.files)

sender <- "xxx"  # Replace with a valid address, only one!
recipients <- c('xxx', 'xxx')
email <- send.mail(from = sender,
                   to = recipients,
                   # cc = recipients, # 抄送人
                   subject=sprintf("今日数据%s", date_today) ,
                   body = mail.text,
                   smtp = list(host.name = "smtp.163.com",
                               port = 465, user.name='xxx',
                               passwd='xxx', ssl=T),
                   authenticate = T,
                   encoding = 'utf-8',
                   attach.files = mail.attach,
                   send = FALSE)
email$send() # execute to send email
