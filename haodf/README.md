Readme
================
Hecen
2018年7月23日

学习内容：
==========

curl + rvest 抓取静态网页内容
-----------------------------

RSelenium + rvest 实现对动态网页进行抓取
----------------------------------------

先对数据进行分块，进而实现多线程同时运行
----------------------------------------

遇到的问题：
============

网页编码不统一，导致抓取结果乱码
--------------------------------

直接使用原网页的xpath定位爬取位置导致不能获取预期内容
-----------------------------------------------------

收获：
======

爬取时注意网页编码，按需在代码中添加编码
----------------------------------------

使用curl获取的网页的xpath进行内容定位
-------------------------------------

当前解析总是等待下载，之后可以考虑异步
--------------------------------------
