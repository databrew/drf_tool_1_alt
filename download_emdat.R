library(rvest)
library(httr)
library(xml2)
library(XML)

temp <- tempfile()
download.file("https://www.desinventar.net/DesInventar/download/DI_export_ago.zip",temp)
data <-unz(temp, xmlParse("DI_export_ago.xml"))
unlink(temp)

dat <- read_xml('www/DI_export_ago.xml')
xmlParse('www/DI_export_ago.xml')
xmlParse(file = data)
