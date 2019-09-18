library(rvest)
library(httr)
library(xml2)
library(XML)

temp <- tempfile(tmpdir = tdir <- tempdir())

download.file("https://www.desinventar.net/DesInventar/download/DI_export_blz.zip",temp)
xml_files <-unzip(temp, exdir = tdir)
unlink(temp)

dat <- read_xml('www/DI_export_ago.xml')
xmlParse('www/DI_export_ago.xml')
xmlParse(file = data)

download.file('https://ourworldindata.org/947e43ae-51b5-4994-95ce-581ffe433a3a', temp)
read.csv(temp)
