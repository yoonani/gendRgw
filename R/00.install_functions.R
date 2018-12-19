install.packages("tidyverse")
install.packages("rvest")

install.packages("tm")
install.packages("KoNLP")
install.packages("wordcloud2")
install.packages("topicmodels")

install.packages("extrafont")

install.packages("XML")

install.packages("sna")
install.packages("igraph")


# 글꼴 설정
# 사용한 글꼴은 한국출판인회의 KoPub 폰트를 사용
# 글꼴 다운로드 : http://www.kopus.org/biz/electronic/font.aspx
font_import(pattern="KoPub")
fonts()
loadfonts(device="win")


# mapping
install.packages("digest")
devtools::install_github("dkahle/ggmap")

install.packages(c("maptools", "rgeos", "rgdal"))
install.packages("leaflet")
install.packages(c("mapplots", "shapefiles"))



ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos22(d))
  extracted <- str_match(pos, '([가-힣]+)/NC')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}


korean.words <- function(doc){
  d <- as.character(doc)
  extractNoun(d)
}

naverDate2Date <- function (x) {
  dateTemp <- gsub("^[[:upper:]]{1}[[:lower:]]{2}, ([[:digit:]]{2}) ([[:upper:]]{1}[[:lower:]]{2}) ([[:digit:]]{4}) ([0-9:+ ]{1,})", "\\3-\\2-\\1", x)
  dateTemp <- strsplit(dateTemp, "-") 
  for(i in seq_along(dateTemp)) {
    #  print(i)
    dateTemp[[i]][2] <- match(dateTemp[[i]][2], month.abb)
    dateTemp[[i]] <- paste(dateTemp[[i]], collapse="-")
  }
  dateTemp <- as.character( unlist(dateTemp) )
  as.Date(dateTemp)
}
