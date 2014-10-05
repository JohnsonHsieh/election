library(rjson)
library(RCurl)
library(reshape)

get_table <- function(file){
  tmp <- fromJSON(getURL(file))
  out <- as.data.frame(do.call("rbind", tmp$hit_count))
  
  sources=do.call(rbind, tmp$sources)
  sources <- data.frame(id=rownames(sources), sources=do.call(rbind, tmp$sources))
  
  out$date <- as.factor(unlist(out$date))
  out$source <- as.numeric(out$source)
  out$count <- as.numeric(out$count)
  out$total_news <- as.numeric(out$total_news)
  
  out2 <- cast(data=out, formula=date~.,fun.aggregate=function(x)sum(x>0), value="source")
  out2$count <- cast(data=out, formula=date~.,fun.aggregate=function(x) sum(x), value="count")[,2]
  out2$total <- cast(data=out, formula=date~.,fun.aggregate=function(x) sum(x), value="total_news")[,2]
  colnames(out2) <- c("date", "source_counts", "news_counts", "total_news")
  out2$name <- tmp$info$name
  out2$date <- as.POSIXct(paste(substr(out2$date,1,4),substr(out2$date,5,6),substr(out2$date,7,8),sep="-"))
  out2$weekday <- as.numeric(strftime(out2$date,'%w'))
  out2 <- out2[,c(1,6,5,2,3,4)]
  out2
}
file <- "http://newstrend.g0v.ronny.tw/index/api/3"
get_table(file)

candidate <- list()

x <- c(1,3,67)
html <- paste("http://newstrend.g0v.ronny.tw/index/api/",x,sep="")
for(i in 1:length(x)){
  candidate[[i]] <- get_table(html[i])
}
candidate <- do.call(rbind, candidate)
dat_tpe <- subset(candidate, weekday>0 & weekday <6)



library(XML)
theurl <- "http://zh.wikipedia.org/wiki/2014%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%9B%B4%E8%BD%84%E5%B8%82%E9%95%B7%E5%8F%8A%E7%B8%A3%E5%B8%82%E9%95%B7%E9%81%B8%E8%88%89"
tables <- readHTMLTable(theurl)
poll <- tables[[7]]
colnames(poll) <- c("sources", "date", "柯文哲","連勝文","馮光遠","未表態")
poll <- poll[-c(1, nrow(poll)),]
poll$sources <- sub("[*]","", as.character(poll$sources))
poll$date <- as.POSIXct(paste(substr(poll$date,1,4),substr(poll$date,6,7),substr(poll$date,9,10),sep="-"))
poll[,3] <- as.numeric(sub("%","",poll[,3]))
poll[,4] <- as.numeric(sub("%","",poll[,4]))
poll[,5] <- as.numeric(sub("%","",poll[,5]))
poll[,6] <- as.numeric(sub("%","",poll[,6]))
poll

write.csv(poll, "poll.csv")
write.csv(dat_tpe, "news.csv")
