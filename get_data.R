library(rjson)
library(RCurl)
library(reshape)

get_news_table <- function(url){
  tmp <- fromJSON(getURL(url))
  out <- as.data.frame(do.call("rbind", tmp$hit_count))
  
  sources <- do.call(rbind, tmp$sources)
  sources <- data.frame(id=rownames(sources), sources=do.call(rbind, tmp$sources))
  
  out$date <- as.factor(unlist(out$date))
  out$source <- as.numeric(out$source)
  out$count <- as.numeric(out$count)
  out$total_news <- as.numeric(out$total_news)
  
  
  out2 <- cast(data=out, formula=date~.,fun.aggregate=function(x) sum(x[x!=0]>0), value="source")
  out2$count <- cast(data=out, formula=date~.,fun.aggregate=function(x) sum(x), value="count")[,2]
  
  out2 <- merge(out2, subset(out, source==0, c(date, total_news)))
  
  colnames(out2) <- c("date", "source_counts", "news_counts", "total_news")
  out2$name <- tmp$info$name
  out2$date <- as.POSIXct(paste(substr(out2$date,1,4),substr(out2$date,5,6),substr(out2$date,7,8),sep="-"))
  #out2$weekday <- as.numeric(strftime(out2$date,'%w'))
  out2 <- out2[,c(1,5,2,3,4)]
  out2
}
# file <- "http://newstrend.g0v.ronny.tw/index/api/3"
# get_table(file)

data_news <- list()
x <- c(1,3,67)
url <- paste("http://newstrend.g0v.ronny.tw/index/api/",x,sep="")
for(i in 1:length(x)){
  data_news[[i]] <- get_news_table(url[i])
}
data_news <- do.call(rbind, data_news)
#data_news <- subset(candidate, weekday>0 & weekday <6)


#---------------------------------
url <- "http://link.que.tw/searchchart.php?search=%E9%80%A3%E5%8B%9D%E6%96%87"

get_facebook_table <- function(url){
  html <- htmlParse(getURL(url), encoding = "UTF-8")
  
  tmp <- xpathApply(html, "//script", xmlValue)[5]
  tmp <- substring(tmp, 213)
  tmp <- strsplit(tmp,"]\n]);")[[1]][1]
  tmp <- strsplit(tmp,"]\n,")[[1]]
  tmp <- substring(tmp,2)
  tmp <- gsub("[' ]","",tmp)
  
  write.table(tmp, "tmp.csv", quote=FALSE, row.names=FALSE)
  dat <- read.csv("tmp.csv",skip=1, h=1, stringsAsFactors = FALSE)
  names(dat) <- c("date", names(dat)[-1])
  names(dat)[c(3,4)] <- substr(names(dat)[c(3,4)],1,2)
  dat$date <- as.POSIXct(paste("2014-", dat$date, sep=""))
  dat <- data.frame(dat$date, names(dat)[2], dat[,2], dat$支持, dat$反對)
  colnames(dat) <- c("date", "name", "heat", "support", "opposite")
  dat
}

url <- c("http://link.que.tw/searchchart.php?search=%E9%80%A3%E5%8B%9D%E6%96%87",
         "http://link.que.tw/searchchart.php?search=%E6%9F%AF%E6%96%87%E5%93%B2",
         "http://link.que.tw/searchchart.php?search=%E9%A6%AE%E5%85%89%E9%81%A0")

data_fb <- list()
for(i in 1:3){
  data_fb[[i]] <- get_facebook_table(url[i])
}
data_fb <- do.call(rbind, data_fb)

dim(data_fb)

dat <- merge(data_news, data_fb)
dat$date

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
colnames(poll) <- c("source", "date", "KP", "LD", "VR", "UN")
as.numeric
g = gam(KP~s(as.numeric(date),k=50), data=poll)
?gam

plot(KP~date, data=poll)
predict(g, newdata = as.numeric(dat$date))
lines(poll$date, predict(g))

g=loess(poll$KP~poll$date)
predict(g)
?lowess
write.csv(poll, "poll.csv")
write.csv(dat_tpe, "news.csv")

