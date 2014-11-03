library(rjson)
library(RCurl)
library(reshape)
library(XML)

get_news_table <- function(url){
  tmp <- fromJSON(getURL(url))
  out <- as.data.frame(do.call("rbind", tmp$hit_count))
  
  sources <- do.call(rbind, tmp$sources)
  sources <- data.frame(id=rownames(sources), sources=do.call(rbind, tmp$sources))
  
  out$date <- as.factor(unlist(out$date))
  out$source <- as.numeric(out$source)
  out$count <- as.numeric(out$count)
  out$total_news <- as.numeric(lapply(out$total_news, function(x) ifelse(is.null(x), NA, x)))
  
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

get_poll_table <- function(url){
  tables <- readHTMLTable(url)
  options(warn=1)
  poll <- tables[[7]]
  colnames(poll) <- c("sources", "date", "柯文哲","連勝文","馮光遠", "陳永昌","未表態")
  poll <- poll[-c(1, nrow(poll)),]
  poll$sources <- sub("[*]","", as.character(poll$sources))
  poll$date <- as.POSIXct(paste(substr(poll$date,1,4),substr(poll$date,6,7),substr(poll$date,9,10),sep="-"))
  poll[,3] <- as.numeric(sub("%","",poll[,3]))
  poll[,4] <- as.numeric(sub("%","",poll[,4]))
  poll[,5] <- suppressWarnings(as.numeric(sub("%","",poll[,5])))
  poll[,6] <- suppressWarnings(as.numeric(sub("%","",poll[,6])))
  poll[,7] <- suppressWarnings(as.numeric(sub("%","",poll[,7])))
  poll[is.na(poll[,5]),5] <- 0
  poll[is.na(poll[,6]),6] <- 0
  poll[is.na(poll[,7]),7] <- 0
  # poll[,3] <- round(poll$柯文哲 + 0.435*poll$未表態,2)
  # poll[,4] <- round(poll$連勝文 + 0.565*poll$未表態,2)
  # w <- 0.2644231
  # d <- 5
  w <- 1-0.6892857
  d <- 2
  poll[,3] <- round(poll$柯文哲 + w * (poll$未表態-d),2)
  poll[,4] <- round(poll$連勝文 + (1-w)*(poll$未表態-d),2)
  #poll[,3] <- round(poll$柯文哲)
  #poll[,4] <- round(poll$連勝文)
  poll <- melt(poll[, c("date","柯文哲","連勝文","馮光遠")], id="date")
  colnames(poll) <- c("date", "name", "score")
  poll  
}





#----------------------------------------------------
# Import news data

x <- c(1,3,67)
ronny_url <- paste("http://newstrend.g0v.ronny.tw/index/api/",x,sep="")
data_news <- list()
for(i in 1:length(x)){
  data_news[[i]] <- get_news_table(ronny_url[i])
}
data_news <- do.call(rbind, data_news)

#----------------------------------------------------
# Import facebook data

link_url <- c("http://link.que.tw/searchchart.php?search=%E9%80%A3%E5%8B%9D%E6%96%87",
              "http://link.que.tw/searchchart.php?search=%E6%9F%AF%E6%96%87%E5%93%B2",
              "http://link.que.tw/searchchart.php?search=%E9%A6%AE%E5%85%89%E9%81%A0")
data_fb <- list()
for(i in 1:3){
  data_fb[[i]] <- get_facebook_table(link_url[i])
}
data_fb <- do.call(rbind, data_fb)
data_fb_new <- data_fb
data_fb_old <- read.csv("data_fb.csv")
data_fb_old$date <- as.POSIXct(data_fb_old$date)
rbind(data_fb_old[1:15,], data_fb_new[1:5,])
id <- which(!data_fb_old[,1]%in%data_fb_new[,1])
tmp <- rbind(data_fb_old[id,], data_fb_new)
data_fb <- tmp[order(tmp$date, tmp$name),]

#----------------------------------------------------
wiki_url <- "http://zh.wikipedia.org/wiki/2014%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%9B%B4%E8%BD%84%E5%B8%82%E9%95%B7%E5%8F%8A%E7%B8%A3%E5%B8%82%E9%95%B7%E9%81%B8%E8%88%89"
data_poll <- get_poll_table(wiki_url)

dat <- Reduce(function(x,y) merge(x,y,all=TRUE), list(data_news, data_fb, data_poll))
dat <- subset(dat, name!="馮光遠" & date>"2014-07-31")
# tail(dat,20)

KP <- subset(dat, name=="柯文哲")
LD <- subset(dat, name=="連勝文")
KP.loess <- loess(score~as.numeric(date), data=KP)
LD.loess <- loess(score~as.numeric(date), data=LD)
KP.pred <- predict(KP.loess, newdata=KP$date)
LD.pred <- predict(LD.loess, newdata=LD$date)

dat2 <- rbind(cbind(KP, est_score=KP.pred), cbind(LD, est_score=LD.pred))
dat2 <- subset(dat2, date>"2014-09-15" & date <"2014-10-17")
KP <- subset(dat2, name=="柯文哲")
LD <- subset(dat2, name=="連勝文")

library(ggplot2)
library(splines)
library(MASS)
library(RColorBrewer)

ggplot(KP, aes(x=opposite, y=est_score)) + stat_smooth(se=F, method=lm, lwd=2, col="#009E73") + geom_point(size=3) +
  labs(x="反柯聲浪", y="柯文哲支持度") + theme(text=element_text(size=18))
ggplot(LD, aes(x=opposite, y=est_score)) + stat_smooth(se=F, method=lm, lwd=2, col="#0072B2") + geom_point(size=3) +
  labs(x="反連聲浪", y="連勝文支持度") + theme(text=element_text(size=18))
ggplot(KP, aes(x=KP$opposite, y=LD$est_score)) + stat_smooth(se=F, method=lm, lwd=2, col="#0072B2") + geom_point(size=3) +
  labs(x="反柯聲浪", y="連勝文支持度") + theme(text=element_text(size=18))
ggplot(LD, aes(x=LD$opposite, y=KP$est_score)) + stat_smooth(se=F, method=lm, lwd=2, col="#009E73") + geom_point(size=3) +
  labs(x="反連聲浪", y="柯文哲支持度") + theme(text=element_text(size=18))


KP$month <- factor((KP$date)>"2014-09-30", labels = c("Sep","Oct"))
LD$month <- factor((LD$date)>"2014-09-30", labels = c("Sep","Oct"))

# (x="反柯聲浪", y="柯文哲支持度")
ggplot(KP, aes(x=opposite, y=est_score, colour=month, group=month)) + 
  stat_smooth(se=F, method="lm", formula=y~ns(x,1), lwd=4) + 
  geom_point(size=5, alpha=0.7) + geom_hline(yintercept=41.5, lty=2, col="gray50", lwd=2)+
  labs(x="", y="") + theme(text=element_text(size=40), legend.position="none") +
  scale_colour_hue(h = c(80,150), l=50)
ggsave("antiKP-suppKP.png")

# (x="反連聲浪", y="連勝文支持度")
ggplot(LD, aes(x=opposite, y=est_score, colour=month, group=month)) + 
  stat_smooth(se=F, method="lm", formula=y~ns(x,1), lwd=4) +
  geom_point(size=5, alpha=0.7) + 
  labs(x="", y="") + theme(text=element_text(size=40), legend.position="none") +
  scale_colour_hue(h = c(200, 280), l=50)
ggsave("antiLD-suppLD.png")

# (x="反柯聲浪", y="連勝文支持度")
ggplot(KP, aes(x=KP$opposite, y=LD$est_score, colour=month, group=month)) + 
  stat_smooth(se=F, method=lm, formula=y~ns(x,1), lwd=4) + 
  geom_point(size=5, alpha=0.7) + 
  labs(x="", y="") + theme(text=element_text(size=40), legend.position="none") +
  scale_colour_hue(h = c(200, 280), l=50)
ggsave("antiKP-suppLD.png")  

# (x="反連聲浪", y="柯文哲支持度")
ggplot(LD, aes(x=LD$opposite, y=KP$est_score, colour=month, group=month)) + 
  stat_smooth(se=F, method=lm, formula=y~ns(x,1), lwd=4) + 
  geom_point(size=5, alpha=0.7) + geom_hline(yintercept=41.5, lty=2, col="gray50", lwd=2)+
  labs(x="", y="") + theme(text=element_text(size=40), legend.position="none") +
  scale_colour_hue(h = c(80,150), l=50)
ggsave("antiLD-suppKP.png")



ggplot(KP, aes(x=KP$opposite, y=LD$est_score, colour=month, group=month)) + 
  stat_smooth(se=F, method=lm, formula=y~ns(x,1), lwd=4) + 
  geom_point(size=5, alpha=0.7) + 
  labs(x="", y="") + theme(text=element_text(size=40)) +
  scale_colour_hue(h = c(200, 280), l=50)




ggplot(data = dat2, aes(x=date, y=est_score, colour=name)) +
  geom_abline(intercept=50, slope=0, col="gray50", lty=2, lwd=1) +
  geom_line(size=2) + geom_point(aes(y=score), size=3) +
  theme(text=element_text(size=20)) + 
  labs(x="日期", y="支持度") + ylim(46,54)


pred <- data.frame(KP.pred, LD.pred)
rowSums(pred)
# pred <- sweep(pred, MARGIN=1, STATS=rowSums(pred), FUN="/")*100
dat$est_score <- dat$score
dat$est_score[dat$name=="柯文哲"] <- pred$KP.pred
dat$est_score[dat$name=="連勝文"] <- pred$LD.pred



id <- strftime(KP$date, "%w")%in%c(1:5)

tmp <- data.frame(KP.pred[id], (KP$news_counts/(KP$news_counts+LD$news_counts))[id])
plot(tmp)


id2 <- KP$date>"2014-09-18" & KP$date <"2014-10-16"
pred


tmp <- data.frame(KP.pred[id2], (LD$opposite/LD$heat)[id2])
plot(tmp)

tmp <- data.frame((KP$opposite)[id2], LD.pred[id2])
plot(tmp)
lines(loess(tmp))


tmp <- data.frame((KP$support/KP$heat)[id2], LD.pred[id2])
plot(tmp)


tmp <- data.frame((LD$support/LD$heat)[id2], KP.pred[id2])
plot(tmp)

tmp <- data.frame((LD$opposite/LD$heat)[id2], KP.pred[id2])
plot(tmp)



tmp <- data.frame(LD.pred[id], (LD$news_counts/(KP$news_counts+LD$news_counts))[id])
plot(tmp)

tmp <- data.frame(LD.pred[id], (LD$opposite/LD$heat)[id])
plot(tmp)





df <- dat[strftime(dat$date, "%w")%in%c(1:5),]
ggplot(data = df, aes(x=date, y=news_counts, colour=name)) + geom_line(size=2) + 
  theme(text=element_text(size=20)) + labs(x="日期", y="新聞數")

ggplot(data = df, aes(x=date, y=news_counts, colour=name)) + geom_point() + stat_smooth(size=2, se=F) + 
  theme(text=element_text(size=20)) + labs(x="日期", y="新聞數") + ylim(150, 350)

dat$opp_rate <- dat$opposite/dat$heat
dat$sup_rate <- dat$support/dat$heat
df <- dat[strftime(dat$date, "%w")%in%c(1:5),]
ggplot(data=df, aes(x=date, y=support-opposite, colour=name)) + geom_point() + stat_smooth(size=2) + 
  theme(text=element_text(size=20)) + labs(x="日期", y="支持度")

tail(dat,20)

dim(KP)
rowSums(cbind(KP$est_score, LD$est_score))

plot(g1)
points(KP$date, y, col=2)




plot(g1)
points(predict(g1, newdata=subset(dat, name=="柯文哲", c(date)))

glm

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

old_fb <- read.csv("data_fb.csv")
write.csv(data_fb, "tmp.csv", row.names=FALSE)
data_fb <- read.csv("tmp.csv")
tmp <- rbind(old_fb,data_fb)

old_fb[!duplicated(tmp$date)[1:nrow(old_fb)],]
