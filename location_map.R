#匯入爬蟲爬下來的資料
ptt_exam=rreadLines("car_crawl.txt", encoding = "UTF-8")

suppressPackageStartupMessages({
  library(httr)
  library(data.table)
  library(stringr)
  library(rvest)
  require(jiebaR)
  require(data.table)
  library(tidyverse)
  library(plotly)
})

#ptt板的名稱
forums = "car"

title.url <- list()
ptt_exam$result= as.numeric(ifelse(ptt_exam$nrec=="",0,
                                   ifelse(ptt_exam$nrec=="爆", 100,
                                          ifelse(ptt_exam$nrec=="XX",-100,
                                                 ifelse(ptt_exam$nrec=="X1",-10,
                                                        ifelse(ptt_exam$nrec=="X2",-20,
                                                               ifelse(ptt_exam$nrec=="X3",-30,
                                                                      ifelse(ptt_exam$nrec=="X4",-40,
                                                                             ifelse(ptt_exam$nrec=="X5",-50,
                                                                                    ifelse(ptt_exam$nrec=="X6",-60,
                                                                                           ifelse(ptt_exam$nrec=="X7",-70,
                                                                                                  ifelse(ptt_exam$nrec=="X8",-80, ptt_exam$nrec))))))))))))


ptt_exam$four_class= as.factor(ifelse(ptt_exam$result==100, "great",
                                      ifelse(ptt_exam$result>0 , "good", 
                                             ifelse(ptt_exam$result<0,"bad","soso"))))

ptt_exam$two_result= as.factor(ifelse(ptt_exam$result>=summary(ptt_exam$result)[5], "great",
                                      ifelse(ptt_exam$result< summary(ptt_exam$result)[5] , "soso","soso")))


u=str_match(ptt_exam$title, "\\[(.*)\\]")[,2]  %>% 
  ifelse(is.na(.)==T, "No",.) 
u[grep(" ", u)]="emp"

ptt_exam$type=u

######################## 整理數據 ##############################
name <- list()
count <- list()
BrandArticle <- list()

name <- readLines("Brand.txt", encoding = "UTF-8")

#提取關於品牌名稱的文章
for(k in 1:length(name)){
  count[k] <- length(grep(name[k], ptt_exam$內容, ignore.case = T))
  BrandArticle[k] <- list(grep(name[k], ptt_exam$內容, ignore.case = T))
  print(sprintf('progress: %s', round(k/length(name),2)))
}

brand_df <- data.table(name = name, count = as.numeric(count), ArticleID = BrandArticle)
######################## 處理正負面的評語 ##############################

#載入正負面的評語
neg<- readLines("negative words.txt", encoding = "UTF-8")
pos <- readLines("positive words.txt", encoding = "UTF-8")

#對正負面給予權重
weight <- rep(-1, length(neg))
neg <- cbind(neg, weight)

weight <- rep(1, length(pos))
pos <- cbind(pos, weight)

posneg <- rbind(pos, neg)
posneg = as.data.frame(posneg)

######################## 將文章分詞並取出正負面的評語 ##############################
cutter <- worker()

seg_tmp = list()
article_pos = list()
article_neg = list()
article_rec=list()
#檢查在for循環中放置什麼
mean(c(ptt_exam[brand_df$ArticleID[[1]],8]$result), na.rm = T)

a=as.numeric(ptt_exam[brand_df$ArticleID[[1]],8]$result)

a= 0
article_tmp = list()
for (i in ptt_exam$內容) {
  a = a+1
  article_tmp[i] = list(i)
}




seg_tmp = segment(as.character(article_tmp[brand_df$ArticleID[[1]][4]]), cutter)
article_pos = plyr::count(as.character(seg_tmp) %in% as.character(pos))[2,2]
article_neg = plyr::count(as.character(seg_tmp) %in% as.character(neg))[2, 2]

#計算這些文章中涵蓋了多少正詞和負詞
for(l in 1:length(brand_df$ArticleID)){
  article_rec[l]=list(mean(c(ptt_exam[brand_df$ArticleID[[l]],8]$result), na.rm = T))
  if(length(brand_df$ArticleID[[l]] > 0)){
    seg_tmp = segment(as.character(article_tmp[brand_df$ArticleID[[l]]]), cutter)
    article_pos[l] = plyr::count(as.character(seg_tmp) %in% as.character(pos))[2, 2]
    article_neg[l] = plyr::count(as.character(seg_tmp) %in% as.character(neg))[2, 2]
  } else {
    article_pos[l] = 0
    article_neg[l] = 0
  }
  print(sprintf('progress: %s', round(l/length(brand_df$ArticleID),2)))
}

#將所有數據綁定在一起
brand_df_2 = cbind(brand_df, unlist(article_rec),unlist(article_pos), unlist(article_neg) )
colnames(brand_df_2) <- c("brand_name", "article_count", "article_id",'rec', "positive", "negative" ) #colname changed
brand_df_2=na.omit(brand_df_2)


######################## 繪圖 ##############################

#使用聚集函數將數據從寬數據轉換為長數據

brand_df_2_tmp <- brand_df_2 %>% gather("item", value, -1:-4)

brand_df_2_tmp_2 <- brand_df_2 %>% 
  group_by(brand_name, article_count, rec,positive, negative) %>% 
  summarise(sum=sum(positive, negative)) %>% mutate(pct=positive/sum * 100)

#權重
brand_df_2_tmp_2$trn=scale(brand_df_2_tmp_2$rec)*brand_df_2_tmp_2$pct

#設置繪圖的主題
howard_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "navy", color = "navy", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "white"),
      legend.position = "right",
      legend.justification = "bottom", 
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
      title = element_text(size = 20),
      plot.caption=element_text(size = 10)
    )
}
theme_set(howard_theme())
attach(brand_df_2)
# 正面評價較多的品牌
ggplot(brand_df_2, aes(reorder(brand_name, -positive), positive, fill = brand_name)) + 
   geom_bar(position = "dodge", stat="identity", width = 0.8) + 
  theme(axis.text.x =  element_text(angle = 35, color="black"))+
  theme(legend.position = "none") +
  labs( x = "品牌名稱",
        y = "正面評價數量",
        title = "正面評價較多的品牌",
        caption = "數據來源: PTT汽車板")

# 負面評價較多的品牌
ggplot(brand_df_2, aes(x = reorder(brand_name, -negative), y =  negative, fill = brand_name)) + 
  geom_bar(position = "dodge", stat="identity", width = 0.8) + 
  theme(axis.text.x =  element_text(angle = 35, color="black"))+
  theme(legend.position = "none") +
  labs( x = "品牌名稱",
        y = "負面評價數量",
        title = "負面評價較多的品牌",
        caption = "數據來源: PTT汽車板")

# 品牌在PTT中的文章數量比較圖
ggplot(brand_df_2, aes(reorder(brand_name, -article_count), article_count, fill = brand_name)) +
  geom_bar(position = "dodge", stat="identity", width = 0.8) + 
  theme(axis.text.x =  element_text(angle = 35, color="black"))+
  theme(legend.position = "none") +
  labs( x = "品牌名稱",
        y = "品牌在PTT中的文章數量",
        title = "品牌在PTT中的文章數量比較圖",
        caption = "數據來源: PTT汽車板")

# 品牌的正評/負評數量比較圖
ggplot(brand_df_2_tmp, aes(reorder(brand_name, -article_count), value, fill = item)) +
  geom_bar(stat = "identity",position = "dodge", width = 0.8)+ 
  theme(axis.text.x =  element_text(angle = 35, color="black"))+
  scale_fill_discrete(name="評價狀況",
                      breaks=c("negative", "positive"),
                      labels=c("負評價", "正評價"))+
  labs( x = "品牌名稱",
        y = "品牌的正評/負評數量",
        title = "品牌的正評/負評數量比較圖",
        caption = "數據來源: PTT汽車板")
 # 品牌的正評/負評數量比較百分比
ggplot(brand_df_2_tmp, aes(reorder(brand_name, -article_count), value, fill = item)) +
  geom_bar(stat = "identity",position = "fill", width = 0.8)+ 
  theme(axis.text.x =  element_text(angle = 35, color="black"))+
  scale_fill_discrete(name="評價狀況",
                      breaks=c("negative", "positive"),
                      labels=c("負評價", "正評價"))+
  labs( x = "品牌名稱",
        y = "品牌的正評/負評數量百分比",
        title = "品牌的正評/負評數量比較百分比",
        caption = "數據來源: PTT汽車板")

# 正評價分數比較圖
ggplot(brand_df_2_tmp, aes(reorder(brand_name, -rec),y =  rec,fill = brand_name)) +
  geom_bar(stat = "identity",position = "dodge", width = 0.8)+ 
  theme(axis.text.x =  element_text(angle = 35, color="black"))+
  theme(legend.position = "none") +
  labs( x = "品牌名稱",
        y = "正評價分數",
        title = "正評價分數比較圖",
        caption = "數據來源: PTT汽車板")
        
 #聲量與好感度分佈圖
ggplot(data=brand_df_2_tmp_2, aes(x = article_count  , y = pct, colour = brand_name)) +
  geom_point(aes(size = rec, colour =brand_name )) +
  geom_text(aes(label=brand_name), size = 3, hjust=.5, vjust=-.99) +
  geom_vline(xintercept=mean(brand_df_2_tmp_2$article_count), lty=2) +
  geom_hline(yintercept=mean(brand_df_2_tmp_2$pct, na.rm = T), lty=2) + 
  theme(legend.title=element_blank())+
  labs(x = "網路聲量",y = "網路好感度",
       title = "聲量與好感度分佈圖",
       caption = "數據來源: PTT汽車板")
