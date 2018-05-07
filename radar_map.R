#匯入爬蟲爬下來的資料
ptt_exam=rreadLines("car_crawl.txt", encoding = "UTF-8")

ptt_exam$nrec=as.character(ptt_exam$nrec)
ptt_exam$內容=as.character(ptt_exam$內容)
ptt_exam$title=as.character(ptt_exam$title)

suppressPackageStartupMessages({
  library(httr)
  library(data.table)
  library(stringr)
  library(rvest)
  require(jiebaR)
  require(data.table)
  library(tidyverse)
  library(caTools)
  library(text2vec)
  library(stringr)
  library(pbapply)
  library(plyr)
})

#匯入字典
neg<- readLines("MDS讀書會05-實作(5)：文字資料視覺化_字典_negative words.txt", encoding = "UTF-8")
pos <- readLines("MDS讀書會05-實作(5)：文字資料視覺化_字典_positive words.txt", encoding = "UTF-8")

#正負面評價給予權重
weight <- rep(-1, length(neg))
neg <- cbind(neg, weight)

weight <- rep(1, length(pos))
pos <- cbind(pos, weight)

posneg <- rbind(pos, neg)
posneg = as.data.frame(posneg)

######################## 整合數據 ##############################
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
                                                                                                  ifelse(ptt_exam$nrec=="X8",-80,ptt_exam$nrec))))))))))))

ptt_exam$four_class= as.factor(ifelse(ptt_exam$result==100, "great",
                                      ifelse(ptt_exam$result>0 , "good", 
                                             ifelse(ptt_exam$result<0,"bad","soso"))))

ptt_exam$two_result= as.factor(ifelse(ptt_exam$result>=summary(ptt_exam$result)[5], "great",
                                      ifelse(ptt_exam$result< summary(ptt_exam$result)[5] , "soso","soso")))


u=str_match(ptt_exam$title, "\\[(.*)\\]")[,2]  %>% 
  ifelse(is.na(.)==T, "No",.) 
u[grep(" ", u)]="emp"

ptt_exam$type=as.factor(u)

######################## 分詞 ##############################
#數據轉換
#從ptt_exam數據框中提取內容並將其轉換為多個內容列表
ptt_list_ptt_exam=c()
for(i in 1:length(ptt_exam$nrec)){
  name=sprintf("No.%s_%s",i,ptt_exam[i,2])
  ptt_list_ptt_exam[name]=c(ptt_exam[i,7])
  print(i/length(ptt_exam$nrec))
}
str(ptt_list_ptt_exam[[1]])
  
 #匯入jiebar切詞的字典
cutter <- worker(bylines=F, dict = "dict.txt.big.txt") 
#細分每篇文章
ptt_split_ptt_exam <- sapply(ptt_list_ptt_exam, function(x) {
  segment(x, cutter)})
#刪除可能毫無意義的中文無用詞
#載入停頓詞字典
stopwords<-readLines('/home/cheating/Downloads/ch5/stopwords.txt')
stopwords

for(i in 1:length(ptt_split_ptt_exam)){
  ar=names(ptt_split_ptt_exam)[i]
  ptt_split_ptt_exam[ar]=list(gsub('tkb','TKB',ptt_split_ptt_exam[[i]]))
  print(i)
}

mm=ptt_split_ptt_exam[1]
mm=list(gsub('說','TKB',ptt_split_ptt_exam[[1]]))

#根據上述文章詞語構建一個詞彙表
ptt_split_ptt_exam.token <- itoken(ptt_split_ptt_exam)

ptt_split_ptt_exam.vocab <-create_vocabulary(ptt_split_ptt_exam.token, sep_ngram = ' ',
                                           stopwords =stopwords ,ngram = c(1L,1L)) 
                                          
#修剪文本
ptt_split_ptt_exam.vocab= prune_vocabulary(ptt_split_ptt_exam.vocab , 
                                        term_count_min = 5, 
                                        doc_proportion_max = 0.5,
                                        doc_proportion_min = 0.0005)
ptt_tf=data.frame(ptt_split_ptt_exam.vocab$vocab)
length(ptt_tf$terms)

escapeRegex <- function(string) {
  stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
}

vectorizer = vocab_vectorizer(ptt_split_ptt_exam.vocab)
dtm_ptt_exam = create_dtm(ptt_split_ptt_exam.token, vectorizer)

dtm_ptt_exam=cbind(dtm_ptt_exam, type=ptt_exam$type, result=ptt_exam$result, two_result=ptt_exam$two_result )

#訓練集和測試集
idx=sample.split(dtm_ptt_exam[,dim(dtm_ptt_exam)[2]], SplitRatio = 0.2)
dtm_ptt_exam_train=dtm_ptt_exam[idx==F,]
dtm_ptt_exam_test=dtm_ptt_exam[idx==T,]

tfidf = TfIdf$new()
dtm_ptt_exam_tfidf = fit_transform(dtm_ptt_exam, tfidf)

dtm_ptt_exam_tfidf=cbind(dtm_ptt_exam_tfidf, type=ptt_exam$type, result=ptt_exam$result, two_result=ptt_exam$two_result )

#訓練集和測試集
idx=sample.split(dtm_ptt_exam_tfidf[,dim(dtm_ptt_exam_tfidf)[2]], SplitRatio = 0.2)
dtm_ptt_exam_tfidf_train=dtm_ptt_exam_tfidf[idx==F,]
dtm_ptt_exam_tfidf_test=dtm_ptt_exam_tfidf[idx==T,]


vectorizer = vocab_vectorizer(ptt_split_ptt_exam.vocab)

tcm_ptt_exam <- create_tcm(ptt_split_ptt_exam.token, vectorizer)

glove = GlobalVectors$new(word_vectors_size = 100, vocabulary = ptt_split_ptt_exam.vocab, x_max = 30)
glove$fit_transform(tcm_ptt_exam, n_iter = 40)
ptt.word.vec <-glove$get_word_vectors()
ptt.word.vec = t(ptt.word.vec)

rownames(ptt.word.vec) = rownames(tcm_ptt_exam)
write.csv(ptt.word.vec, "ptt.word.vec.csv")

######################## 載入library ##############################
suppressPackageStartupMessages({
  library(reshape2)
  library(shiny)
  library(radarchart)
  library(httr)
  library(data.table)
  library(stringr)
  library(rvest)
  require(jiebaR)
  require(data.table)
  library(tidyverse)
  library(caTools)
  library(text2vec)
  library(stringr)
  library(pbapply)
  library(plyr)
})

ptt.word.vec=ptt.word.vec[,-101]

get_analogy_three = function(add1=king, add2= woman, deduct=man, display=150 ) {
  
  queen = ptt.word.vec[add1,, drop=F]  + ptt.word.vec[add2, , drop=F] - ptt.word.vec[deduct, ,drop=F]# please complete this!
  
  
  cos.dist = text2vec:::sim2(x = queen, y = ptt.word.vec,
                             method = "cosine", norm = "l2")# please complete this!
  
  # 顯示此類比任務的前10個單詞
  head(sort(cos.dist[1,], decreasing = T), display)
}




get_analogy_two = function(add1=king,add2= woman, deduct=man,display=20) {

  queen = ptt.word.vec[add1, , drop=F]  + ptt.word.vec[add2, , drop=F] #- ptt.word.vec[deduct, ,drop=F]# please complete this!

  cos.dist = text2vec:::sim2(x = queen, y = ptt.word.vec,
                             method = "cosine", norm = "l2")# please complete this!
  
  #顯示此類比任務的前10個單詞
  head(sort(cos.dist[1,], decreasing = T), display)
}


######################## 關鍵詞提取 ##############################


#在這個領域互相競爭的品牌
brand=read_lines('/home/cheating/Downloads/ch5/Brand.txt')
brand = gsub('volvo', 'Volvo',brand)


recom=c('推薦','馬力','扭力','性能','省油','配備','價格','引擎','汽缸','舒適','煞車')
all_vec=list()
for(i in 1:length(recom)){

  u=data.frame( a=get_analogy_two(add1 = recom[i], add2 = "品牌",display = 2000) ) %>%  mutate(.,row=row.names(.))
  name=sprintf('%s', recom[i])
  all_vec[name]=list(u[u$row%in%brand[brand %in% u$row],])
  print(i/length(recom))
}

#數據清理
all_vec=do.call("rbind", lapply(all_vec, data.frame))
all_vec$rown=row.names(all_vec)
all_vec=cbind(colsplit(all_vec$rown, pattern = '\\.',names = c('keyword','brands')), 
         cor=all_vec[,1])
brand_label <-spread(key=brands, value=cor, data = all_vec)



#視覺化圖表-雷達圖

ser=shinyServer(function(input, output) {
  output$radar <- renderChartJSRadar({
    
    chartJSRadar(brand_label[, c("keyword", input$selectedPeople)], 
                 maxScale = 0.5, showToolTipLabel=TRUE)
  })
})



ui=shinyUI(pageWithSidebar(
  headerPanel('品牌雷達圖'),
  sidebarPanel(
    checkboxGroupInput('selectedPeople', 'select checkbox', 
                       names(brand_label)[-1], selected="BMW")
  ),
  mainPanel(
    chartJSRadarOutput("radar", width = "450", height = "300"), width = 8
  )
))

shinyApp(ui = ui,server = ser )
