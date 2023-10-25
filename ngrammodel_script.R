library(stringr)
library(quanteda)
library(dplyr)
library(ggplot2)
library(rJava)
library(qdap)
library(tm)
library(sentimentr)

##open data
setwd("C:/Users/juani/OneDrive/Documentos/R/capstone/Coursera-SwiftKey/final/en_US")
fileName="en_US.blogs.txt"
con=file(fileName,open="r")
blogs=readLines(con)

fileName="en_US.twitter.txt"
con2=file(fileName,open="r")
twitter=readLines(con2)

fileName="en_US.news.txt"
con3=file(fileName,open="r")
news=readLines(con3)

hunspell::dictionary(lang = "en_US")
english_words<-readLines("C:/Users/juani/AppData/Local/R/win-library/4.2/hunspell/dict/en_GB.dic")%>%gsub("/.+", "", .)


close(con,con2,con3)
rm(con,con2,con3, fileName)

#sample each file (all together sum up 3% of whole data, similar number of words per dataset)
set.seed(100)
sample_t <- sample(x=twitter, size = floor( 18.81941*length(twitter)/100), replace = F)
sample_n <- sample(x=news, size = length(news), replace = F)
sample_b <- sample(x=blogs, size = floor(15.23031*length(blogs)/100), replace = F)

##clean unnecessary data
rm(twitter,blogs,news)

##joining data
joined_data<-c(sample_t,sample_n, sample_b)
length(joined_data)
rm(sample_t,sample_n,sample_b)

## Separating training and testing
set.seed(101)
sample <- sample.int(n = length(joined_data), size = floor(.75*length(joined_data)), replace = F)
train <- joined_data[sample]
test  <- joined_data[-sample]

train<-replace_contraction(train)
test<-replace_contraction(test)
train<-tolower(train)
test<-tolower(test)

length(test)
length(train)
rm(joined_data,sample,sample_2)

train<-removePunctuation(train, preserve_intra_word_contractions = TRUE)
train<-gsub("'s","",train)
test<-removePunctuation(test, preserve_intra_word_contractions = TRUE)
test<-gsub("'s","",test)

profanity<-unique(c(tolower(lexicon::profanity_alvarez),
      tolower(lexicon::profanity_arr_bad),
      tolower(lexicon::profanity_banned),
      tolower(lexicon::profanity_zac_anger),
      tolower(lexicon::profanity_racist)))

##term document matrix and frequencies
save<-train
tokens<-tokens(train, what="word", remove_numbers=TRUE,
               remove_punct=TRUE,
               remove_symbols=TRUE)
tokens<-tokens_select(tokens,profanity, selection = "remove", padding = FALSE)

###finding word frequency and mle
dfm <- dfm(tokens, tolower = TRUE)
w_count <- colSums(dfm)
freq_word <- data.frame(part1=names(w_count),freq=w_count, row.names = NULL)

##remove words repeating 1-10 times and not in english dictionary
select_words<-freq_word$part1[which(freq_word$freq<=50)]
remove_words<-select_words[!(select_words %in% english_words)]
tokens<-tokens_select(tokens,remove_words, selection = "remove", padding = FALSE)

##words in english dictionary appearing once will be considered UNK(unknown)
dfm <- dfm(tokens, tolower = TRUE)
w_count <- colSums(dfm)
freq_word <- data.frame(part1=names(w_count),freq=w_count, row.names = NULL)
replace_words<-freq_word$part1[which(freq_word$freq==1)]

col <- paste0("\\b",replace_words[1:500],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train, fixed = FALSE)
col <- paste0("\\b",replace_words[501:1000],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
col <- paste0("\\b",replace_words[1001:1500],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
col <- paste0("\\b",replace_words[1501:2000],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
col <- paste0("\\b",replace_words[2001:2500],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
col <- paste0("\\b",replace_words[2501:3000],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
col <- paste0("\\b",replace_words[3001:3500],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
col <- paste0("\\b",replace_words[3501:4000],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
col <- paste0("\\b",replace_words[4001:length(replace_words)],"\\b",collapse="|")
train2<-mgsub(col,"UNK",train2, fixed = FALSE)
save(train2,file="train2.txt")

##I do the same process again with the new train data
tokens2<-tokens(train2, what="word", remove_numbers=TRUE,
               remove_punct=TRUE,
               remove_symbols=TRUE)
tokens2<-tokens_select(tokens2,profanity, selection = "remove", padding = FALSE)

dfm <- dfm(tokens2, tolower = TRUE)
w_count <- colSums(dfm)
freq_word <- data.frame(part1=names(w_count),freq=w_count, row.names = NULL)
select_words<-freq_word$part1[which(freq_word$freq<=50)]
remove_words<-select_words[!(select_words %in% english_words)]
tokens2<-tokens_select(tokens2,remove_words, selection = "remove", padding = FALSE)

dfm <- dfm(tokens2, tolower = TRUE)
w_count <- colSums(dfm)
freq_word <- data.frame(part1=names(w_count),freq=w_count, row.names = NULL)

##remove every word that repeats less than or 20 times

#rm(w_count,dfm)
remove_words_20<-freq_word$part1[which(freq_word$freq<=20)]
tokens2<-tokens_select(tokens2,remove_words_20, selection = "remove", padding = FALSE)

tokens2 <- tokens_replace(tokens2,
                         pattern = types(tokens2),
                         replacement = stringi::stri_replace_all_fixed(types(tokens2), "_", ""),
                         valuetype = "fixed")
tokens2 <- tokens_replace(tokens2,
                         pattern = types(tokens2),
                         replacement = stringi::stri_replace_all_fixed(types(tokens2), "-", ""),
                         valuetype = "fixed")
tokens2 <- tokens_replace(tokens2,
                         pattern = types(tokens2),
                         replacement = stringi::stri_replace_all_fixed(types(tokens2), " ", ""),
                         valuetype = "fixed")
#rm(english_words)

dfm <- dfm(tokens2, tolower = TRUE)
w_count <- colSums(dfm)
freq_word <- data.frame(part1=names(w_count),freq=w_count, row.names = NULL)
rm(w_count,dfm)

##finding mle
mle<-vector(mode = "double", length = nrow(freq_word))
for (i in 1:nrow(freq_word)){
  num<-freq_word[i,2]
  den<-sum(freq_word$freq)
  mle[i]<-num/den
}
freq_word$mle<-mle
rm(mle,remove_words_20, replace_words, remove_words, select_words,num,den)
freq_word<-filter(freq_word,part1!="")
save(freq_word,file="freqword2.Rda")
load("freqword2.Rda")

ngram_count<-function(tokens, ng){
  initial_t<-Sys.time()

  tokens_ngram <- tokens %>% tokens_tolower() %>%
    tokens_ngrams(n=ng)
  dfmn <- dfm(tokens_ngram)
  n_count <- colSums(dfmn)
  n_freq <- data.frame(word=names(n_count),freq=n_count, row.names = NULL)
  rm(dfmn,n_count)

  ##split ngram
  ngram<-n_freq[,1]
  split<-strsplit(ngram,"_")
  part1<-character(length = length(ngram))
  part2<-character(length = length(ngram))
  for (i in 1:length(ngram)){
    part2[i] <-split[[i]][length(split[[i]])]
    if (ng==2){
      part1[i] <-split[[i]][1]
    }
    if (ng==3){
      part1[i] <-paste(split[[i]][1],split[[i]][2], sep = " ")
    }
    if (ng==4){
      part1[i] <-paste(split[[i]][1],split[[i]][2],split[[i]][3], sep = " ")
    }
    if (ng==5){
      part1[i] <-paste(split[[i]][1],split[[i]][2],split[[i]][3],split[[i]][4], sep = " ")
    }
  }
  words_table<-data.frame(part1,part2,count=n_freq$freq)

  final_t<-Sys.time()-initial_t
  print(final_t)
  return(words_table)
}
two_counts<-ngram_count(tokens2,2)
three_counts<-ngram_count(tokens2,3)
four_counts<-ngram_count(tokens2,4)
five_counts<-ngram_count(tokens2,5)

##remove ngrams that appear once
two_counts<-filter(two_counts,count>20)
three_counts<-filter(three_counts,count>12)
four_counts<-filter(four_counts,count>5)
five_counts<-filter(five_counts,count>2)

head(five_counts[order(five_counts$count,decreasing=TRUE),],10)

##Good turing smoothing
##computing C*
#ngram_table_count<-two_counts
gtf<-function(ngram_table_count){
  initial_t<-Sys.time()
  count_table<-table(ngram_table_count$count)
  N<-as.data.frame(count_table)
  N$Var1<-as.integer(as.character(N$Var1))
  last_c<-tail(N$Var1,1)
  C<-data.frame(C=c(1:last_c))
  N$C<-N$Var1
  gt <- merge(C,N,by="C",all=T)
  gt <- gt%>%select(C,Freq)
  gt$Freq[is.na(gt$Freq)] <- 0
  remove(count_table,last_c,C)

  ##finding zr values
  colnames(N)<-c("C","NC")
  N$z<-vector(mode = "double", length = nrow(N))
  for (i in 1:nrow(N)){
    q<-max(N$C[N$C < N$C[i]])
    t<-min(N$C[N$C > N$C[i]])
    if (i==1){
      N$z[i]<-(N$NC[i])/(1/2*(t))
    }
    else if ((i>1) & (i<(nrow(N)))){
      N$z[i]<-(N$NC[i])/(1/2*(t -q))
    }
    else{
      N$z[i]<-(N$NC[i])/(1/2*(N$C[i]-q))
    }
  }
  #fit linear regression to zr
  N$logC<-log(N$C)
  N$logz<-log(N$z)
  zr_lm<-lm(logz~logC,data=N)

  #plot linear regression
  ggplot(N, aes(x = logC, y = logz)) + geom_point() + stat_smooth(method = "lm")

  ##find Zr (Nr) for c values with linear regression
  colnames(gt)[2]<-"NC"
  gt$logC<-log(gt$C)


  gt$logz<-predict(zr_lm,gt)
  gt$z<-exp(gt$logz)
  ggplot(gt, aes(x = logC, y = logz)) + geom_point() + stat_smooth(method = "lm")

  ##finding good turing c*
  gt$cgt<-vector(mode = "double", length = nrow(gt))
  for (i in 1:(nrow(gt)-1)){
    gt$cgt[i]<-(gt$C[i]+1)*(gt$z[i+1]/gt$z[i])
  }
  # for (i in 6:nrow(gt)){
  gt$cgt[nrow(gt)]<-gt$C[nrow(gt)]
  # }

  ###computing good turing pr
  gt$pr <- gt$cgt/sum(gt$cgt*gt$NC)

  ##final good turing table
  fgt<-data.frame(gt$C,gt$NC,gt$cgt,gt$pr)

  #adding zero count
  zero_count_pr<-gt$z[1]/sum(gt$cgt*gt$NC)
  fgt<-rbind(c(0,NA,0,zero_count_pr),fgt)

  ##finding dr
  fgt$dr<-fgt$gt.cgt/fgt$gt.C

  fgt_ngram<-rbind(c("<UNK>","<UNK>",0),select(ngram_table_count,part1,part2,count))
  fgt_ngram$prgt<-vector(mode = "double", length = nrow(fgt_ngram))
  fgt_ngram$d<-vector(mode = "double", length = nrow(fgt_ngram))
  for (i in 1:nrow(fgt_ngram)){
    filter_count<-filter(fgt,gt.C==fgt_ngram$count[i])
    fgt_ngram$prgt[i] <- filter_count%>%select(gt.pr)
    fgt_ngram$d[i] <- filter_count%>%select(dr)
  }
  fgt_ngram$prgt<-as.numeric(unlist(fgt_ngram%>%select(prgt)))
  fgt_ngram$d<-as.numeric(unlist(fgt_ngram%>%select(d)))
  fgt_ngram$count<-as.numeric(unlist(fgt_ngram%>%select(count)))
  final_t<-Sys.time()-initial_t
  print(final_t)
  return(fgt_ngram)
}

two_gram2<-gtf(two_counts)
three_gram3<-gtf(three_counts)
four_gram4<-gtf(four_counts)
five_gram5<-gtf(five_counts)

#saving files
save(two_gram2,file="twogram2.Rda")
save(three_gram3,file="threegram3.Rda")
save(four_gram4,file="fourgram4.Rda")
save(five_gram5,file="fivegram5.Rda")

#loading files
load("twogram2.Rda")
load("threegram3.Rda")
load("fourgram4.Rda")
load("fivegram5.Rda")

predict_word<-function(phrase,n,freq_word,two_gram=NULL,three_gram=NULL,four_gram=NULL,five_gram=NULL){
  #initial_t<-Sys.time()
  ##select model to use
  if (n==2){model<-two_gram; prev_model<-freq_word}
  if (n==3){model<-three_gram; prev_model<-two_gram}
  if (n==4){model<-four_gram; prev_model<-three_gram}
  if (n==5){model<-five_gram; prev_model<-four_gram}

  ##split phrase
  split_phrase<-strsplit(phrase," ")
  word1 <-split_phrase[[1]][1]
  if (n>=3){
    word2 <-split_phrase[[1]][2]
  }
  if (n>=4){
    word3 <-split_phrase[[1]][3]
  }
  if (n==5){
    word4 <-split_phrase[[1]][4]
  }


  #find previous ngram and compute backoff probability
  pkatz<-function(words,prev_model,model,phrase,m){
    if (m==2){
      seen_bi<-filter(two_gram,part1==words)$part2
      katz_selection<-filter(freq_word,!(part1%in%seen_bi))
      den <-sum(katz_selection$mle)
    }else{
      seen<-filter(model,part1==phrase)$part2
      katz_selection<-filter(filter(prev_model,part1==words),!(part2%in%seen))
      den <-sum(katz_selection$count*katz_selection$d)/sum(katz_selection$count)
    }
    if (dim(katz_selection)[1]!=0){
      filter_num<-filter(model,part1==phrase)
      num<-1-(sum(filter_num$count*filter_num$d)/sum(filter_num$count))

      alpha <- num/den

      if(m==2){
        for (i in 1:nrow(katz_selection)){
          katz_selection$pkz[i] <- alpha*katz_selection$mle[i]
        }
      }else{
        for (i in 1:nrow(katz_selection)){
          katz_selection$pkz[i] <- alpha*((katz_selection$count[i]*katz_selection$d[i])/sum(katz_selection$count))
        }
      }
    }
    return(katz_selection)
  }

  katz_model<-data.frame(phrase=rep(phrase,times=nrow(freq_word)),
                         pred_word=freq_word$part1)
  katz_model<-cbind(katz_model,pkz=NA)

  ##this function asign probability value if the model have the whole phrase
  disc_pkz<-function(phrase,model,prev_model,katz_model){
    katz_selection<-filter(model,part1==phrase)
    for (i in 1:nrow(katz_model)){
      if (is.na(katz_model$pkz[i])){
        if ((katz_model$pred_word[i]%in%katz_selection$part2)){
          #&(katz_model$phrase[i]==phrase)
          katz_model_select<-katz_selection[which(katz_selection$part2 == katz_model$pred_word[i]),]
          katz_model$pkz[i]<-(katz_model_select$d*katz_model_select$count)/sum(katz_selection$count)
        }
      }
    }
    return(katz_model)
  }
  if (n==2){
    if (word1%in%freq_word$part1){
      katz_model<-disc_pkz(phrase,model,prev_model,katz_model)
      katz_selection<-pkatz(word1,prev_model,model,word1,2)
      for (i in 1:nrow(katz_model)){
        if (is.na(katz_model$pkz[i])){
          if (katz_model$pred_word[i]%in%katz_selection$part1){
            katz_model$pkz[i] <- katz_selection[which(katz_selection$part1 == katz_model$pred_word[i]),]$pkz
          }
        }
      }
    } else{
      for (i in 1:nrow(katz_model)){
        katz_model$pkz[i] <- freq_word[which(freq_word$part1 == katz_model$pred_word[i]),]$mle
      }
    }
  }
  if (n==3){
    if (phrase%in%three_gram$part1){
      katz_model<-disc_pkz(phrase,model,prev_model,katz_model)
      katz_selection<-pkatz(word2,two_gram,model,phrase,3)
      for (i in 1:nrow(katz_model)){
        if (is.na(katz_model$pkz[i])){
          if (katz_model$pred_word[i]%in%katz_selection$part2){
            katz_model$pkz[i]<-katz_selection[which(katz_selection$part2 == katz_model$pred_word[i]),]$pkz
          }
        }
      }
    }
    if (word2%in%two_gram$part1){
      katz_model<-disc_pkz(word2,two_gram,freq_word,katz_model)
      katz_selection<-pkatz(word2,freq_word,two_gram,word2,2)
      for (i in 1:nrow(katz_model)){
        if (is.na(katz_model$pkz[i])){
          if (katz_model$pred_word[i]%in%katz_selection$part1){
            katz_model$pkz[i] <- katz_selection[which(katz_selection$part1 == katz_model$pred_word[i]),]$pkz
          }
        }
      }
    }else{
      for (i in 1:nrow(katz_model)){
        katz_model$pkz[i] <- freq_word[which(freq_word$part1 == katz_model$pred_word[i]),]$mle
      }
    }
  }
  if(n==4){
    words<-paste(word2,word3,sep = " ")
    if (phrase%in%model$part1){
      katz_model<-disc_pkz(phrase,model,three_gram,katz_model)
      katz_selection<-pkatz(words,three_gram,model,phrase,4)
      for (i in 1:nrow(katz_model)){
        if (is.na(katz_model$pkz[i])){
          if (katz_model$pred_word[i]%in%katz_selection$part2){
            katz_model$pkz[i]<-katz_selection[which(katz_selection$part2 == katz_model$pred_word[i]),]$pkz
          }
        }
      }
    }
    if (words%in%three_gram$part1){
      katz_model<-disc_pkz(words,three_gram,two_gram,katz_model)
      katz_selection<-pkatz(word3,two_gram,three_gram,words,3)
      for (i in 1:nrow(katz_model)){
        if ((is.na(katz_model$pkz[i])) & (katz_model$pred_word[i]%in%katz_selection$part2)){
          katz_model$pkz[i]<-katz_selection[which(katz_selection$part2 == katz_model$pred_word[i]),]$pkz
        }
      }
    }
    if (word3%in%two_gram$part1){
      katz_model<-disc_pkz(word3,two_gram,freq_word,katz_model)
      katz_selection<-pkatz(word3,freq_word,two_gram,word3,2)
      for (i in 1:nrow(katz_model)){
        if (is.na(katz_model$pkz[i])){
          if (katz_model$pred_word[i]%in%katz_selection$part1){
            katz_model$pkz[i] <- katz_selection[which(katz_selection$part1 == katz_model$pred_word[i]),]$pkz
          }
        }
      }
    }
    else{
      for (i in 1:nrow(katz_model)){
        katz_model$pkz[i] <- freq_word[which(freq_word$part1 == katz_model$pred_word[i]),]$mle
      }
    }
  }
  if(n==5){
    words<-paste(word2,word3,word4,sep = " ")
    if (phrase%in%five_gram$part1){
      katz_model<-disc_pkz(phrase,model,four_gram,katz_model)
      katz_selection<-pkatz(words,four_gram,model,phrase,5)
      for (i in 1:nrow(katz_model)){
        if (is.na(katz_model$pkz[i])){
          if (katz_model$pred_word[i]%in%katz_selection$part2){
            katz_model$pkz[i]<-katz_selection[which(katz_selection$part2 == katz_model$pred_word[i]),]$pkz
          }
        }
      }
    }
    if (words%in%four_gram$part1){
      katz_model<-disc_pkz(words,four_gram,three_gram,katz_model)
      katz_selection<-pkatz(paste(word3,word4,sep = " "),three_gram,four_gram,words,4)
      for (i in 1:nrow(katz_model)){
        if ((is.na(katz_model$pkz[i])) & (katz_model$pred_word[i]%in%katz_selection$part2)){
          katz_model$pkz[i]<-katz_selection[which(katz_selection$part2 == katz_model$pred_word[i]),]$pkz
        }
      }
    }
    if (paste(word3,word4,sep = " ")%in%three_gram$part1){
      katz_model<-disc_pkz(paste(word3,word4,sep = " "),three_gram,two_gram,katz_model)
      katz_selection<-pkatz(word4,two_gram,three_gram,paste(word3,word4,sep = " "),3)
      for (i in 1:nrow(katz_model)){
        if ((is.na(katz_model$pkz[i])) & (katz_model$pred_word[i]%in%katz_selection$part2)){
          katz_model$pkz[i]<-katz_selection[which(katz_selection$part2 == katz_model$pred_word[i]),]$pkz
        }
      }
    }
    if (word4%in%two_gram$part1){
      katz_model<-disc_pkz(word4,two_gram,freq_word,katz_model)
      katz_selection<-pkatz(word4,freq_word,two_gram,word4,2)
      for (i in 1:nrow(katz_model)){
        if (is.na(katz_model$pkz[i])){
          if (katz_model$pred_word[i]%in%katz_selection$part1){
            katz_model$pkz[i] <- katz_selection[which(katz_selection$part1 == katz_model$pred_word[i]),]$pkz
          }
        }
      }
    }
    else{
      for (i in 1:nrow(katz_model)){
        katz_model$pkz[i] <- freq_word[which(freq_word$part1 == katz_model$pred_word[i]),]$mle
      }
    }
  }
  #final_t<-Sys.time()-initial_t
  #print(final_t)
  return(katz_model)
}


selection<-predict_word("on my",3,freq_word,five_gram=five_gram5,two_gram = two_gram2,three_gram = three_gram3,four_gram = four_gram4)
#head(selection)
#tail(selection[order(selection$pkz,decreasing = TRUE),],10)
head(selection[order(selection$pkz,decreasing = TRUE),],10)
inorder<-three_gram3[order(three_gram3$prgt,decreasing = TRUE),]
head(filter(inorder,(part1=="the")))
#head(five_gram,10)

##cleaning and separating val set
length(test)
val_set<-test[1:200]
length(val_set)

tokens_val<-tokens(val_set, what="word", remove_numbers=TRUE,
                   remove_punct=TRUE,
                   remove_symbols=TRUE)

tokens_val <- tokens_replace(tokens_val,
                             pattern = types(tokens_val),
                             replacement = stringi::stri_replace_all_fixed(types(tokens_val), "_", ""),
                             valuetype = "fixed")
tokens_val <- tokens_replace(tokens_val,
                             pattern = types(tokens_val),
                             replacement = stringi::stri_replace_all_fixed(types(tokens_val), "-", ""),
                             valuetype = "fixed")
tokens_val <- tokens_replace(tokens_val,
                             pattern = types(tokens_val),
                             replacement = stringi::stri_replace_all_fixed(types(tokens_val), " ", ""),
                             valuetype = "fixed")
#rm(english_words)

##counting ngrams
two_counts_val<-ngram_count(tokens_val,2)
three_counts_val<-ngram_count(tokens_val,3)
four_counts_val<-ngram_count(tokens_val,4)
five_counts_val<-ngram_count(tokens_val,5)

##computing probabilty for each set of ngrams

#phrase<-"ive revoked"
#n<-2
#katz_model<-val_two

pkatz<-function(words,prev_model,model,phrase,m){
  if (m==2){
    seen_bi<-filter(two_gram2,part1==words)$part2
    katz_selection<-filter(freq_word,!(part1%in%seen_bi))
    den <-sum(katz_selection$mle)
  }else{
    seen<-filter(model,part1==phrase)$part2
    katz_selection<-filter(filter(prev_model,part1==words),!(part2%in%seen))
    den <-sum(katz_selection$count*katz_selection$d)/sum(katz_selection$count)
  }
  if (dim(katz_selection)[1]!=0){
    filter_num<-filter(model,part1==phrase)
    num<-1-(sum(filter_num$count*filter_num$d)/sum(filter_num$count))
    alpha <- num/den
    if(m==2){
      for (i in 1:nrow(katz_selection)){
        katz_selection$pkz[i] <- alpha*katz_selection$mle[i]
      }
    }else{
      for (i in 1:nrow(katz_selection)){
        katz_selection$pkz[i] <- alpha*((katz_selection$count[i]*katz_selection$d[i])/sum(katz_selection$count))
      }
    }
  }
  return(katz_selection)
}

find_p<-function(phrase,n,freq_word,two_gram,three_gram,four_gram,five_gram){
  #initial_t<-Sys.time()

  if (n==2){model<-two_gram; prev_model<-freq_word}
  if (n==3){model<-three_gram; prev_model<-two_gram}
  if (n==4){model<-four_gram; prev_model<-three_gram}
  if (n==5){model<-five_gram; prev_model<-four_gram}

  ##split phrase
    split_phrase<-strsplit(phrase," ")
  word1 <-split_phrase[[1]][1]
  word2 <-split_phrase[[1]][2]
  if (n>=3){
    word3 <-split_phrase[[1]][3]
  }
  if (n>=4){
    word4 <-split_phrase[[1]][4]
  }
  if (n==5){
    word5 <-split_phrase[[1]][5]
  }

  find_pbo<-function(phrase,model,words){
    katz_selection<-model[which(paste(model$part1,model$part2,sep=" ")==phrase),]
    prev_selection<-model[which(model$part1==words),]
    pkz<-(katz_selection$d*katz_selection$count)/sum(prev_selection$count)
    return(pkz)
  }

  if (n==2){
    if (phrase %in% paste(two_gram$part1,two_gram$part2,sep = " ")){
      pkz<-find_pbo(phrase,model,word1)
    }else  if ((word2 %in% prev_model$part1)&(word1 %in% freq_word$part1)&(if(word1 %in% model$part1){mean(filter(model,part1==word1)$d)!=1}else{FALSE})){
      katz_selection<-pkatz(word1,freq_word,two_gram,word1,2)
      pkz <- katz_selection[which(katz_selection$part1 == word2),]$pkz
    }else if (word2 %in% prev_model$part1){
      pkz <- freq_word[which(freq_word$part1 == word2),]$mle
    }else{
      pkz <- freq_word[which(freq_word$part1 == "unk"),]$mle
    }
  }

  if (n==3){
    if (phrase %in% paste(three_gram$part1, three_gram$part2,sep=" ")){
      pkz<-find_pbo(phrase,model,paste(word1,word2,sep = " "))
    }else  if ((paste(word2,word3,sep=" ") %in% paste(two_gram$part1,two_gram$part2,sep=" ")) &
               (if(paste(word1,word2,sep=" ") %in% model$part1){mean(filter(model,part1==paste(word1,word2,sep=" "))$d)!=1}else{FALSE})){
      katz_selection<-pkatz(word2,prev_model,model,paste(word1,word2,sep=" "),3)
      pkz <- katz_selection[which(katz_selection$part2 == word3),]$pkz
    }else if (paste(word2,word3,sep=" ") %in% paste(two_gram$part1,two_gram$part2,sep=" ")){
      pkz<-find_pbo(paste(word2, word3, sep=" "),two_gram,word2)
    }else if ((word3 %in% freq_word$part1) & (word2 %in% freq_word$part1)&
              (if(word2 %in% two_gram$part1){mean(filter(two_gram,part1==word2)$d)!=1}else{FALSE})){
      katz_selection<-pkatz(word2,freq_word,two_gram,word2,2)
      pkz <- katz_selection[which(katz_selection$part1 == word3),]$pkz
    }else if(word3 %in% freq_word$part1){
      pkz <- freq_word[which(freq_word$part1 == word3),]$mle
    }else{
      pkz <- freq_word[which(freq_word$part1 == "unk"),]$mle
    }
  }

  if(n==4){
    if (phrase %in% paste(four_gram$part1, four_gram$part2,sep=" ")){
      pkz<-find_pbo(phrase,model,paste(word1,word2,word3,sep = " "))
    }else  if ((paste(word2, word3,word4, sep=" ") %in% paste(three_gram$part1,three_gram$part2,sep=" ")) &
               (if(paste(word1,word2,word3,sep=" ")%in%model$part1){mean(filter(model,part1==paste(word1,word2,word3,sep=" "))$d)!=1}else{FALSE})){
      katz_selection<-pkatz(paste(word2,word3,sep=" "),prev_model,model,paste(word1,word2,word3,sep=" "),4)
      pkz <- katz_selection[which(katz_selection$part2 == word4),]$pkz
    }else if (paste(word2, word3,word4, sep=" ") %in% paste(three_gram$part1,three_gram$part2,sep=" ")){
      pkz<-find_pbo(paste(word2, word3,word4, sep=" "),three_gram,paste(word2,word3,sep=" "))
    }else if ((paste(word3,word4, sep=" ") %in% paste(two_gram$part1,two_gram$part2,sep=" ")) &
              (if(paste(word2,word3,sep=" ")%in%three_gram$part1){mean(filter(three_gram,part1==paste(word2,word3,sep=" "))$d)!=1}else{FALSE})){
      katz_selection<-pkatz(word3,two_gram,three_gram,paste(word2,word3,sep=" "),3)
      pkz <- katz_selection[which(katz_selection$part2 == word4),]$pkz
    }else if (paste(word3,word4, sep=" ") %in% paste(two_gram$part1,two_gram$part2,sep=" ")){
      pkz<-find_pbo(paste(word3,word4, sep=" "),two_gram,word3)
    }else if ((word4 %in% freq_word$part1) & (word3 %in% freq_word$part1)&
              (if(word3%in%two_gram$part1){mean(filter(two_gram,part1==word3)$d)!=1}else{FALSE})){
      katz_selection<-pkatz(word3,freq_word,two_gram,word3,2)
      pkz <- katz_selection[which(katz_selection$part1 == word4),]$pkz
    }else if(word4 %in% freq_word$part1){
      pkz <- freq_word[which(freq_word$part1 == word4),]$mle
    }else{
      pkz <- freq_word[which(freq_word$part1 == "unk"),]$mle
    }
  }

  if(n==5){
    if (phrase %in% paste(five_gram$part1, five_gram$part2,sep=" ")){
      pkz<-find_pbo(phrase,model,paste(word1,word2,word3,word4,sep = " "))
    }else  if ((paste(word2, word3,word4,word5,sep=" ") %in% paste(four_gram$part1,four_gram$part2,sep=" ")) &
               (if(paste(word1,word2,word3,word4,sep=" ")%in%model$part1){mean(filter(model,part1==paste(word1,word2,word3,word4,sep=" "))$d)!=1}else{FALSE})){
      katz_selection<-pkatz(paste(word2,word3,word4,sep=" "),prev_model,model,paste(word1,word2,word3,word4,sep=" "),5)
      pkz <- katz_selection[which(katz_selection$part2 == word5),]$pkz
    }else if (paste(word2, word3,word4,word5,sep=" ") %in% paste(four_gram$part1,four_gram$part2,sep=" ")){
      pkz<-find_pbo(paste(word2, word3,word4,word5, sep=" "),four_gram,paste(word2,word3,word4,sep=" "))
    }else if ((paste(word3,word4,word5, sep=" ") %in% paste(three_gram$part1,three_gram$part2,sep=" ")) &
              (if(paste(word2,word3,word4,sep=" ")%in%four_gram$part1){mean(filter(four_gram,part1==paste(word2,word3,word4,sep=" "))$d)!=1}else{FALSE})){
      katz_selection<-pkatz(paste(word3,word4,sep=" "),three_gram,four_gram,paste(word2,word3,word4,sep=" "),4)
      pkz <- katz_selection[which(katz_selection$part2 == word5),]$pkz
    }else if (paste(word3,word4,word5, sep=" ") %in% paste(three_gram$part1,three_gram$part2,sep=" ")){
      pkz<-find_pbo(paste(word3, word4,word5, sep=" "),three_gram,paste(word3,word4,sep=" "))
    }else if ((paste(word4,word5, sep=" ") %in% paste(two_gram$part1,two_gram$part2,sep=" ")) &
              (if(paste(word3,word4,sep=" ")%in%three_gram$part1){mean(filter(three_gram,part1==paste(word3,word4,sep=" "))$d)!=1}else{FALSE})){
      katz_selection<-pkatz(word4,two_gram,three_gram,paste(word3,word4,sep=" "),3)
      pkz <- katz_selection[which(katz_selection$part2 == word5),]$pkz
    }else if (paste(word4,word5, sep=" ") %in% paste(two_gram$part1,two_gram$part2,sep=" ")){
      pkz<-find_pbo(paste(word4,word5, sep=" "),two_gram,word4)
    }else if ((word5 %in% freq_word$part1) & (word4 %in% freq_word$part1)&
              (if(word4%in%two_gram$part1){mean(filter(two_gram,part1==word4)$d)!=1}else{FALSE})){
      katz_selection<-pkatz(word4,freq_word,two_gram,word4,2)
      pkz <- katz_selection[which(katz_selection$part1 == word5),]$pkz
    }else if(word5 %in% freq_word$part1){
      pkz <- freq_word[which(freq_word$part1 == word5),]$mle
    }else{
      pkz <- freq_word[which(freq_word$part1 == "unk"),]$mle
    }
    }
  final_t<-Sys.time()-initial_t
  #print(final_t)
  return(pkz)
}

val_two<-data.frame(phrase=two_counts_val$part1,pred_word=two_counts_val$part2)
val_two<-cbind(val_two,pkz=NA)

val_three<-data.frame(phrase=three_counts_val$part1,pred_word=three_counts_val$part2)
val_three<-cbind(val_three,pkz=NA)

val_four<-data.frame(phrase=four_counts_val$part1,pred_word=four_counts_val$part2)
val_four<-cbind(val_four,pkz=NA)

val_five<-data.frame(phrase=five_counts_val$part1,pred_word=five_counts_val$part2)
val_five<-cbind(val_five,pkz=NA)

#pkz of valoration set for 2 gram
initial_t<-Sys.time()
for (i in 1:nrow(val_two)){
  val_two$pkz[i]<-find_p(paste(val_two$phrase[i],val_two$pred_word[i],sep = " "),n=2,freq_word = freq_word,two_gram=two_gram2)
}
final_t<-Sys.time()-initial_t
save(val_two,file="twoval.Rda")

#pkz of valoration set for 3 gram
initial_t<-Sys.time()
for (i in 1:nrow(val_three)){
  val_three$pkz[i]<-find_p(paste(val_three$phrase[i],val_three$pred_word[i],sep = " "),n=3,freq_word = freq_word,two_gram=two_gram2,three_gram = three_gram3)
}
final_t<-Sys.time()-initial_t
save(val_three,file="threeval.Rda")

#pkz of valoration set for 4 gram
initial_t<-Sys.time()
for (i in 1:nrow(val_four)){
  val_four$pkz[i]<-find_p(paste(val_four$phrase[i],val_four$pred_word[i],sep = " "),n=4,freq_word = freq_word, two_gram=two_gram2,three_gram = three_gram3, four_gram = four_gram4)
}
final_t<-Sys.time()-initial_t
save(val_four,file="fourval.Rda")

#pkz of valoration set for 5 gram
initial_t<-Sys.time()
for (i in 1:nrow(val_five)){
  val_five$pkz[i]<-find_p(paste(val_five$phrase[i],val_five$pred_word[i],sep = " "),n=5,freq_word = freq_word, two_gram=two_gram2,three_gram = three_gram3, four_gram = four_gram4,five_gram = five_gram5)
}
final_t<-Sys.time()-initial_t
save(val_five,file="fiveval.Rda")

load("twoval.Rda")
load("threeval.Rda")
load("fourval.Rda")
load("fiveval.Rda")

###finding perplexity
ppx_twogram<-exp(-sum(log(val_two$pkz)) / length(val_two$pkz))
ppx_threegram<-exp(-sum(log(val_three$pkz)) / length(val_three$pkz))
ppx_fourgram<-exp(-sum(log(val_four$pkz)) / length(val_four$pkz))
ppx_fivegram<-exp(-sum(log(val_five$pkz)) / length(val_five$pkz))

##accuracy
pred_two<-subset(val_two,select=-pred_word)
pred_two$pred_word_1<-NA
pred_two$pred_word_2<-NA
pred_two$pred_word_3<-NA
pred_two$test_word<-val_two$pred_word
initial_t<-Sys.time()
for (i in 1:300){
  selection<-predict_word(pred_two$phrase[i],2,freq_word,two_gram = two_gram2)
  pred_two$pred_word_1[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[1]
  pred_two$pred_word_2[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[2]
  pred_two$pred_word_3[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[3]
}
final_t_2<-Sys.time()-initial_t


pred_three_s<-subset(val_three,select=-pred_word)
pred_three_s$pred_word_1<-NA
pred_three_s$pred_word_2<-NA
pred_three_s$pred_word_3<-NA
pred_three_S$test_word<-val_three$pred_word
initial_t<-Sys.time()
for (i in 1:300){
  selection<-predict_word(pred_three_s$phrase[i],3,freq_word_s,two_gram = two_gram_s,three_gram = three_gram_s)
  pred_three_s$pred_word_1[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[1]
  pred_three_s$pred_word_2[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[2]
  pred_three_s$pred_word_3[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[3]
}
final_t_3<-Sys.time()-initial_t

pred_four<-subset(val_four,select=-pred_word)
pred_four$pred_word_1<-NA
pred_four$pred_word_2<-NA
pred_four$pred_word_3<-NA
pred_four$test_word<-val_four$pred_word
initial_t<-Sys.time()
for (i in 1:300){
  selection<-predict_word(pred_four$phrase[i],4,freq_word,two_gram = two_gram2,three_gram = three_gram3,four_gram = four_gram4)
  pred_four$pred_word_1[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[1]
  pred_four$pred_word_2[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[2]
  pred_four$pred_word_3[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[3]
}
final_t_4<-Sys.time()-initial_t

pred_five<-subset(val_five,select=-pred_word)
pred_five$pred_word_1<-NA
pred_five$pred_word_2<-NA
pred_five$pred_word_3<-NA
pred_five$test_word<-val_five$pred_word
initial_t<-Sys.time()
for (i in 1:300){
  selection<-predict_word(pred_five$phrase[i],5,freq_word,two_gram = two_gram2,three_gram = three_gram3,four_gram = four_gram4,five_gram=five_gram5)
  pred_five$pred_word_1[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[1]
  pred_five$pred_word_2[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[2]
  pred_five$pred_word_3[i]<-selection[order(selection$pkz,decreasing = TRUE),]$pred_word[3]
}
final_t_5<-Sys.time()-initial_t

save(pred_two,file="two_pred.Rda")
save(pred_three,file="three_pred.Rda")
save(pred_four,file="four_pred.Rda")
save(pred_five,file="five_pred.Rda")

#checking accuracy
trues_two<-as.data.frame(rbind(prop.table(table(pred_two$pred_word_1[1:300]==pred_two$test_word[1:300])),
                               prop.table(table(pred_two$pred_word_2[1:300]==pred_two$test_word[1:300])),
                               prop.table(table(pred_two$pred_word_3[1:300]==pred_two$test_word[1:300]))))

trues_three<-as.data.frame(rbind(prop.table(table(pred_three_s$pred_word_1[1:300]==pred_three$test_word[1:300])),
                                 prop.table(table(pred_three_s$pred_word_2[1:300]==pred_three$test_word[1:300])),
                                 prop.table(table(pred_three_s$pred_word_3[1:300]==pred_three$test_word[1:300]))))

trues_four<-as.data.frame(rbind(prop.table(table(pred_four$pred_word_1[1:300]==pred_four$test_word[1:300])),
                                prop.table(table(pred_four$pred_word_2[1:300]==pred_four$test_word[1:300])),
                                prop.table(table(pred_four$pred_word_3[1:300]==pred_four$test_word[1:300]))))

trues_five<-as.data.frame(rbind(prop.table(table(pred_five$pred_word_1[1:300]==pred_five$test_word[1:300])),
                                prop.table(table(pred_five$pred_word_2[1:300]==pred_five$test_word[1:300])),
                                prop.table(table(pred_five$pred_word_3[1:300]==pred_five$test_word[1:300]))))
acc_two<-sum(trues_two$`TRUE`)
acc_three<-sum(trues_three$`TRUE`)
acc_four<-sum(trues_four$`TRUE`)
acc_five<-sum(trues_five$`TRUE`)

##reducing time
tail(two_gram2[order(two_gram2$count),],10)
tail(three_gram3[order(three_gram3$count),],10)

selection<-predict_word("mean the",3,freq_word_s,two_gram = two_gram_s,three_gram = three_gram_s)
head(selection[order(selection$pkz,decreasing = TRUE),],10)

two_gram_s<-filter(two_gram2,count>60)
three_gram_s<-filter(three_gram3,count>18)
freq_word_s<-filter(freq_word,freq>220)

save(two_gram_s,file="two_gram_s.Rda")
save(three_gram_s,file="three_gram_s.Rda")
save(freq_word_s,file="freq_word_s.Rda")

###save prediction functions
save(predict_word,file="pred_word.Rda")
save(find_p,file="find_p.Rda")
save(pkatz,file="pkatz.Rda")

###making the model faster
dim(two_gram)
dim(three_gram)
table(two_gram$count)
