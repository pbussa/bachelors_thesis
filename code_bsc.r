rm(list=ls())
gc()
#IMPORT LIBRARIE E DATASET
libraries=c("tidytext", "tidyverse","TextWiller", "tm", "tau", "wordcloud")
lapply(libraries, require, character.only = TRUE)
testonuovo %>% filter(str_detect("del", text))
?normalizzahtml
load("smpl_tw_cv.Rdata") 
feb_mar=as_tibble(sampl_tw_cv)
print(feb_mar, width=Inf)
tweets=read.csv("tweet_final.csv",  sep=",")
tweets=as_tibble(tweet_final)
colnames(tweets)
dim(tweets)
head(tweets) = testo %>% filter(created_hour != "00:00:00")

sum(is.na(tweets))
sort(unique(strftime(tweets$created_at, format="%Y-%m-%d"))) #tweet dal 14-02 all'11-05
testo=as_tibble(testoun)
table(ora$created_day)

testoun=as_tibble(testoun)
#SELEZIONE VARIABILI 
testo=as_tibble(uniq)
testo=as_tibble(tweet_mar_mag)
testo1=nuovo %>% rename(screen_name=user_screen_name.1,OP=reweet_id) %>% filter (is.na(OP)) %>% distinct()

#TOP 5 LUOGHI (solo febbraio-marzo)
testo %>% 
  filter(!is.na(user_location)) %>% 
  count(user_location, sort = TRUE) %>% 
  top_n(5)
testo %>% left_join(lingue)
#TOP RETWEET
testo %>% 
  arrange(-retweet_count) %>%
  select(X, screen_name, text, retweet_count) %>%
  top_n(10) 
#presenza di tweet in inglese
as_vector(testo[47900,])
library(textcat)
testonuovo$lan=textcat(testonuovo$text,ECIMCI_profiles)
table(testonuovo$lan)
teston1=testonuovo %>%filter(lan %in% c("it", "la")) 
dim(teston1)
write.csv(nuovo,"mergenuovo.csv", row.names=FALSE)
table(textcat(testo$text,ECIMCI_profiles))
textcat(testo$text[1])
#28083 italian
#429 latin
#328 english
#240 spanish
testo1%>%group_by(id)
unique(testo1$created_at)
#TOP MENTIONS
testonuovo %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)
#TOP HASHTAG
testo %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = TRUE) %>%
  filter(str_detect(hashtag, "^#"),
         !hashtag %in% c("#coronavirus","#covid", "#covid19", "#coronarvirusitalia","#coronavirusitalla", "#coronvirusitalia",
                         "#coronavirusitaly", "#coronavirusitalia", "#covid2019", "#covid19italia", "#covid???19", "#coviditalia"
                         
         )) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(20)

normalizzaTesti(c("Coronavirus: â???oDona la potenza del tuo computer e troveremo la curaâ???\u009d [di JAIME D'ALESSANDRO] [aggiornamento delle 23:12] https://t.co/yXwpoBRw28", "ciao"),normalizzacaratteri=TRUE,tolower=TRUE,perl=FALSE,fixed=FALSE)
normalizzaTesti(c("ciaoooooooo", "uno dueee treeee"), normalizzacaratteri=TRUE,tolower=TRUE,perl=FALSE,fixed=FALSE)
#PREPROCESSING
#install_github("livioivil/TextWiller@TextWiller_JOSS")
testonuovo=as_tibble(uniq)
testonuovo$text=normalizzaTesti(as_vector(testonuovo$text),normalizzacaratteri=TRUE,tolower=TRUE,perl=FALSE,fixed=FALSE)
head(testonuovo$text)
testonuovo$text=gsub('[0-9]+', '', testonuovo$text)
testonuovo$text=gsub("\\d", '', testonuovo$text)
testonuovo$text=gsub('[[:cntrl:]]', '', testonuovo$text)
testonuovo$text=gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", testonuovo$text)
citation("libiconv")

testo$text=gsub("piã","più",testo$text)
testo$text=gsub("cosã","così",testo$text)
testo$text=gsub("giã","già",testo$text)
testo$text=gsub("perchã","perché",testo$text)

testonuovo$text=gsub('#\\S+', '',testonuovo$text)
testonuovo$text=gsub('@\\S+', '', testonuovo$text)
testonuovo$text=gsub("'", " ", testonuovo$text)


jaime=testo %>% filter(str_detect(text,"di JAIME D'ALESSANDRO"))
normalizzaTesti(jaime$text)
head(testonuovo$text)
testonuovo$text=normalizzaTesti(testonuovo$text, remove=c("coronavirus","covid","fef", "ffb","ddc","wwwurlwww", "#coronavirus", "#covid",#covid19", "#covid-19",
                                                          "dfc","ffa","fda", "fbc", "def",
                                                          "#coronavirusitaly", "#coronavirusitalia", "#covid2019", "#covid19italia", "#coronaviriusitalia",
                                                "#coronaviruslombardia", "covid19", "rt", "coronavirusitalla", "coranavirusitalia", "coronarvirusitalia"
))
head(testo)

Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}

dim(testo1)
testonuovo$text=Textprocessing(testonuovo$text)
?iconv
testonuovo$text=gsub("'", " ", testonuovo$text)
testonuovo$text=iconv(testonuovo$text, "latin1", "ASCII", sub="")
testonuovo$text=gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", testonuovo$text)
testonuovo = testonuovo %>% filter(text != "")

testo=testo%>% distinct(text, .keep_all = TRUE)
testonuovo=testoun %>% filter(isRetweet==F)
testo1$text
write.csv(testoun, "testoun.csv", row.names = FALSE)
write.csv(testo1, "testo19feb23apr.csv", row.names=FALSE)

#install.packages("rjson")
library(rjson)
jsonData <- toJSON(testo)
write(jsonData, "output.json") 

#DISTRIBUZIONE TEMPORALE 
tweets$created_day=strftime(tweets$created_at, format="%Y-%m-%d")
tweets$created_hour=strftime(tweets$created_at, format="%H:%M:%S")
chr=testonuovo%>%select(created_at)
created_day=vector()
created_hour=vector()
for (i in 1:dim(chr)[1]){ 
  data=as.character(chr[i,])
  s1 = unlist(strsplit(data, split=' ', fixed=TRUE))
  if (s1[2] == "Feb"){
    
    month="02"
  }
  if (s1[2] == "Mar"){
    month="03"
  }
  if (s1[2] == "Apr"){
    month="04"
  }
  
  
  created_day[i]=paste( s1[6], month,s1[3], sep="-")
  created_hour[i]=s1[4]
  
}
testonuovo %>% select(created_at, created_day, created_hour)
testonuovo$created_hour=created_hour
testonuovo$created_day=created_day

testoun=as_tibble(testoun)
testoun %>% filter(str_detect(text,"contagi "))
testoun %>% filter(str_detect(text,"terapie intensive"))

444+1761 #mascherine
2843+1177+305+183  #positivi
1163+504 #tamponi
1600+815 #ospedali
573+1025 #letti
1176+632 #medici
509+117 #infermieri
108+38 #ventilatori
341+509 #aiuto
86 #tosse
197 #febbre

5957 #contagi


testohr=testoun %>% filter(created_hour != "00:00:00")
testohr %>% group_by(created_hour) %>% mutate(count=n()) %>% ggplot(aes(x=as.Date(created_hour), y=count))+geom_point()

strptime(testohr$created_hour, format="%h %m")

testohr %>% filter(str_detect(created_at, "22:" ))
testohr %>%
  mutate(created_at = as.POSIXct(created_at)) %>%
  group_by(lubridate::hour(created_at)) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

interno=testohr %>% filter(screen_name=="zazoomblog")

interno=interno %>%
  mutate(created_at = as.POSIXct(created_at)) %>%
  group_by(lubridate::hour(created_at)) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

ggplot(interno, aes(x=as.Date(created_at,format="%h %m") , y=))

ggplot(interno, aes(x= as.POSIXct(created_at))) + geom_area(stat = "count", binwidth=1, drop=TRUE)



testo2$cr=created_day
strptime(as.character(chr), format="%b %d, %Y %I:%M %p")
ggplot(testo,aes(created_day))+
  geom_histogram(stat="count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(testo$created_day) #normalizzazione tweet per giorno
#PAROLE PIù FREQUENTI

text_df3=testo %>% unnest_tokens(word,textnuovo)

text_df=testoun %>% unnest_tokens(word,text)
text_df=as_tibble(corpus_tokens) %>% unnest_tokens(word,text)
head(text_df)
wordcloudDF=text_df %>% count(word,sort=T)
sentiment("casa")
text_df %>%
  filter(created_day >= "2020-04-10" & created_day <= "2020-04-11") %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
 
#WORDCLOUD
require(wordcloud2)
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
letterCloud(wordcloudDF,"R")

figPath <- "t.png"
wordcloud2(wordcloudDF, figPath = figPath,color = "skyblue")
wordcloud2(wordcloudDF,color = "skyblue")
figPath = system.file("t.png",package = "wordcloud2")
wordcloud2(wordcloudDF, color = "skyblue")


corpus=Corpus(VectorSource(testoun$text))
dtm=DocumentTermMatrix(corpus,control = list( stemming = FALSE,stopwords=itastopwords, 
                                              minWordLength = 3,removeNumbers = FALSE,
                                              removePunctuation = TRUE,bounds=list(local = c(1,Inf)) ))

97467*62098
inspect(dtm[160:165,])
wordcloud(words=colnames(dtm),freq=colSums(as.matrix(dtm)),min.freq=700,color="black")

testoun[150:155,] %>% select(text)

library(quanteda)

testo=as_tibble(testo)
prova=testo%>%filter(created_day %in% c("2020-03-07","2020-03-08","2020-03-09"))
testo=prova %>% filter(created_hour != "00:00:00")
testo$created=paste(testo$created_day, testo$created_hour)

sotu_corpus <- corpus(testo$text)
lemma_data <-lemmatization.it

corpus_tokens <- sotu_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() #%>%
  tokens_replace(lemma_data$V2, lemma_data$V1, valuetype = "fixed") 

text_df
tok1=tokens(testo$text)
tok2=tokens_replace(tok1, lemma_data$V2,lemma_data$V1, valuetype = "fixed") 

    
sotu_collocations <- textstat_collocations(corpus_tokens, min_count = 10)
sotu_collocations <- sotu_collocations[1:300, ]
?tokens_compound
corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)

DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.6, docfreq_type = "prop")

# have a look at the number of documents and terms in the matrix
dim(DTM)
top10_terms <- c( "non", "italia", "emotelove", "emergenza", "ora", "solo", "ecco", "cosa", "perch", "cos", "dopo", "prima")

DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- testo[sel_idx, ]
library(topicmodels)
topicModel <- LDA(DTM, k=7, method="Gibbs", control=list(iter = 500, verbose = 25))
tmResult <- posterior(topicModel)
attributes(tmResult)
ncol(DTM)    
beta <- tmResult$terms 
dim(beta)
rowSums(beta)  
theta <- tmResult$topics 
dim(theta)
terms(topicModel, 10)
M2
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

topicNames2 <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")


exampleIds <- c(2, 100, 200)
cat(sotu_corpus[1:10534])
cat(sotu_corpus[exampleIds[2]])
cat(sotu_corpus[exampleIds[3]])

topicProportionExamples <- theta[1:10534,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

feb=testonuovo %>% filter(as.Date(created_day) > "2020-02-13" & as.Date(created_day) < "2020-03-16")
mar=testonuovo %>% filter(as.Date(created_day) > "2020-03-15" & as.Date(created_day) < "2020-04-14")
apr=testonuovo %>% filter(as.Date(created_day) > "2020-04-13" & as.Date(created_day) < "2020-05-13")

testo2=testo1 %>% inner_join(feb) %>% mutate(month= "feb_mar")
testo3=testo1 %>% inner_join(mar) %>% mutate(month= "mar_apr")
testo4=testo1 %>% inner_join(apr) %>% mutate(month= "apr_mag")




textdata=textdata %>% inner_join(bind_rows(testo2,testo3,testo4))
# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(month = textdata$month), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "month")

# plot topic proportions per deacde as bar plot
require(pals)
ggplot(vizDataFrame, aes(x=month, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "month") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#DA QUI
library(chron)
df=data.frame(id=names(topics(topicModel)),
    date=as.Date(textdata$created_day))
dft <- cbind(df,posterior(topicModel)$topics)

M <- gather(dft,topic,value,-id,-date) %>%
  group_by(topic,date) %>%
  summarize(value=mean(value))

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse="-")

nomi=as_tibble(topicNames) %>% transmute(topic=row_number(), nome=value)

nomi=nomi%>%mutate(topic = as.character(topic))
sample_n(M, 15)
M=M%>%left_join(nomi)
M2=M%>%filter(topic==4)

write.csv(M, "M.csv", row.names = F)

sp=ggplot(M1,aes(x=date,y=value,color=nome, alpha = nome, size = nome)) + 
  geom_line() +
  scale_color_manual(values=c("black"))
  
sp + geom_vline(xintercept = as.Date("2020-03-08"), linetype="dashed") +
  geom_vline(xintercept = as.Date("2020-03-22"), linetype="dashed") +
   geom_vline(xintercept = as.Date("2020-04-10"), linetype="dashed") +
 geom_vline(xintercept = as.Date("2020-04-26"), linetype="dashed") +
geom_vline(xintercept = as.Date("2020-05-04"), linetype="dashed")

spp + geom_vline(xintercept = as.Date("2020-03-22"), linetype="dashed")


M2=M%>%filter(topic==2)
ggplot(M2,aes(x=date,y=value,color=nome, alpha = nome, size = nome)) + 
  geom_line() +
  scale_color_manual(values=c("red"))
library(scales)
lims <- as.POSIXct(strptime(c("2020-03-21 00:00","2020-03-22 23:59"), format = "%Y-%m-%d %H:%M"))    
filter(M, topic==)
M %>%
  mutate(date = as.POSIXct(date)) %>%
  ggplot(aes(x = date, y = value,color=nome, alpha = nome, size = nome)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "4 hours", 
                   date_minor_breaks = "1 hour", labels = date_format("%H:%M:%S"))
                   
theme(axis.text.x=element_text(angle=90))

#USER PIù ATTIVI
temp=testoun %>% group_by(screen_name) %>% count(sort=T) %>% top_n(20)
temp[10:20,]
testo %>%
  count(screen_name, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(tot = reorder(screen_name, n)) %>%
  ggplot(aes(tot, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



#ANALISI N-GRAMMI
library(tau)
bigrams=textcnt(testo$text,method="string",n=2L,split="[[:blank:]]")
sort(bigrams,decreasing=TRUE)[1:30]
trigrams=textcnt(testo$text,method="string",n=3L,split="[[:blank:]]")
sort(trigrams,decreasing=TRUE)[1:30]

#CONTE VS SALVINI

tdconte=testo %>%
  mutate(text2=text) %>%
  unnest_tokens(tok, text2, "tweets", to_lower = FALSE) %>%
  filter(tok %in% c("@conte", "#conte", "conte")) %>% 
  select(text) %>%
  unnest_tokens(word, text)
tdsalvini=testo %>%
  mutate(text2=text) %>%
  unnest_tokens(tok, text2, "tweets", to_lower = FALSE) %>%
  filter(tok %in% c("matteosalvinimi", "salvini")) %>% 
  select(text) %>%
  unnest_tokens(word, text)

td=testo %>%
  unnest_tokens(word, text)

frequency <- bind_rows(mutate(tdconte, author = "Conte"),
                       mutate(tdsalvini, author = "Salvini"), 
                       mutate(td, author = "Word")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Conte`:`Salvini`)

library(scales)
ggplot(frequency, aes(x = proportion, y = `Word`, color = abs(`Word` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Word", x = NULL)



#SENTIMENT ANALYSIS
sub=select(text_df,word) %>% sample_n(10000)
sent=sentiment(as.vector(sub$word))
pos=subset(sent,sent==1)
temp=bind_rows(pos)
pos=gather(temp, key="word")

neg=subset(sent,sent==-1)
temp=bind_rows(neg)
neg=gather(temp, key="word")
(pos)
(neg)
pos_df=text_df %>%
  inner_join(pos) %>%
  count(word, sort = TRUE)%>%
  mutate(sentiment="positive") 
neg_df=text_df %>%
  inner_join(neg) %>%
  count(word, sort = TRUE)%>%
  mutate(sentiment="negative") 
word_counts=full_join(pos_df, neg_df) %>% arrange(desc(n))



text_df %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds


word_counts %>%
  group_by(sentiment) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

word_counts %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red3", "green2"),
                   max.words = 500)

testo_df=tbl_df(testo)
date=unique(testo$created_day)

orig=testo %>% filter(is_retweet == F)
vec=data.frame(sentiment=integer(), date=as.Date(character()),stringsAsFactors=FALSE) 

for (i in 1:length(date)){
  temp = testo %>% filter(as.Date(created_day) == date[i])
  tokens <- data_frame(temp) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(word_counts) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) 
 
  df=data.frame(sentiment$sentiment,as.Date(temp$created_day[1]))
  colnames(df) = c("sentiment", "date")
  vec=rbind(vec, df)
  
}



  temp = testo %>% filter(as.Date(created_day) == "2020-03-03")
  tokens <- data_frame(testo) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
  
  
  bigrams_separated <- tokens %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  negation_words <- c("mai", "no", "senza", "non")
  
  negated_words <- bigrams_separated %>%
    filter(word1 %in% negation_words) %>%
    inner_join(word_counts, by = c(word2 = "word")) %>%
    count(word1, word2, sentiment, sort = TRUE)
  
  negated_words %>%
    mutate(contribution = n*ifelse(sentiment == "positive", 1 ,-1 )) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n* ifelse(sentiment == "positive", 1 ,-1 ) , fill = n * ifelse(sentiment == "positive", 1 ,-1 ))) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by \"senza\"") +
    ylab(" number of occurrences") +
    coord_flip()
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(word_counts) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) 
  
  df=data.frame(sentiment$sentiment,as.Date(temp$created_day[1]))
  colnames(df) = c("sentiment", "date")
  vec=rbind(vec, df)
  


ggplot(vec, aes(x = date, y = sentiment)) +  # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model


head(testo$text)

ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model


#LDA
corpus=normalizzaTesti(unique(testo$text),normalizzacaratteri=TRUE,tolower=TRUE,perl=FALSE,fixed=FALSE)
corpus1=lexicalize(corpus)
to.keep.voc=corpus1$vocab[word.counts(corpus1$documents,corpus1$vocab) >= 3]
to.keep.stop=subset(to.keep.voc,is.na(pmatch(to.keep.voc,itastopwords)))
corpus=lexicalize(corpus,vocab=to.keep.stop)
n = nrow(corpus)
k  = 10
top = 10
I = 50
result = lda.collapsed.gibbs.sampler(corpus, k, to.keep.stop,I,  0.1, 0.1, compute.log.likelihood=TRUE)
plot(c(1:I),result$log.likelihoods[1,],type="l",ylim=c(min(min(result$log.likelihoods[1,]),
                                                           min(result$log.likelihoods[2,])),max(max(result$log.likelihoods[1,]),
                                                                                                max(result$log.likelihoods[2,]))),col="blue",xlab="Iterations",ylab="LogLikelihood")
lines(result$log.likelihoods[2,],col="red")
top.words=top.topic.words(result$topics, top, by.score=TRUE)
top.docs=top.topic.documents(result$document_sums,num.documents=7, alpha = 0.1)
topic.proportions=t(result$document_sums)/colSums(result$document_sums)
(top.words)  #topic individuati: sanità, calcio, scuola, politica, primi contagi

library(syuzhet)

head(testo$text[99])

get_sentiment(testo$text[1:100], language="it")



#stemming
library(corpus)
text_tokens(testo1$text, stemmer = "it")
