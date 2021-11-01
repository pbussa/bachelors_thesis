#CHAPTER 1

libraries=c("streamR", "ROAuth", "twitteR", "tidyverse", "TextWiller", "tidytext", "textcat", "tm", "tau", "wordcloud2", "quanteda", "igraph", "ggraph", "hrbrthemes", "scales", "topicmodels", "tidyquant")
lapply(libraries, require,character.only = TRUE)

#API connection
consumerKey='****'
consumerSecret='****'
accessToken='****'
accessTokenSecret= '****'
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

search = function(searchterm) {
  list = searchTwitter(searchterm, n=2000, lang="it")
  df = twListToDF(list)
  df = df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d')
  if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
  
  stack = read.csv(file=paste(searchterm, '_stack.csv'))
  stack = rbind(stack, df)
  stack = subset(stack, !duplicated(stack$text))
  write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
}
search("coronavirus") 


#Hydrator 
for (i in 1:length(vec)){
  temp=read.csv(vec[i],  sep=",")  #vector containing names of files to be hydrated
  temp=as_tibble(temp)
  
  it=temp %>% filter(Language == "it")%>% select((Tweet_ID))
  it=as.data.frame(it)
  out=write.table(format(it, digits=19), vec[i], col.names = F, row.names = F, quote=F, sep="\t")
}

for (i in 1:length(vec2)){
  temp=read.csv(vec2[i],  sep=",") #vector containing names of hydrated files
  temp=as_tibble(temp)
  corpus=bind_rows(corpus, temp) 
  
}

corpus=as_tibble(corpus)
tweet=corpus %>% filter(lang=="it")
tweet=tweet%>% distinct(id, .keep_all = T)
sort(unique(strftime(tweet$created_at, format="%Y-%m-%d"))) #range from 14-02  to 12-05

#convert dates to UTC format
chr=tweet%>%select(created_at)
created_day=vector()
created_hour=vector()
convert = function(chr, day, hour) {
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
    if (s1[2] == "May"){
      month="05"
    }  
    day[i]=paste( s1[6], month,s1[3], sep="-")
    hour[i]=s1[4]
  }
}
convert(chr, created_day, created_hour)

tweet$created_hour=created_hour
tweet$created_day=created_day
tweet=tweet %>% select(id, favorite_count, retweet_count, text, screen_name, created_at, latitude, longitude, created_hour, created_day)
tweet=bind_rows(tweet, coronavirus_stack)

retweet=corpus%>%filter(!is.na(retweet_id))
retweet=retweet%>% distinct(id, .keep_all = T)

#pre-processing

tweet$text=normalizzaTesti(as_vector(tweet$text),normalizzacaratteri=TRUE,tolower=TRUE,perl=FALSE,fixed=FALSE)

tweet$text=gsub("'", " ", tweet$text)
Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}
tweet$text=Textprocessing(tweet$text)

tweet$text=normalizzaTesti(tweet$text, remove=c("coronavirus","covid","fef", "ffb","ddc","wwwurlwww",  "covid","covid19", "dfc","ffa","fda", "fbc", "def","coronavirusitaly", "coronavirusitalia", "covid2019", "covid19italia", "coronaviriusitalia", "coronaviruslombardia", "covid19", "coronavirusitalla", "coranavirusitalia", "coronarvirusitalia"))


tweet$text=iconv(tweet$text, "latin1", "ASCII", sub="")
tweet$text=gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", tweet$text)
tweet = tweet %>% filter(text != "")


tweet$lan=textcat(tweet$text,ECIMCI_profiles)
table(tweet$lan)
tweet=tweet %>%filter(lan %in% c("it", "la")) 

#CHAPTER 2

corpus=Corpus(VectorSource(tweet$text))
dtm=DocumentTermMatrix(corpus,control = list( stemming = FALSE,stopwords=itastopwords,  minWordLength = 3,removeNumbers = FALSE, removePunctuation = TRUE,bounds=list(local = c(1,Inf)) ))

text_df=tweet %>% unnest_tokens(word,text)

#top retweets
tweet %>% 
  arrange(-retweet_count) %>%
  select(screen_name, text, retweet_count) %>%
  top_n(10) 

#top mentions
tweet %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)

#top hashtags
tweet  %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = TRUE) %>%
  filter(str_detect(hashtag, "^#"), !hashtag %in% c("#coronavirus","#covid", "#covid19", "#coronarvirusitalia","#coronavirusitalla", "#coronvirusitalia", "#coronavirusitaly", "#coronavirusitalia", "#covid2019", "#covid19italia", "#covid???19", "#coviditalia")) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(20)

#keywords 
tweet %>% filter(str_detect(text,"contagi "))
tweet %>% filter(str_detect(text,"mascherine"))

#users
tweet %>% group_by(screen_name) %>% count(sort=T) %>% top_n(20)

#tokens
wordcloudDF=text_df %>% count(word,sort=T)
wordcloud2(wordcloudDF, color = "skyblue")
bigrams=textcnt(tweet$text,method="string",n=2L,split="[[:blank:]]")
sort(bigrams,decreasing=TRUE)[1:30]
trigrams=textcnt(tweet$text,method="string",n=3L,split="[[:blank:]]")
sort(trigrams,decreasing=TRUE)[1:30]

#tf-idf
feb=tweet %>% filter(as.Date(created_day) > "2020-02-13" & as.Date(created_day) < "2020-03-08")
mar=tweet %>% filter(as.Date(created_day) > "2020-03-07" & as.Date(created_day) < "2020-04-02")
apr=tweet %>% filter(as.Date(created_day) > "2020-04-01" & as.Date(created_day) < "2020-04-25")
mag=tweet %>% filter(as.Date(created_day) > "2020-04-24" & as.Date(created_day) < "2020-05-15")

testo1=tweet %>% inner_join(feb) %>% mutate(month= "feb")
testo2=tweet %>% inner_join(mar) %>% mutate(month= "mar")
testo3=tweet %>% inner_join(apr) %>% mutate(month= "apr")
testo4=tweet %>% inner_join(mag) %>% mutate(month= "mag")

tweet_words = bind_rows(testo1,testo2,testo3,testo4)%>%
  unnest_tokens(word, text) %>%
  count(month, word, sort = TRUE)

total_words <- tweet_words  %>% 
  group_by(month) %>% 
  summarize(total = sum(n))
tweet_words <- left_join(tweet_words, total_words)

tweet_words <- tweet_words %>%
  bind_tf_idf(word, month, n)

idf=tweet_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tweet_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(month) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()

#CHAPTER 3

#original messages extraction
retweet=retweet %>% rename(screen_name=user_screen_name.1,OP=retweet_id) %>% filter (!is.na(OP)) %>% distinct()
retweet %>%distinct(OP)

id=select(retweet, OP)
id$OP=as.character(id$OP)
id=as.data.frame(id)
write.table(format(id, digits=19), "retweet.txt", col.names = F, row.names = F, quote=F, sep="\t")

originals=as_tibble(hydrated_retweet)
originals=originals %>% distinct(id, .keep_all = TRUE)
originals=originals %>% rename( OP = id)
originals$OP=as.double(originals$OP)
join=inner_join(originals,retweet, by = "OP")
join %>% select(created_at.x, created_at.y)
join %>% select(text.x, text.y)


#response time
created_orig=join %>% select(created_at.x)
created_ret=join %>% select(created_at.y)

created_day_orig=vector()
created_hour_orig=vector()
created_day_ret=vector()
created_hour_ret=vector()
convert(created_orig, created_day_orig, created_hour_orig)
convert(created_ret, created_day_ret, created_hour_ret)
join$created_day_ret=created_day_ret
join$created_hour_ret=created_hour_ret
join$created_day_orig=created_day_orig
join$created_hour_orig=created_hour_orig	
join$diff=as.numeric(difftime(strptime(paste(join$created_day_orig, join$created_hour_orig),"%Y-%m-%d %H:%M:%S"),
                              strptime(paste(join$created_day_ret, join$created_hour_ret),"%Y-%m-%d %H:%M:%S")))
median(join$diff, na.rm=T) #9820 seconds, 2.7h
mean(join$diff, na.rm=T) #50432 secpnds, 14h
join2=join%>%filter(is.na(diff) == FALSE) 

ggplot(data=join2, mapping=aes(x=(abs(diff)/3600)))+
  stat_density(aes(y=..count..), fill="blue", alpha=0.3) +
  scale_x_continuous(breaks=c(0,0.2,0.5,1,2.7,5,14,24,48,100,250,500,1000,5000,10000), trans="log1p", expand=c(0,0)) +
  scale_y_continuous(breaks=c(0, 1000, 2000,3000,4000,10000), expand=c(0,0)) +
  theme_bw()+ xlab("hours")

#rapid retweets
less_than=join%>% filter(abs(diff) < 10000) %>% distinct(OP, .keep_all = TRUE)
feb=less_than %>% filter(as.Date(created_day_ret) == "2020-02-19")
mar=less_than %>% filter(as.Date(created_day_ret) > "2020-02-19" & as.Date(created_day_ret) < "2020-02-25")
testo1=less_than %>% filter(user_description.x != "") %>% inner_join(feb) %>% mutate(month= "1")
testo2=less_than %>% filter(user_description.x != "")%>%inner_join(mar) %>% mutate(month= "2")

tweet_words=bind_rows(testo1,testo2) %>% unnest_tokens(word,text.x) %>% count(month, word,sort=T) 
total_words <- tweet_words %>% 
  group_by(month) %>% 
  summarize(total = sum(n))
tweet_words <- left_join(tweet_words, total_words)
tweet_words <- tweet_words %>% 
  select(-total) %>%
  bind_tf_idf(word, month,n)
idf=tweet_words %>%
  arrange(desc(tf_idf))

tweet_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(month) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, col = "blue")) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()


bigrams=bind_rows(testo1,testo2) %>% unnest_tokens(bigram,user_description.x, token="ngrams",n=2)
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_united <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(month, bigram) %>%
  bind_tf_idf(bigram, month, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(month) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(x= reorder(bigram,tf_idf),tf_idf, col="blue")) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()

#visualization 

top_orig=originals%>% group_by(user_screen_name) %>% count(sort=T) %>% top_n(50)
top_ret=join%>% group_by(user_screen_name.y) %>% count(sort=T) %>% top_n(50)


join %>% filter(user_screen_name.x != "") %>% 
  filter(user_screen_name.y != "") %>% filter(user_screen_name.x %in% top_orig[2:100,]$user_screen_name) %>%
  filter(user_screen_name.y %in% top_ret[1:100,]$user_screen_name.y) %>%
  select(user_screen_name.x, user_screen_name.y) %>%
  filter(!is.na(user_screen_name.y)) %>%
  graph_from_data_frame() -> rt_g

V(rt_g)$node_label <- unname(names(V(rt_g)))

# Size of node
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 1, degree(rt_g), 1)) 

# Adjust angle of label based on position
nIds <- length(V(rt_g))
V(rt_g)$Id <- seq(1:nIds)
V(rt_g)$label_angle <- 90 - 360 *  V(rt_g)$Id / nIds
V(rt_g)$hjust <- ifelse(V(rt_g)$label_angle < -90, 1, 0)

# Flip text depending on what side of the plot it is on
V(rt_g)$angle <- ifelse(V(rt_g)$label_angle < -90, V(rt_g)$label_angle+180, V(rt_g)$label_angle)

p <- ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(aes(alpha=..index..)) +
  geom_node_point(aes(x = x*1.07, y=y*1.07, size=node_size,  alpha=0.2)) +
  geom_node_text(aes(x=x*1.15, y=y*1.15,label=node_label, angle=angle, hjust=hjust),
                 color="dodgerblue", size=2.7, family=font_rc) +
  coord_fixed() +
  labs(title="#coronavirus Relationships", subtitle="Darker edges == more retweets. Node size == larger degree") +
  theme_graph(base_family=font_rc) +
  theme(legend.position="none") +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

p


#CHAPTER 4

#lexicon based unigram classification
sent=sentiment(as.vector(text_df$word))
pos=subset(sent,sent==1)
temp=bind_rows(pos)
pos=gather(temp, key="word")

neg=subset(sent,sent==-1)
temp=bind_rows(neg)
neg=gather(temp, key="word")

pos_df=text_df %>%
  inner_join(pos) %>%
  count(word, sort = TRUE)%>%
  mutate(sentiment="positive") 
neg_df=text_df %>%
  inner_join(neg) %>%
  count(word, sort = TRUE)%>%
  mutate(sentiment="negative") 
word_counts=full_join(pos_df, neg_df) %>% arrange(desc(n))

word_counts %>%
  group_by(sentiment) %>%
  top_n(30, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


#daily sentiment index
date=sort(unique(tweet$created_day))
vec=data.frame(sentiment=integer(), date=as.Date(character()),stringsAsFactors=FALSE) 
wordcounts=select(word_counts, word, sentiment)

for (i in 1:length(date)){
  temp = tweet %>% filter(as.Date(created_day) == date[i])
  tokens <- data_frame(temp) %>% unnest_tokens(word, text)
  
  sentiment <- tokens %>%
    inner_join(wordcounts) %>%
    count(sentiment) %>% # 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative) 
  
  df=data.frame(sentiment$sentiment,as.Date(temp$created_day[1]))
  colnames(df) = c("sentiment", "date")
  vec=rbind(vec, df)
}

vec$sentiment=(vec$sentiment-max(vec$sentiment))/(max(vec$sentiment)-min(vec$sentiment))
ggplot(vec, aes(x = date, y = sentiment)) + 
  geom_smooth(method = "auto") +
  geom_point()+
  ylim(-2.2, -0.8)


#CHAPTER 5

tweet_corpus <- corpus(testo$text)


corpus_tokens <- tweet_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() 

tweet_collocations <- textstat_collocations(corpus_tokens, min_count = 10)
tweet_collocations <- tweet_collocations[1:300, ]

corpus_tokens <- tokens_compound(corpus_tokens, tweet_collocations)

DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.6, docfreq_type = "prop")

dim(DTM)
top10_terms <- c( "non", "italia", "emotelove", "emergenza", "ora", "solo", "ecco", "cosa", "perch", "cos", "dopo", "prima")

DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- tweet[sel_idx, ]

topicModel <- LDA(DTM, k=7, method="Gibbs", control=list(iter = 500, verbose = 25))
tmResult <- posterior(topicModel)
attributes(tmResult)

beta <- tmResult$terms 
theta <- tmResult$topics 

terms(topicModel, 10)

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
M=M%>%left_join(nomi)
M1=M%>%filter(topic==1)


p=ggplot(M1,aes(x=date,y=value)) + 
  geom_line(color = palette_light()[[1]], size=.6) +facet_wrap(~nome)+ theme_tq() 

p + geom_vline(xintercept = as.Date("2020-03-08"), linetype="dashed") +
  geom_vline(xintercept = as.Date("2020-03-05"), linetype="dashed", color="red", size=1) + 
  geom_vline(xintercept = as.Date("2020-03-28"), linetype="dashed", color="red", size=1) + 
  geom_vline(xintercept = as.Date("2020-04-20"), linetype="dashed", color="red", size=1) + 
  geom_vline(xintercept = as.Date("2020-03-22"), linetype="dashed") +
  geom_vline(xintercept = as.Date("2020-04-10"), linetype="dashed") +
  geom_vline(xintercept = as.Date("2020-04-26"), linetype="dashed") +
  geom_vline(xintercept = as.Date("2020-05-04"), linetype="dashed") + geom_smooth(method="loess", se=F, size=1)+   ylim(0.139,0.152)

M2=M%>%filter(topic==2)

pp=ggplot(M2,aes(x=date,y=value)) + 
  geom_line(color = palette_light()[[1]], size=.6) +facet_wrap(~nome)+ theme_tq() 

pp+
  geom_vline(xintercept = as.Date("2020-03-05"), linetype="dashed", color="red", size=1) + 
  geom_vline(xintercept = as.Date("2020-03-28"), linetype="dashed", color="red", size=1) + 
  geom_vline(xintercept = as.Date("2020-04-20"), linetype="dashed", color="red", size=1) + 
  geom_smooth(method="loess", se=F, size=1)+   ylim(0.139,0.152)


M3=M%>%filter(topic==7)

pp=ggplot(M3,aes(x=date,y=value)) + 
  geom_line(color = palette_light()[[1]], size=.6) +facet_wrap(~nome)+ theme_tq() 

pp+
  geom_vline(xintercept = as.Date("2020-03-05"), linetype="dashed", color="red", size=1) + 
  geom_vline(xintercept = as.Date("2020-03-28"), linetype="dashed", color="red", size=1) + 
  geom_vline(xintercept = as.Date("2020-04-20"), linetype="dashed", color="red", size=1) + 
  geom_smooth(method="loess", se=F, size=1)+   ylim(0.139,0.152)

