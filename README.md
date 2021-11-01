# Bachelor's Thesis



The 2020 COVID-19 pandemic has severely affected every part of the world's human activity: from economics to healthcare systems, from tourism and personal services to education; few sectors have been left untouched.

To counteract the dramatic consequences, competing to create a situation of uncertainty that has not been experienced since the post-war period, governments and
institutions have been asked for prompt action and immense efforts. Likewise, the scientific community, from a wide range of fields, has been urged to concentrate
all its resources to help the competent authorities. Reference is obviously made in primis to microbiology and in particular to virology, but not only: consider,
for example, the task forces set up to plan the economic recovery, the need for an app to track the infected or to control the epidemic curve.

It is therefore evident that sciences such as epidemiology, statistics and informatics have played a leading role in coronavirus-related research. Also in this case,
just as happened in the biological field with, for example, the publication of the genetic sequence of SARS-CoV-2, we have witnessed a mobilization and sharing of
of resources that had rarely occurred before. It has been made available a truly remarkable amount of articles, datasets and codes online.
Among the various repositories there is no shortage of data extracted from social media, which have increasingly become
more and more a sounding board of public opinion during the last decade. In particular, in the case of natural disasters, terrorist attacks and precisely the spread of
diseases, the main suspect for the monitoring of public conversations is Twitter, the world's most popular microblogging platform.

In times of uncertainty the peculiarities of Twitter, such as accessibility and the need to concentrate information in a few characters, convey at the same time the use
and the study of the content of the messages, an index of the mood of the users and of their opinions. This is even more significant considering the speed with which the situation changes during a pandemic and the unpredictability during these adverse times.

Starting from this premise, this paper aims to respond to the exponential increase in Twitter activity, which has taken place at the same time as the pandemic, analyzing tweets from a syntactic and semantic point of view with the application of Natural Language Processing techniques. In particular, we focus on the messages published in Italy in a period that covers the most excited phases of the pandemic.
This choice stems from the absence of detailed analysis on these data, unlike the numerous ones carried out on English language tweets, and
provides the opportunity to deepen some text mining techniques and to give an idea of the entire process behind NLP: from data collection to the consolidation
of the discovered knowledge.

The thesis is divided into the following chapters:
- **Chapter 1**: we describe the two processes that were followed to the creation of the final dataset. We proceed with the description and selection
of the variables of interest, and then we move on to the application of preprocessing methods on the text of the tweets, the main suspect of the analysis;
- **Chapter 2**: some descriptive analyses are presented in order to better understand the structure of the dataset and roughly grasp the content of information content; we continue in this sense by focusing on some issues of relevance to the topic;
- **Chapter 3**: a brief study of retweets is carried out, evaluating both the "response time" and the content. In particular, we compare the speed of retweets for COVID-19 with other emergencies, especially related to the to the spread of infectious diseases;
- **Chapter 4**: we evaluate the psychological repercussions caused by the pandemic by analyzing the sentimental content of the messages. In this sense, we limit ourselves
to a classification of polarity (positive or negative) at the level of the unigrams. We then arrive at the trend of sentiment over the months;
- **Chapter 5**: the statistical model Latent Dirichlet Allocation (LDA) is applied to the tweets in order to obtain a subdivision of the texts on the basis
of the treated topic. The trend of topics over time is also visualized, trying to verify if there are dependencies between them and the news events related to the
chronic events related to the coronavirus.

As can be seen from the content of the chapters, given the topicality of the phenomenon under consideration, there is no lack of opportunity to make a comparison between the results obtained and what happened in the real world. However, since the work done is not free from criticalities and limitations, at the end of the elaboration these will come duly commented and discussed, trying to understand to what eventual distortions are due and which data would have been useful to have to improve the analysis.
