library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)

#############################
#Get Twitter Authentification
#############################

key="lQxWjN6LvpFdWpCcf6IetvcXC"
secret="f8c3753dddYR7DDsrZ4wrmElPyi925ZpXkEOJfpzpnOrLsn28m"
setwd("/Users/jamesmartherus/Documents/TwitterScrapingProject/")

authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret)
save(authenticate, file="twitter authentication.Rdata")

####################################
#Get 2000 tweets from various cities
####################################
N=50  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,33.5,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-112,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7)

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seattle,Vegas,Phoenix,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul
#11,13,15,17,18,19,20,21
#####Get Trump Tweets########
donald=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Trump',
                                                                      lang="en",n=N,resultType="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

donaldlat=sapply(donald, function(x) as.numeric(x$getLatitude()))
donaldlat=sapply(donaldlat, function(z) ifelse(length(z)==0,NA,z))  

donaldlon=sapply(donald, function(x) as.numeric(x$getLongitude()))
donaldlon=sapply(donaldlon, function(z) ifelse(length(z)==0,NA,z))  

donalddate=lapply(donald, function(x) x$getCreated())
donalddate=sapply(donalddate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

donaldtext=sapply(donald, function(x) x$getText())
donaldtext=unlist(donaldtext)

isretweet=sapply(donald, function(x) x$getIsRetweet())
retweeted=sapply(donald, function(x) x$getRetweeted())
retweetcount=sapply(donald, function(x) x$getRetweetCount())

favoritecount=sapply(donald, function(x) x$getFavoriteCount())
favorited=sapply(donald, function(x) x$getFavorited())

data_trump=as.data.frame(cbind(candidate="Trump",tweet=donaldtext,date=donalddate,lat=donaldlat,lon=donaldlon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))


#######Get Clinton Tweets########
hillary=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Clinton',
                                                                       lang="en",n=N,resultType="recent",
                                                                       geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

hillarylat=sapply(hillary, function(x) as.numeric(x$getLatitude()))
hillarylat=sapply(hillarylat, function(z) ifelse(length(z)==0,NA,z))  

hillarylon=sapply(hillary, function(x) as.numeric(x$getLongitude()))
hillarylon=sapply(hillarylon, function(z) ifelse(length(z)==0,NA,z))  

hillarydate=lapply(hillary, function(x) x$getCreated())
hillarydate=sapply(hillarydate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

hillarytext=sapply(hillary, function(x) x$getText())
hillarytext=unlist(hillarytext)

isretweet=sapply(hillary, function(x) x$getIsRetweet())
retweeted=sapply(hillary, function(x) x$getRetweeted())
retweetcount=sapply(hillary, function(x) x$getRetweetCount())

favoritecount=sapply(hillary, function(x) x$getFavoriteCount())
favorited=sapply(hillary, function(x) x$getFavorited())

data_clinton=as.data.frame(cbind(candidate="Clinton",tweet=hillarytext,date=hillarydate,lat=hillarylat,lon=hillarylon,
                                 isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

########Get Cruz Tweets#########
ted=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Cruz',
                                                                       lang="en",n=N,resultType="recent",
                                                                       geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

tedlat=sapply(ted, function(x) as.numeric(x$getLatitude()))
tedlat=sapply(tedlat, function(z) ifelse(length(z)==0,NA,z))  

tedlon=sapply(ted, function(x) as.numeric(x$getLongitude()))
tedlon=sapply(tedlon, function(z) ifelse(length(z)==0,NA,z))  

teddate=lapply(ted, function(x) x$getCreated())
teddate=sapply(teddate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

tedtext=sapply(ted, function(x) x$getText())
tedtext=unlist(tedtext)

isretweet=sapply(ted, function(x) x$getIsRetweet())
retweeted=sapply(ted, function(x) x$getRetweeted())
retweetcount=sapply(ted, function(x) x$getRetweetCount())

favoritecount=sapply(ted, function(x) x$getFavoriteCount())
favorited=sapply(ted, function(x) x$getFavorited())

data_cruz=as.data.frame(cbind(candidate="Cruz",tweet=tedtext,date=teddate,lat=tedlat,lon=tedlon,
                                 isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

########Get Kasich Tweets#########
kasich=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Kasich',
                                                                   lang="en",n=N,resultType="recent",
                                                                   geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

kasichlat=sapply(kasich, function(x) as.numeric(x$getLatitude()))
kasichlat=sapply(kasichlat, function(z) ifelse(length(z)==0,NA,z))  

kasichlon=sapply(kasich, function(x) as.numeric(x$getLongitude()))
kasichlon=sapply(kasichlon, function(z) ifelse(length(z)==0,NA,z))  

kasichdate=lapply(kasich, function(x) x$getCreated())
kasichdate=sapply(kasichdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

kasichtext=sapply(kasich, function(x) x$getText())
kasichtext=unlist(kasichtext)

isretweet=sapply(kasich, function(x) x$getIsRetweet())
retweeted=sapply(kasich, function(x) x$getRetweeted())
retweetcount=sapply(kasich, function(x) x$getRetweetCount())

favoritecount=sapply(kasich, function(x) x$getFavoriteCount())
favorited=sapply(kasich, function(x) x$getFavorited())

data_kasich=as.data.frame(cbind(candidate="Kasich",tweet=kasichtext,date=kasichdate,lat=kasichlat,lon=kasichlon,
                              isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

########Get Sanders Tweets#########
bernie=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Sanders',
                                                                   lang="en",n=N,resultType="recent",
                                                                   geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

bernielat=sapply(bernie, function(x) as.numeric(x$getLatitude()))
bernielat=sapply(bernielat, function(z) ifelse(length(z)==0,NA,z))  

bernielon=sapply(bernie, function(x) as.numeric(x$getLongitude()))
bernielon=sapply(bernielon, function(z) ifelse(length(z)==0,NA,z))  

berniedate=lapply(bernie, function(x) x$getCreated())
berniedate=sapply(berniedate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

bernietext=sapply(bernie, function(x) x$getText())
bernietext=unlist(bernietext)

isretweet=sapply(bernie, function(x) x$getIsRetweet())
retweeted=sapply(bernie, function(x) x$getRetweeted())
retweetcount=sapply(bernie, function(x) x$getRetweetCount())

favoritecount=sapply(bernie, function(x) x$getFavoriteCount())
favorited=sapply(bernie, function(x) x$getFavorited())

data_sanders=as.data.frame(cbind(candidate="Sanders",tweet=bernietext,date=berniedate,lat=bernielat,lon=bernielon,
                              isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

#Combine Candidate Tweets
data <- rbind(data_trump,data_clinton,data_cruz,data_kasich,data_sanders)

############################
#Get interpretable locations
############################

#remove tweets without lat/lon
data=filter(data, !is.na(lat),!is.na(lon))

#get address using lat/lon
lonlat=select(data,lon,lat)
lonlat$lon <- as.numeric(as.character(lonlat$lon))
lonlat$lat <- as.numeric(as.character(lonlat$lat))
# 
# #google maps API only allows 2500 queries per day. Can we get around this?
# result <- do.call(rbind, lapply(1:2000,
#                                 function(i) revgeocode(as.numeric(lonlat[i,1:2]))))
# 
# #Parse full address to multiple, more useful fields
# data2=lapply(result,  function(x) unlist(strsplit(x,",")))
# address=sapply(data2,function(x) paste(x[1:3],collapse=''))
# city=sapply(data2,function(x) x[2])
# stzip=sapply(data2,function(x) x[3])
# zipcode = as.numeric(str_extract(stzip,"[0-9]{5}"))   
# state=str_extract(stzip,"[:alpha:]{2}")
# data2=as.data.frame(list(address=address,city=city,zipcode=zipcode,state=state))
# 
# #Until I figure out how to reverse geocode ALL addresses, I need to just use 100
# data=data[1:2000,]
# data=cbind(data,data2)

tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet)
data$tweet=tweet

###################
#Sentiment Analysis
###################

#Load positve and negative words for lexicon based sentiment analysis
positives= readLines("positivewords.txt")
negatives = readLines("negativewords.txt")

#Wrapper function to calculate sentiment scores
sent_score <- function(tweet, positive_words, negative_words){
  tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
  tweet = gsub("[[:cntrl:]]", "", tweet)    # remove control characters
  tweet = gsub("[0-9]", "", tweet)          # remove digits
  
  # Error handling function when trying tolower function
  tryTolower = function(x){
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # use tryTolower with sapply
  tweet = sapply(tweet, tryTolower)
  # split sentence into words with str_split function from stringr package
  word_list = str_split(tweet, " ")
  words = unlist(word_list)
  # compare words to the dictionaries of positive & negative terms
  positive_matches = match(words, positive_words)
  negative_matches = match(words, negative_words)
  # get the position of the matched term or NA
  # we just want a TRUE/FALSE
  positive_matches = !is.na(positive_matches)
  negative_matches = !is.na(negative_matches)
  # final score
  score = sum(positive_matches) - sum(negative_matches)
  return(score)
}
sentiment_scores = function(tweets, positive_words, negative_words){
  scores = lapply(tweets, sent_score, positive_words, negative_words)
  return(scores)
}

score = sentiment_scores(tweet, positives, negatives)
data$score=unlist(as.numeric(score))

#########
#Analysis
#########
hist(as.numeric(data$score), xlab="Sentiment Score", main="Sentiment of Tweets about Donal Trump",
     border="black",col="skyblue")

qplot(data$score, binwidth=1, col=data$candidate, xlab="Sentiment Score",ylab="Number of Tweets")
trump_mean <- mean(data$score[data$candidate=="Trump"])
cruz_mean <- mean(data$score[data$candidate=="Cruz"])
kasich_mean <- mean(data$score[data$candidate=="Kasich"])
clinton_mean <- mean(data$score[data$candidate=="Clinton"])
sanders_mean <- mean(data$score[data$candidate=="Sanders"])

xtabs(data$score ~ data$candidate) # mean of each candidate

boxplot(score ~ candidate, data=data)

#wordcloud
trump_corpus <- Corpus(VectorSource(sent_data$tweet[sent_data$candidate=="Trump"))

# trump_corpus <- tm_map(trump_corpus,
#                    content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
#                    mc.cores=1)

trump_corpus <- tm_map(trump_corpus, content_transformer(toLower), lazy=TRUE)
trump_corpus <- tm_map(trump_corpus, PlainTextDocument, lazy=TRUE)
trump_corpus <- tm_map(trump_corpus, content_transformer(removePunctuation), lazy=TRUE)
#stopwords are things like I, me, etc.
trump_corpus <- tm_map(trump_corpus, content_transformer(removeWords), stopwords('english'), lazy=TRUE)
#this converts all words to stem, i.e., standing to stand
trump_corpus <- tm_map(trump_corpus, content_transformer(stemDocument), lazy=TRUE)


wordcloud(trump_corpus, max.words=100, random.order=FALSE)


#######
#Export
#######
write.csv(as.data.frame(data), "Sentiment_Analysis.csv")

