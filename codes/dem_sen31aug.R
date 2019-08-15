# ========================================================================================
# Sentiment Analysis of Post Demon Numbers
# ========================================================================================
# Load the required R libraries
library(twitteR)
library(ROAuth)
library(RCurl)
library(base64enc)      
library(wordcloud)
library(Rgraphviz)
library(httr)
library(devtools)
library(ggplot2)
library(tm)
library(extrafont)
library(lubridate)
library(dplyr)
library(SnowballC)
loadfonts(quiet = T)
# ========================================================================================
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

# SETUP TWITTER
# ========================================================================================
# Authentication
# ---------------
pt <- "/Users/parthkhare/Desktop/TwitterSurvey"
load(paste0(pt,"/twit_cred.RData"))
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_token_secret)
# ========================================================================================

# dd <- dem
# Extract Text from Twitter
# ========================================================================================
# Load Twitter
# ---------------
t <- Sys.time()
dem <- searchTwitter("#demonetisation", n = 400000)
length(dem)
t1 <- Sys.time()-t
system.time(dm77 <- do.call("rbind", lapply(dem, as.data.frame)))
save(dm, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem1.5_31aug.RData")
# save(dm55, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem55_31aug.RData")
# save(dm85, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem85_2sep.RData")
# save(dm155, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem1.5_6sep.RData")
# save(dm77, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem77_10sep.RData")
# ========================================================================================


# # Comparing Different Twitter Data Strands
# # ========================================================================================
# # Load Pre loaded
# # ---------------
# # 77k
# load(file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem_31aug.RData")
# dm77 <- dm
# # 185k
# load(file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem1.5_6sep.RData")
# # 85k
# load(file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem85_2sep.RData")
# dm85 <- dm1
# # 55k
# load(file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Dem_31aug.RData")
# dm55 <- dm


# # Load Pre loaded
# # ---------------
# table(dm155$text %in% dm85$text)
# table(dm155$text %in% dm55$text)
# table(dm55$text %in% dm85$text)
# 
# # Load Pre loaded
# # ---------------
# mt <- rbind(dm55, dm85, dm155, dm77)
# mt1 <- mt[!duplicated(mt),]
# dim(mt1)
# dm <- mt1

# Save 1 :All Tweets Stitched from different time periods 
# save(mt, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/Twfin10sep/AllDemTw_10sep.RData")
# Save 2 :All Tweets Stitched from different time periods 
# save(mt1, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/Twfin10sep/DemTwNoRep_10sep.RData")
# # ========================================================================================

# Cleaning and Corpus
# ========================================================================================
load(file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwFin10sep/DemTwNoRep_10sep.RData")
dm <- mt1
# Convert tweets to data frame
# ---------------
dm$tx <- dm$text

# Text Transformations
# ---------------# ---------------# ---------------# ---------------# ---------------
# Remove RT
dm$tx <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dm$tx)
# Replace blank space (???rt???)
dm$tx <- gsub("rt", "", dm$tx)
# Replace @UserName
dm$tx <- gsub("@\\w+", "", dm$tx)
# Remove punctuation
dm$tx <- gsub("[[:punct:]]", "", dm$tx)
# Remove links
dm$tx <- gsub("http\\w+", "", dm$tx)
# Remove Special Characters
dm$tx <- gsub("[^a-zA-Z0-9 ]","",dm$tx)
# Remove blank spaces at the beginning
dm$tx <- gsub("^ ", "", dm$tx)
# Remove blank spaces at the end
dm$tx <- gsub(" $", "", dm$tx)
# Remove tabs
dm$tx <- gsub("[ |\t]{2,}", "", dm$tx)
# ---------------# ---------------# ---------------# ---------------# ---------------

# Check for Repeated Id's
# ---------------
dm$rep <- paste0(dm$tx,"-",dm$created,"-",dm$screenName,"-",dm$id)
dm <- dm[!duplicated(dm$rep),]  # 1.5lks {6sep}  # 1.6 {10 sep}

# Save Non Repeated Text Cleaned
# save(dm, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwFin10sep/DemMt.prsd_10sep.RData")
# ========================================================================================

# CORPUS and CLOUD
# ========================================================================================
# Create corpus of tweveets
# ---------------
crp <- Corpus(VectorSource(dm$tx))

# Strelaization
# ---------------
# strip white space
crp <- tm_map(crp, stripWhitespace)
# convert all characters to lower case
crp <- tm_map(crp, tolower, lazy = T)

# # Include 'stop words' to the standard ones
# # ---------------
twev.stp <- c(stopwords('english'),"amp","can","say","came","http",
              "per","said","give","will","now","come","new","says")
# remove stop words
crp <- tm_map(crp, removeWords, twev.stp)

# Create document term matrix
# ---------------
# crp <- tm_map(crp, PlainTextDocument,lazy = T)   # Retain the TYPE
# twev.tdm <- DocumentTermMatrix(crp, control= list(minWordlength= 5))
# save(crp, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwFin10sep/Corp.DemMt_10sep.RData")

# Wordcloud
# ---------------
col <- palette()
col <- c("black","red","forestgreen","blue","deeppink2","magenta",
         "orange2","gray40")
wordcloud(crp, max.words = 100,min.freq = 30,scale = c(1.5,1), 
          random.order = FALSE,rot.per = 0.5,vfont = c("sans serif","plain"),
          colors = col,random.color = TRUE)
# ========================================================================================


# SENTIMENT ANALYSIS [Basic +/- matching]
# ========================================================================================
# Load the Sentiment Score Function
source("/Users/parthkhare/Desktop/TwitterSurvey/Lexicon/SentiScr_basic.R")

# Read Lexicon of all positive and negative words
# ---------------
pos.words <- read.csv("/Users/parthkhare/Desktop/TwitterSurvey/Lexicon/Positive Words.csv")
neg.words <- read.csv("/Users/parthkhare/Desktop/TwitterSurvey/Lexicon/Negative Words.csv")

# Scan the words into R
# ---------------
pos.words <- scan("/Users/parthkhare/Desktop/TwitterSurvey/Lexicon/Positive Words.csv",
                  what = 'character')
neg.words <- scan("/Users/parthkhare/Desktop/TwitterSurvey/Lexicon/Negative Words.csv",
                  what = 'character')

# Add +/- words Manually to the list
# ---------------
pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')
neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad', 'no','freaking','sucks','horrible')

# Apply sentiment Function to the tweets
# ---------------
system.time(res <- scr.snt(dm$tx,pos.words,neg.words))
class(res)
summary(res$score)

# 'res' and 'r2' and both datasets with more negative than neutral tweets
# It was obsserved that some after a certain amount of trails the algo
# was giving more neurtal and than negative responses 
# --------------------------
save(res, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwFin10sep/SntMan_10sep.RData")
# save(r2, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/SntMan.DemMt_HiNeg_6sep.RData")
# ========================================================================================


# Sentiment Analysis : Visualization
# ========================================================================================
# Sentiment Distrbution
# ---------------
hist(res$score,col ="yellow", main ="Score of tweets",
     ylab = " Count of tweets")
res$sn <- ifelse(res$score > 0, "Positive","Negative")
res$sn <- ifelse(res$score == 0, "Neutral",res$sn)

# ---------------# ---------------# ---------------# ---------------#
# Aggregate +/-
# ---------------# ---------------# ---------------# ---------------#
g4 <- ggplot(res, aes(x = factor(score), fill = sn))
g4 <- g4 + geom_bar(stats = "identity")
g4 <- g4 + ggtitle("Demonetization Sentiment: Frequency Count") 
g4 <- g4 + xlab("Sentiment Spectrum") + ylab("Frequency")
g4 <- g4 + theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + labs(fill = "Sentiment")
g4
# GG 2
g5 <- ggplot(res, aes(x = score, colour = sn))
g5 <- g5 + geom_freqpoly()
g5 <- g5 + ggtitle("Sentiment Manual function") 
g5 <- g5 + xlab("Sentiment Spectrum") + ylab("Frequency")
g5
rr <- res[res$score != 0,]
nrow(res)-nrow(rr)
# ---------------# ---------------# ---------------# ---------------#



# ---------------# ---------------# ---------------# ---------------#
# Sentiment Wave by Time
# ---------------# ---------------# ---------------# ---------------#
# Add Basic Snetiment to main data
# ---------------
dm$res <- res 
dmf <- do.call(data.frame, dm)
View(dmf)

# Tweet Sentiment by Time
# ---------------
dmf <- dmf[order(as.Date(dmf$created, format="%d/%m/%Y")),]
dmf$dt <- cut(dmf$created, breaks = "2 hour")
stat <- mutate(dmf, tweet=ifelse(dmf$res.score > 0, 'positive', 
                ifelse(dmf$res.score < 0, 'negative', 'neutral')))
twday <- group_by(stat, tweet, dt)
twday <- summarise(twday, number=n())
twday$date <- as.POSIXct(twday$dt, tz = "UTC")
twday$date <- with_tz(twday$date, "Asia/Kolkata")
names(twday)[names(twday) == "tweet"] <- "sentiment"
save(twday, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwFin10sep/Wv.twday_10sep.RData")

# Main Sentiment Wave
# --------------------
gd <- ggplot(twday, aes(date, number))
gd <- gd + geom_line(aes(group=sentiment, color=sentiment), size=1)
gd <- gd + geom_point(aes(group=sentiment, color=sentiment), size=4) 
gd <- gd + theme(text = element_text(size=10), 
                 axis.text.x = element_text(angle=90, vjust=1)) 
gd <- gd + ggtitle(paste0("Sentiment Wave by Time"))
gd <- gd + scale_x_datetime(labels = date_format("%d-%b-%y", tz = "Asia/Kolkata"),
                             breaks = date_breaks("1 day"))
gd <- gd + xlab("Tweet Time") + ylab("Frequency")
gd <- gd + theme(plot.title = element_text(hjust = 0.5))
gd <- gd + labs(fill = "Sentiment")
gd

# Add Events to Plot: Event [GDP Announced]
# --------------------
gdp <- as.POSIXct("2017-08-31 17:30:00", tz = "Asia/Kolkata")
gd1 <- gd + geom_vline(aes(xintercept = as.numeric(gdp)),
                        linetype=5,color = "aquamarine4")
gd1 <- gd1 + geom_text(aes(label = "GDP QII {5.7%}"), 
              x = as.numeric(gdp),
              y=5000, vjust = 1, hjust = -0.07, colour = "red")
gd1

# Add Events to Plot: Event [Demon Numbers Released]
# --------------------
drel <- as.POSIXct("2017-08-30 17:30:00", tz = "Asia/Kolkata") 
gd2 <- gd1 + geom_vline(aes(xintercept = as.numeric(drel)),
                        linetype=5,color = "aquamarine4")
gd2 <- gd2 + geom_text(aes(label = "Dmntztn Release {99% returned}"), 
                       x = as.numeric(drel),
                       y=6000, vjust = 1, hjust = 1, colour = "red")
# gd2
gdt <- gd2

# Final Scales and Data limits
# --------------------
library(scales)
lims <- as.POSIXct(strptime(c("2017-08-29 10:00","2017-09-03 00:00"), 
                            format = "%Y-%m-%d %H:%M"))    
gdf <- gdt + scale_x_datetime(labels = date_format("%d-%b-%y %H:%M", tz = "Asia/Kolkata"),
                             limits = lims,
                             breaks = date_breaks("4 hour"))
gdf
# ---------------# ---------------# ---------------# ---------------#


# ---------------# ---------------# ---------------# ---------------#
# GGLPOT: ReTweet Frequency
# ---------------# ---------------# ---------------# ---------------#
# Time of Day: Condense All Different Dates into 1
# ---------------
dmf$rt <- strftime(dmf$created, format="%T")
dmf$rt <- as.POSIXct(dmf$rt, format="%H:%M")
dmf$rt1 <- cut(dmf$rt, breaks = "1 hour")

# Aggregate Data
# ---------------
dmf <- data.table(dmf)
dmrt <- dmf[,.(tw = .N,retw = sum(retweetCount),
               scr = mean(res.score)), by = rt1]

# Melt
dmr <- melt(dmrt, id.vars = c("rt1"),measure.vars = c("tw", "retw"))
dmr$rt1 <- as.POSIXct(dmr$rt1)
gtdf <- ggplot(dmr, aes(x = rt1, y= value ,color = variable, group = variable))
gtdf <- gtdf + geom_line()
gtdf <- gtdf + ggtitle("Tweet/Retweet Density by Time")
gtdf <- gtdf + xlab("Time of Day") + ylab("Frequency")
gtdf <- gtdf + theme(plot.title = element_text(hjust = 0.5))
gtdf <- gtdf + theme(axis.text.x=element_text(angle=90))
gtdf <- gtdf + scale_x_datetime(breaks = date_breaks("1 hour"),
                                labels = date_format("%H:%M", tz = "Asia/Kolkata"))
plot(gtdf)

# ---------------# ---------------# ---------------# ---------------#
# GGLPOT: Any User Tweeted more ?
# ---------------# ---------------# ---------------# ---------------#
tb <- as.data.frame(table(dmf$screenName))
quantile(tb$Freq, probs = seq(0,1,0.1)); 
quantile(tb$Freq, probs = seq(0.9998,1,0.00001))
tb$Usr <- ifelse(tb$Freq > quantile(tb$Freq, 0.9998), as.character(tb$Var1), "Other")
tb1 <- tb[tb$Freq > quantile(tb$Freq, 0.9998),]

# Frequently Tweeting Users
ggplot(tb1, aes(x= Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + 
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, vjust=1)) +
  guides(fill = F) + ggtitle(paste0("Frequently Tweeting Users")) + 
  xlab("Twitter User ID") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5))
# ---------------# ---------------# ---------------
# ========================================================================================




# ========================================================================================
# SENTIMENT ANALYSIS [MIT library: Likely to use basic match  + training] 
# ========================================================================================
library(SentimentAnalysis)
# Run Analyzer
# ---------------
system.time(dm$sn <- analyzeSentiment(dm$tx))
dm2 <- do.call(data.frame, dm)
View(dm2)
save(dm2, file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwPrep/Snt.Dem85_2sep.RData")

# View Results
# ---------------
hist(dm2$sn.SentimentGI,col ="blue", main ="Demon Sentiment",
     ylab = " Count of tweets", xlab = "Naive Bayes: Sentiment Score")

# GG
# ---------------
dm2$sn <- ifelse(dm2$sn.SentimentGI > 0, "Positive","Negative")
dm2$sn <- ifelse(dm2$sn.SentimentGI == 0, "Neutral",dm2$sn)
table(dm2$sn)
g3 <- ggplot(dm2, aes(x = sn.SentimentGI, colour = sn))
g3 <- g3 + geom_freqpoly()
g3 <- g3 + ggtitle("Sentiment Analyser package") 
g3 <- g3 + xlab("Sentiment Spectrum") + ylab("Frequency")
g3 <- g3 + theme(axis.text.x=element_text(angle=90))
g3 <- g3 + theme(plot.title = element_text(hjust = 0.5))
g3
# ========================================================================================


# ========================================================================================
# Compare
# -------------
table(dm2$sn); 
table(res$sn)
# ========================================================================================


# ========================================================================================
# SIX EMOTION: Also Done via word match
# ========================================================================================
# Source Function
source("/Users/parthkhare/Desktop/TwitterSurvey/Lexicon/SentiScr_5md.pol.R")
source("/Users/parthkhare/Desktop/TwitterSurvey/Lexicon/SentiScr_5moods.R")

# classify emotion
class_emo <- classify_emotion(dm$tx, algorithm<-"bayes", prior<-1.0)
# get emotion best fit
emotion <- class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] <- "unknown"

# classify polarity
class_pol <- classify_polarity(dm$tx, algorithm<-"bayes")
# get polarity best fit
polarity <- class_pol[,4]

# data frame with results
sent_df <- data.frame(text=dm$tx, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df <- within(sent_df,emotion <- factor(emotion, 
                  levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Sentiment Analysis on Demonitization\n(classification by emotion)")

# plot distribution of emotions w/o unknown
sf <- sent_df[emotion != "unknown",]
nrow(sent_df)-nrow(sf)
ggplot(sf, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Sentiment Analysis on Demonitization\n(classification by emotion)")
# ========================================================================================
# FIN