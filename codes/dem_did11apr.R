# ========================================================================================
# Sentiment Shift: Demon Numbers
# ========================================================================================
# Load the required R libraries
library(twitteR); library(ROAuth); library(RCurl);library(base64enc)      
library(wordcloud);library(Rgraphviz);library(httr);library(devtools)
library(ggplot2);library(tm);library(extrafont);library(lubridate);library(dplyr)
loadfonts(quiet = T)
# ========================================================================================

# SETUP TWITTER
# ========================================================================================
# Authentication
# ---------------
pt <- "/Users/parthkhare/Desktop/TwitterSurvey"
load(paste0(pt,"/twit_cred.RData"))
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_token_secret)
# ========================================================================================

# Announcement [Post Numbers Announcement]
# ========================================================================================
# Announcement 8 Nov 16
# -----------------
pre <- read.csv("/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/DemoKaggle PreNov/demonetization-tweets.csv")
dim(pre)
plot(table(pre$created))

# Announcement 8 Nov 16
# -----------------


# ========================================================================================


# Cleaning and Corpus [Post Numbers Announcement]
# ========================================================================================
# Announcement of post numbers 30 August 17
# -----------------
load(file = "/Users/parthkhare/Desktop/TwitterSurvey/Demonetisation/Event/TwFin10sep/DemTwNoRep_10sep.RData")
pos <- mt1
rm(mt1)


# ========================================================================================
