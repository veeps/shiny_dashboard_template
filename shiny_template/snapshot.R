library(ggplot2)
library(tibble)
library(magrittr)
library(tidyverse)
library(dplyr)
library(scales)
library(stringr)

#read in data file
df <- read.csv("data/train_clean.csv") %>%
  as_data_frame()


#get summary table by content type
neighborhoods <- df %>%
  group_by(neighborhood) %>%
  summarise(
    Avg.quality=percent(mean(overall_qual)),
    Avg.year_built =as.integer(mean(year_built)),
    Avg.living_area =as.integer(mean(gr_liv_area)),
    Avg.price =as.integer(mean(saleprice)),
    Total.sales = n()
  ) %>%
  arrange(-Total.sales)




# see how many posts
Q3_Tweets <- tweets_joe_q3 %>%
  group_by(Category) %>%
  count()

Q3_Tweets$Percent.of.posts  <- (Q3_Tweets$n/nrow(tweets_joe_q3))*100
colnames(Q3_Tweets) <- c("Category", "Total.posts" , "Percent.of.posts")

class(Q3_Tweets$Percent.of.posts)
#combine tables
Q3_Overview_Tweets <- left_join(Q3_Overview, Q3_Tweets) 

#Look at posts with links
Q3_Overview_Tweets_Links <- Q3_Overview_Tweets %>% 
  filter(!Category %in% c( "Inspiration", "Culture" , "Joe Status", "Reply", "Retweet", "Live tweet", "Inspirational Quote"))


#Total tables
Q3_Totals_Links <- Q3_Overview_Tweets_Links %>%
  summarise(
    Total.clicks = sum(Q3_Overview_Tweets$Total.clicks),
   Total.posts = sum(Total.posts)
  )



###########

#Look at Evergreen
Evergreen <- tweets_joe_q3 %>%
  filter(Category %in% "Evergreen") %>%
  group_by(Challenge.Type) %>%
  summarise(
    Total.clicks=sum(url.clicks),
    Avg.clicks =as.integer(mean(url.clicks))
    #Total.engagements=sum(engagements),
    #Avg.engagements =as.integer(mean(engagements)),
    #Avg.engagement.rate=percent(mean(engagement.rate))
  ) %>%
  arrange(-Avg.clicks)

# see how many posts
Evergreen_Tweets <- tweets_joe_q3 %>%
  filter(Category %in% "Evergreen") %>%
  group_by(Challenge.Type) %>%
  count() 

colnames(Evergreen_Tweets) <- c("Challenge.Type", "Total.posts")

#combine tables
Evergreen_Overview_Tweets <- left_join(Evergreen, Evergreen_Tweets, by ="Challenge.Type") %>%
  arrange(-Avg.clicks) %>%
  top_n(10, wt="Avg.clicks")



#chart summary of avg clicks by content 
ggplot(Q3_Overview_Tweets_Links, aes(fill=Category, y=Avg.clicks, x=Category)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Q3 average clicks per tweet") +
  labs(caption="Data pulled from Twitter Analytics between 07/01/2019-09/30/2019")+
  labs(y="Average clicks")


#chart summary of total clicks by content 
ggplot(Q3_Overview_Tweets_Links, aes(fill=Category, y=Total.clicks, x=Category)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Q3 total clicks per tweet") +
  labs(caption="Data pulled from Twitter Analytics between 07/01/2019-09/30/2019")+
  labs(y="Total clicks")

#chart summary of tweets by content 
ggplot(Q3_Overview_Tweets_Links, aes(fill=Category, y=Total.posts, x=Category)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Q3 Tweets by Category") +
  labs(caption="Data pulled from Twitter Analytics between 07/01/2019-09/30/2019")+
  labs(y="Total posts")


colnames(Q3_Overview_Tweets_Links)
#chart summary of clicks by type 
ggplot(Q3_Overview_Tweets_Links, aes(fill=Content, y=Avg.clicks, x=Type)) + 
  geom_bar( stat="identity") + ggtitle("HITRECORD average clicks per tweet by post type") +
  labs(caption="Data pulled from Twitter Analytics between 12/01/2018-01/07/2019", y="Average clicks")



colnames(tweets_joe_q3)


############ Look at Zappos posts
Zappos <- tweets_joe_q3 %>%
  filter(Category %in% "Zappos") %>%
  summarise(
    Total.clicks=sum(url.clicks),
    Avg.clicks =as.integer(mean(url.clicks)),
    Total.engagements=sum(engagements),
    Impressions=sum(impressions),
    Avg.engagements =as.integer(mean(engagements)),
   Avg.engagement.rate=percent(mean(engagement.rate))
  ) %>%
  arrange(-Impressions)


write_csv2(Zappos,"data/zappos_tweets.csv")


