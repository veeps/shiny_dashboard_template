library(ggplot2)
library(tibble)
library(magrittr)
library(tidyverse)
library(dplyr)
library(scales)
library(stringr)
library(wesanderson)

#read in data file
df <- read.csv("data/train_clean.csv") %>%
  as_data_frame()


#get summary table by content type
neighborhoods <- df %>%
  group_by(neighborhood) %>%
  summarise(
    avg_quality=percent(mean(overall_qual)),
    avg_year_built =as.integer(mean(year_built)),
    avg_living_area =as.integer(mean(gr_liv_area)),
    avg_price =as.integer(mean(saleprice)),
    total_sales = n()
  ) %>%   filter(total_sales > 122) %>%
  arrange(-total_sales)






#chart summary of avg clicks by content 
ggplot(neighborhoods, aes(fill=neighborhood, y=total_sales, x=neighborhood, )) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Total Sales by Neighborhood") + 
  scale_fill_manual(values = wes_palette("Darjeeling2")) + 
  labs(y="Total Sales") + labs(x = "Neighborhood") + 
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank(), 
        plot.background = element_rect(colour = "black",size = 1)) 

#ggplot living area
ggplot(neighborhoods, aes(fill=neighborhood, y=avg_living_area, x=neighborhood, )) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Living Area by Neighborhood") + 
  scale_fill_manual(values = wes_palette("Darjeeling1")) + 
  labs(y="Average Living Area") + labs(x = "Neighborhood") + 
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank(),
        plot.background = element_rect(colour = "black",size = 1),
        plot.title = element_text(size=22, hjust = 0.5)) 




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


