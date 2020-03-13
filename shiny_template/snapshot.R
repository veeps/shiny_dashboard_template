library(ggplot2)
library(tibble)
library(magrittr)
library(tidyverse)
library(dplyr)
library(scales)
library(stringr)
library(wesanderson)
library(plotly)

#read in data file
df <- read.csv("data/train_clean.csv") %>%
  as_data_frame()


max(df$year_built)

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



fig <- plot_ly(data = df, x = ~year_built, y = ~saleprice,
               marker = list(size = 5),
               text = ~paste("Price: ", saleprice, 'Year Built:', year_built),
               color= ~neighborhood,
               colors="Set1",
               alpha=0.7)

f <- list(
  family = "Roboto-Bold",
  size = 18,
  color = "#000000"
)
x <- list(
  title = "Year Built",
  titlefont = f
)
y <- list(
  title = "Sale Price",
  titlefont = f
)



fig %>% layout(showlegend = FALSE ,xaxis = x, yaxis = y) 

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


