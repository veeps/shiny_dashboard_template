library(shiny)
library(ggplot2)
library(tibble)
library(magrittr)
library(tidyverse)
library(dplyr)
library(scales)
library(DT)


# Define UI ----
ui <- fluidPage(div(style="padding-left: 30px", 
  titlePanel("EDA Dashboard")),
  
  fluidRow (div(style="padding: 50px",
    dataTableOutput("table_joe"))
  ),
  
  # Define the sidebar with one input
  sidebarPanel(
    selectInput(inputId="var_joe",
                label = "Variable",
                choices=c("Avg Engagements", "Avg Engagement Rate", "Avg Clicks", "Total Engagements", "Total Clicks"))
  ),
  
  mainPanel(plotOutput("plot1_joe"),
            plotOutput("plot2_joe"))
)


# Define server logic ----
server <- function(input, output) {
  
  #read in data file
  tweets_joe <- read.csv("data/joe_tweets.csv") %>%
    as_data_frame()
  
  #get summary table by content type
  Summary_Content_Joe <- tweets_joe %>%
    group_by(Content) %>%
    summarise(
      Avg.engagements =as.integer(mean(engagements)),
      Avg.engagement.rate=percent(mean(engagement.rate)),
      Avg.clicks =as.integer(mean(url.clicks)),
      Total.engagements=sum(engagements),
      Total.clicks=sum(url.clicks)
    ) %>%
    arrange(-Avg.clicks)
  
  
  
  #get summary table for challenge posts
  averages_Challenges_Joe <- tweets_joe %>%
    filter(Content=="Challenge") %>%
    group_by(Type) %>%
    summarise(
      Content="Challenge",
      Avg.engagements =as.integer(mean(engagements)),
      Avg.engagement.rate=percent(mean(engagement.rate)),
      Avg.clicks =as.integer(mean(url.clicks)),
      Total.engagements=sum(engagements),
      Total.clicks=sum(url.clicks)
    )
  
  #get summary table for inspiration posts
  averages_Inspiration_Joe <- tweets_joe %>%
    filter(Content=="Inspiration") %>%
    group_by(Type) %>%
    summarise(
      Content="Inspiration",
      Avg.engagements =as.integer(mean(engagements)),
      Avg.engagement.rate=percent(mean(engagement.rate)),
      Avg.clicks =as.integer(mean(url.clicks)),
      Total.engagements=sum(engagements),
      Total.clicks=sum(url.clicks)
    )
  
  #combine tables by type
  averages_Type_Joe <- rbind(averages_Challenges_Joe,averages_Inspiration_Joe)
  
  #render data table
  output$table_joe <- renderDataTable(Summary_Content_Joe, options=list(info = FALSE, paging = FALSE, searching = FALSE))
  
  #reactive axis and labels
  yaxis1_joe <- reactive({
    if ( "Avg Engagements" %in% input$var_joe) return(Summary_Content_Joe$Avg.engagements)
    if ( "Avg Engagement Rate" %in% input$var_joe) return(Summary_Content_Joe$Avg.engagement.rate)
    if ( "Avg Clicks" %in% input$var_joe) return(Summary_Content_Joe$Avg.clicks)
    if ( "Total Engagements" %in% input$var_joe) return(Summary_Content_Joe$Total.engagements)
    if ( "Total Clicks" %in% input$var_joe) return(Summary_Content_Joe$Total.clicks)
  })
  
  yaxis2_joe <- reactive({
    if ( "Avg Engagements" %in% input$var_joe) return(averages_Type_Joe$Avg.engagements)
    if ( "Avg Engagement Rate" %in% input$var_joe) return(averages_Type_Joe$Avg.engagement.rate)
    if ( "Avg Clicks" %in% input$var_joe) return(averages_Type_Joe$Avg.clicks)
    if ( "Total Engagements" %in% input$var_joe) return(averages_Type_Joe$Total.engagements)
    if ( "Total Clicks" %in% input$var_joe) return(averages_Type_Joe$Total.clicks)
  })
  
  graph_title_joe <- reactive({
    if ( "Avg Engagements" %in% input$var_joe) return("Joe average engagements per tweet")
    if ( "Avg Engagement Rate" %in% input$var_joe) return("Joe average engagement rate per tweet")
    if ( "Avg Clicks" %in% input$var_joe) return("Joe average clicks per tweet")
    if ( "Total Engagements" %in% input$var_joe) return("Joe total engagements per tweet")
    if ( "Total Clicks" %in% input$var_joe) return("Joe total clicks per tweet")
  })
  
  y_label <- reactive({
    if ( "Avg Engagements" %in% input$var_joe) return("Avg engagements per tweet")
    if ( "Avg Engagement Rate" %in% input$var_joe) return("Avg engagement rate per tweet")
    if ( "Avg Clicks" %in% input$var_joe) return("Avg clicks per tweet")
    if ( "Total Engagements" %in% input$var_joe) return("Total engagements per tweet")
    if ( "Total Clicks" %in% input$var_joe) return("Total clicks per tweet")
  })
  
  
  output$plot1_joe <- renderPlot({
    
    # Render a barplot for content summary
    ggplot(Summary_Content_Joe, aes(fill=Content, y=yaxis1_joe(), x=Content)) + 
      geom_bar(position="dodge", stat="identity") +
      labs(title=graph_title_joe(), caption="Data pulled from Twitter Analytics between 12/01/2018-01/31/2019")+
      labs(y=y_label())
  })
  
  
  output$plot2_joe <- renderPlot({
    
    #render barplot by type
    ggplot(averages_Type_Joe, aes(fill=Content, y=yaxis2_joe(), x=Type)) + 
      geom_bar( stat="identity") + ggtitle("HITRECORD average clicks per tweet by post type") +
      labs(title=graph_title_joe(), caption="Data pulled from Twitter Analytics between 12/01/2018-01/31/2019", y=y_label())
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)