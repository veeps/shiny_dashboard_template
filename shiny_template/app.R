library(shiny)
library(tidyverse)
library(scales)
library(DT)


# Define UI ----
ui <- fluidPage(div(style="padding-left: 30px; padding-right: 30px", 
  titlePanel("Housing Prices in Ames, Iowa"),
  titlePanel(h3("Housing sales by neighborhood")),
  p("For this project, I used the housing data from the Ames Assessor’s Office used to appraise the value for individual properties in 2006 to 2010. The data was split into training and 
              testing data for the Kaggle competition. The data includes 81 features of each housing sale, including some ordinal (quality ratings), some nominal (classification of neighborhood, zone, sale type), 
              and numeric (square feet, year built). The source for the Kaggle data is here."),
  includeCSS("style.css"),
  
  
  fluidRow (div(style="padding: 20px",
    dataTableOutput("table_joe"))
  ),
  
  # Define the sidebar with one input
  sidebarPanel(
    selectInput(inputId="var_joe",
                label = "Select a Variable",
                choices=c("Living Area", "Year Built", "Quality", "Price", "Total Sales")),
    fluidRow(h5(span("1,200")),
             h6(span("Total homes sold"))),
  ),
  
  
  mainPanel(
          plotOutput("plot1_joe"))
))


# Define server logic ----
server <- function(input, output) {
  
  #read in data file
  df <- read.csv("data/train_clean.csv") %>%
    as_data_frame()
  
  
  #get summary table by content type
  neighborhoods <- df %>%
    group_by(neighborhood) %>%
    summarise(
      avg_quality=as.integer(mean(overall_qual)),
      avg_year_built =as.integer(mean(year_built)),
      avg_living_area =as.integer(mean(gr_liv_area)),
      avg_price =as.integer(mean(saleprice)),
      total_sales = n()
    ) %>%   filter(total_sales > 122) %>%
    arrange(-total_sales)
  
  
  
 
  #render data table
  output$table_joe <- renderDataTable(neighborhoods, options=list(info = FALSE, paging = FALSE, searching = FALSE))
  
  #reactive axis and labels
  yaxis1_joe <- reactive({
    if ( "Living Area" %in% input$var_joe) return(neighborhoods$avg_living_area)
    if ( "Quality" %in% input$var_joe) return(neighborhoods$avg_quality)
    if ( "Year Built" %in% input$var_joe) return(neighborhoods$avg_year_built)
    if ( "Price" %in% input$var_joe) return(neighborhoods$avg_price)
    if ( "Total Sales" %in% input$var_joe) return(neighborhoods$total_sales)
  })
  

  graph_title <- reactive({
    if ( "Living Area" %in% input$var_joe) return("Average Living Area per Neighborhood")
    if ( "Quality" %in% input$var_joe) return("Average Quality Area per Neighborhood")
    if ( "Year Built" %in% input$var_joe) return("Average Year Built per Neighborhood")
    if ( "Price" %in% input$var_joe) return("Average Selling Price per Neighborhood")
    if ( "Total Sales" %in% input$var_joe) return("Total Sales per Neighborhood")
  })

  y_label <- reactive({
    if ( "Living Area" %in% input$var_joe) return("Average Living Area")
    if ( "Quality" %in% input$var_joe) return("Average Quality Area")
    if ( "Year Built" %in% input$var_joe) return("Average Year Built")
    if ( "Price" %in% input$var_joe) return("Average Selling Price")
    if ( "Total Sales" %in% input$var_joe) return("Total Sales")
  })

  
  output$plot1_joe <- renderPlot({
    
    # Render a barplot for content summary
    ggplot(neighborhoods, aes(fill=neighborhood, y=yaxis1_joe(), x=neighborhood)) + 
      geom_bar(position="dodge", stat="identity") + ggtitle(graph_title()) + 
      scale_fill_manual(values = wes_palette("Darjeeling2")) + 
      labs(y=y_label()) + labs(x = "Neighborhood") + 
      theme(axis.text.x = element_text(angle = 45),
            legend.title = element_blank(), 
            plot.background = element_rect(colour = "black",size = 1),
            plot.title = element_text(size=22, hjust = 0.5)) 
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)