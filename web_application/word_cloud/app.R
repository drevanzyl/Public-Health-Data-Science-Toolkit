library(shiny)
library(tm)
library(wordcloud)
library(wordcloud2)
library(openxlsx)
library(plotly)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Word Frequency Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Text or CSV File",
                accept = c(".csv", ".txt")),
      tags$hr(),
      h5("Word cloud and Excel export of word frequencies from the provided text or CSV file."),
      downloadButton("download", "Download Word Frequencies")
    ),
    mainPanel(
      # Increase the output size in the UI definition
      uiOutput("wordCloudPlot", style = "width:150%; height:900px;"),  # Display the word cloud from wordcloud2
      plotlyOutput("freqPlot", height = "900px")  # Display the frequency plot using plotly
      
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive expression to read and process the text
  text_data <- reactive({
    # Ensure a file is uploaded
    if(is.null(input$file1)) return(NULL)
    
    # Read the file
    req(input$file1)
    # Detect if the file is a CSV or plain text and read accordingly
    if(grepl("\\.csv$", input$file1$name)) {
      data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      text <- paste(data[,1], collapse = " ")  # Assumes text is in the first column
    } else {
      text <- readLines(input$file1$datapath, warn = FALSE)
      text <- paste(text, collapse = " ")
    }
    
    # Text processing
    docs <- Corpus(VectorSource(text))
    #docs <- tm_map(docs, content_transformer(function(x) gsub("http\\S+|www\\S+", "", x)))
    docs <- tm_map(docs, content_transformer(function(x) gsub("[[:punct:]]", "", x)))
    docs <- tm_map(docs, content_transformer(function(x) gsub("\\d+", "", x)))
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("en"))
    docs <- tm_map(docs, stripWhitespace)
    
    # Create a term-document matrix
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    data.frame(Word = names(word_freqs), Frequency = word_freqs)
  })
  
  # Render the word cloud
  output$wordCloudPlot <- renderUI({
    req(text_data())  # Ensure that the text data is available
    data <- text_data()  # Retrieve the data frame
  
    # Scale frequency for visual effect

    # Create a wordcloud
    wordcloud2(data, size = 2)
  })
  
  # Output for frequency plot using plotly
  output$freqPlot <- renderPlotly({
    req(text_data())  # Ensure that text data is available
    freq_data <- text_data()  # Get the text data from the reactive expression
    top_words <- head(freq_data[order(-freq_data$Frequency), ], 50)  # Select top 50 words by frequency
    
    # Generate a plotly horizontal bar plot with sorted data
    plot_ly(data = top_words, x = ~Frequency, y = ~fct_reorder(Word, Frequency), type = 'bar', orientation = 'h',
            marker = list(color = ~Frequency,  # Colors bars based on frequency values
                          colorbar = list(title = "Frequency"),  # Adds a color bar legend for scale
                          colorscale = 'virdis')) %>%  # Use the 'Blues' color scale; other options include 'Greens', 'Greys', etc.
      layout(title = "Top 50 Word Frequencies",
             xaxis = list(title = "Frequency"),
             yaxis = list(title = "", autorange = "true"),
             margin = list(l = 150, b = 100),
             hoverinfo = 'x+y')  # Shows frequency and word on hover
  })
  
  
  # Export data to Excel
  output$download <- downloadHandler(
    filename = function() {
      paste("word-frequencies-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(text_data())
      write.xlsx(text_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)

