# Load required libraries
library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(readxl)
library(tools)

# Define UI
ui <- fluidPage(
        tags$head(
                tags$style(HTML("
      .join-table { border-collapse: collapse; margin: 10px; }
      .join-table th, .join-table td { border: 1px solid #ddd; padding: 8px; text-align: left; }
      .join-table th { background-color: #f2f2f2; }
      .highlight { background-color: #ffff99; }
      .join-illustration { display: flex; flex-wrap: wrap; justify-content: center; align-items: flex-start; margin-bottom: 20px; }
      .join-description { margin: 10px; max-width: 300px; }
      .warning-message { color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; padding: 10px; margin: 10px 0; border-radius: 5px; }
    "))
        ),
        titlePanel("Data Join Application"),
        
        sidebarLayout(
                sidebarPanel(
                        fileInput("file1", "Choose File 1", accept = c(".csv", ".xlsx")),
                        uiOutput("sheet1"),
                        fileInput("file2", "Choose File 2", accept = c(".csv", ".xlsx")),
                        uiOutput("sheet2"),
                        
                        uiOutput("join_cols1"),
                        uiOutput("join_cols2"),
                        
                        uiOutput("matched_columns"),
                        
                        selectInput("join_type", "Select Join Type",
                                    choices = c("Inner Join" = "inner_join",
                                                "Left Join" = "left_join",
                                                "Right Join" = "right_join",
                                                "Full Join" = "full_join")),
                        
                        actionButton("perform_join", "Perform Join"),
                        
                        uiOutput("download_ui")
                ),
                
                mainPanel(
                        uiOutput("warning_message"),
                        tabsetPanel(
                                tabPanel("Data 1", DTOutput("data1")),
                                tabPanel("Data 2", DTOutput("data2")),
                                tabPanel("Joined Data", DTOutput("joined_data")),
                                tabPanel("About", uiOutput("about_content"))
                        )
                )
        )
)

# Define server logic
server <- function(input, output, session) {
        
        # Reactive values to store the datasets
        data1 <- reactiveVal()
        data2 <- reactiveVal()
        
        # Function to read file based on its extension
        read_file <- function(file, sheet = NULL) {
                ext <- tolower(file_ext(file$datapath))
                if (ext == "csv") {
                        read.csv(file$datapath)
                } else if (ext == "xlsx") {
                        read_excel(file$datapath, sheet = sheet)
                }
        }
        
        # Generate UI for sheet selection for file 1
        output$sheet1 <- renderUI({
                req(input$file1)
                if (tolower(file_ext(input$file1$name)) == "xlsx") {
                        sheets <- excel_sheets(input$file1$datapath)
                        selectInput("sheet1", "Select Sheet for File 1", choices = sheets)
                }
        })
        
        # Generate UI for sheet selection for file 2
        output$sheet2 <- renderUI({
                req(input$file2)
                if (tolower(file_ext(input$file2$name)) == "xlsx") {
                        sheets <- excel_sheets(input$file2$datapath)
                        selectInput("sheet2", "Select Sheet for File 2", choices = sheets)
                }
        })
        
        # Read and store the uploaded files
        observeEvent(input$file1, {
                req(input$file1)
                if (tolower(file_ext(input$file1$name)) == "xlsx") {
                        # For Excel files, wait for sheet selection
                        return()
                }
                data1(read_file(input$file1))
        })
        
        observeEvent(input$sheet1, {
                req(input$file1, input$sheet1)
                data1(read_file(input$file1, input$sheet1))
        })
        
        observeEvent(input$file2, {
                req(input$file2)
                if (tolower(file_ext(input$file2$name)) == "xlsx") {
                        # For Excel files, wait for sheet selection
                        return()
                }
                data2(read_file(input$file2))
        })
        
        observeEvent(input$sheet2, {
                req(input$file2, input$sheet2)
                data2(read_file(input$file2, input$sheet2))
        })
        
        # Generate UI for selecting join columns
        output$join_cols1 <- renderUI({
                req(data1())
                virtualSelectInput(
                        inputId = "cols1",
                        label = "Select Join Columns for Data 1",
                        choices = names(data1()),
                        multiple = TRUE,
                        search = TRUE,
                        placeholder = "Select columns"
                )
        })
        
        output$join_cols2 <- renderUI({
                req(data2())
                virtualSelectInput(
                        inputId = "cols2",
                        label = "Select Join Columns for Data 2",
                        choices = names(data2()),
                        multiple = TRUE,
                        search = TRUE,
                        placeholder = "Select columns"
                )
        })
        
        # Display matched columns
        output$matched_columns <- renderUI({
                req(input$cols1, input$cols2)
                if (length(input$cols1) != length(input$cols2)) {
                        return(HTML("<p style='color: red;'>Number of selected columns must match for both datasets</p>"))
                }
                matched <- data.frame(
                        Data1 = input$cols1,
                        Data2 = input$cols2
                )
                HTML(
                        "<h4>Matched Columns:</h4>",
                        paste(
                                sapply(1:nrow(matched), function(i) {
                                        sprintf("<p>%s &#8596; %s</p>", matched$Data1[i], matched$Data2[i])
                                }),
                                collapse = ""
                        )
                )
        })
        
        # Display the uploaded datasets
        output$data1 <- renderDT({
                req(data1())
                datatable(data1())
        })
        
        output$data2 <- renderDT({
                req(data2())
                datatable(data2())
        })
        
        # Reactive value to store warning messages
        warning_message <- reactiveVal(NULL)
        
        # Perform the join operation
        joined_data <- eventReactive(input$perform_join, {
                req(data1(), data2(), input$cols1, input$cols2, input$join_type)
                
                # Validate that the number of selected columns match
                validate(
                        need(length(input$cols1) == length(input$cols2), 
                             "The number of selected columns must be the same for both datasets.")
                )
                
                join_func <- switch(input$join_type,
                                    "inner_join" = inner_join,
                                    "left_join" = left_join,
                                    "right_join" = right_join,
                                    "full_join" = full_join)
                
                # Create a named vector for the join columns
                join_by <- setNames(input$cols2, input$cols1)
                
                # Capture warnings during join operation
                result <- withCallingHandlers(
                        join_func(data1(), data2(), by = join_by),
                        warning = function(w) {
                                warning_message(paste("Warning:", w$message))
                                invokeRestart("muffleWarning")
                        }
                )
                
                result
        })
        
        # Display warning message if any
        output$warning_message <- renderUI({
                warn <- warning_message()
                if (!is.null(warn)) {
                        div(class = "warning-message", 
                            icon("exclamation-triangle"), 
                            tags$b("Join Warning: "), 
                            warn)
                }
        })
        
        # Display the joined data
        output$joined_data <- renderDT({
                req(joined_data())
                datatable(joined_data())
        })
        
        # Generate download button UI
        output$download_ui <- renderUI({
                req(joined_data())
                downloadButton("download_data", "Download Joined Data")
        })
        
        # Download handler for joined data
        output$download_data <- downloadHandler(
                filename = function() {
                        paste("joined_data_", Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                        write.csv(joined_data(), file, row.names = FALSE)
                }
        )
        
        # Add content for the About tab
        output$about_content <- renderUI({
                HTML(
                        "<h2>About This Application</h2>
      <p>This Data Join Application allows you to join two datasets based on common columns. It supports CSV and Excel file formats and provides various join types.</p>
      
      <h3>How to Use</h3>
      <ol>
        <li>Upload two datasets (CSV or Excel files).</li>
        <li>If using Excel, select the desired sheet for each file.</li>
        <li>Choose the columns to join on for each dataset.</li>
        <li>Select the type of join you want to perform.</li>
        <li>Click 'Perform Join' to see the results.</li>
        <li>Download the joined data as a CSV file if needed.</li>
      </ol>
      
      <h3>Join Types Explained</h3>
      
      <div class='join-illustration'>
        <div class='join-description'>
          <h4>Inner Join</h4>
          <p>Returns only the rows that have matching values in both datasets.</p>
        </div>
        <div>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th></tr>
            <tr><td>1</td><td>Alice</td></tr>
            <tr class='highlight'><td>2</td><td>Bob</td></tr>
            <tr class='highlight'><td>3</td><td>Charlie</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Age</th></tr>
            <tr class='highlight'><td>2</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>30</td></tr>
            <tr><td>4</td><td>35</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th><th>Age</th></tr>
            <tr class='highlight'><td>2</td><td>Bob</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>Charlie</td><td>30</td></tr>
          </table>
        </div>
      </div>

      <div class='join-illustration'>
        <div class='join-description'>
          <h4>Left Join</h4>
          <p>Returns all rows from the left dataset, and the matched rows from the right dataset.</p>
        </div>
        <div>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th></tr>
            <tr class='highlight'><td>1</td><td>Alice</td></tr>
            <tr class='highlight'><td>2</td><td>Bob</td></tr>
            <tr class='highlight'><td>3</td><td>Charlie</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Age</th></tr>
            <tr><td>2</td><td>25</td></tr>
            <tr><td>3</td><td>30</td></tr>
            <tr><td>4</td><td>35</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th><th>Age</th></tr>
            <tr class='highlight'><td>1</td><td>Alice</td><td>NULL</td></tr>
            <tr class='highlight'><td>2</td><td>Bob</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>Charlie</td><td>30</td></tr>
          </table>
        </div>
      </div>

      <div class='join-illustration'>
        <div class='join-description'>
          <h4>Right Join</h4>
          <p>Returns all rows from the right dataset, and the matched rows from the left dataset.</p>
        </div>
        <div>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th></tr>
            <tr><td>1</td><td>Alice</td></tr>
            <tr><td>2</td><td>Bob</td></tr>
            <tr><td>3</td><td>Charlie</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Age</th></tr>
            <tr class='highlight'><td>2</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>30</td></tr>
            <tr class='highlight'><td>4</td><td>35</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th><th>Age</th></tr>
            <tr class='highlight'><td>2</td><td>Bob</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>Charlie</td><td>30</td></tr>
            <tr class='highlight'><td>4</td><td>NULL</td><td>35</td></tr>
          </table>
        </div>
      </div>

      <div class='join-illustration'>
        <div class='join-description'>
          <h4>Full Join</h4>
          <p>Returns all rows when there is a match in either left or right dataset.</p>
        </div>
        <div>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th></tr>
            <tr class='highlight'><td>1</td><td>Alice</td></tr>
            <tr class='highlight'><td>2</td><td>Bob</td></tr>
            <tr class='highlight'><td>3</td><td>Charlie</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Age</th></tr>
            <tr class='highlight'><td>2</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>30</td></tr>
            <tr class='highlight'><td>4</t
            <table class='join-table'>
            <tr><th>ID</th><th>Age</th></tr>
            <tr class='highlight'><td>2</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>30</td></tr>
            <tr class='highlight'><td>4</td><td>35</td></tr>
          </table>
          <table class='join-table'>
            <tr><th>ID</th><th>Name</th><th>Age</th></tr>
            <tr class='highlight'><td>1</td><td>Alice</td><td>NULL</td></tr>
            <tr class='highlight'><td>2</td><td>Bob</td><td>25</td></tr>
            <tr class='highlight'><td>3</td><td>Charlie</td><td>30</td></tr>
            <tr class='highlight'><td>4</td><td>NULL</td><td>35</td></tr>
          </table>
        </div>
      </div>
      "
                )
        })
}

# Run the application
shinyApp(ui = ui, server = server)