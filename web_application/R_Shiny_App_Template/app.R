# Load necessary libraries
library(shiny)
library(readr) # For reading CSV files
library(dplyr) # For data manipulation
library(openxlsx) # For Excel file operations
library(DT) # For rendering DataTables in Shiny
library(pivottabler) # For creating pivot tables
library(ggplot2) # For data visualization
library(rmarkdown) # For rendering dynamic reports
library(gtsummary) # For creating summary tables
library(gt) # For formatting tables
library(rpivotTable) 

# Define the UI
ui <- fluidPage(
        titlePanel("Leading Causes of Death in the United States"),
        sidebarLayout(
                sidebarPanel(
                        # Input: Choose one or more CSV files
                        fileInput("fileInput", "Choose CSV File",
                                  multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        # Button: Load data
                        actionButton("loadData", "Load Data"),
                        # Button: Download data summary as Excel
                        downloadButton("downloadData", "Download Excel"),
                        # Button: Download report as Word document
                        downloadButton("downloadReport", "Download Report")
                ),
                mainPanel(
                        # Tabbed layout to display different sections of the app
                        tabsetPanel(
                                tabPanel("Data Table", DTOutput("dataTable")),
                                tabPanel("Pivot Table",
                                         selectInput("rowVar", "Row Variable", choices = NULL),
                                         selectInput("colVar", "Column Variable", choices = NULL),
                                         selectInput("valVar", "Value Variable", choices = c("Deaths", "Age-adjusted Death Rate")),
                                         actionButton("createPivot", "Create Pivot Table"),
                                         htmlOutput("pivotTable")),
                                tabPanel("Visualizations", plotOutput("deathsPlot")),
                                tabPanel("Summary Table", htmlOutput("summaryTable")),
                                tabPanel("Indicators", DTOutput("summaryIndicatorsTable")),
                                tabPanel("RPivotTable", rpivotTableOutput("pivotTableOutput"))
                                
                        )
                )
        )
)

# Define server logic
server <- function(input, output, session) {
        # Reactive value to store the processed data
        data <- reactiveVal()
        
        # Observe event for Load Data button
        observeEvent(input$loadData, {
                req(input$fileInput) # Ensure file is selected
                
                # Read and process the input files
                filePaths <- input$fileInput$datapath
                allData <- lapply(filePaths, read_csv) %>%
                        bind_rows() %>%
                        distinct() %>%
                        # Categorize states into regions
                        mutate(Regions = case_when(
                                State %in% c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'New Jersey', 'New York', 'Pennsylvania', 'Rhode Island', 'Vermont') ~ 'Northeast',
                                State %in% c('Illinois', 'Indiana', 'Iowa', 'Kansas', 'Michigan', 'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'Ohio', 'South Dakota', 'Wisconsin') ~ 'Midwest',
                                State %in% c('Alabama', 'Arkansas', 'Delaware', 'Florida', 'Georgia', 'Kentucky', 'Louisiana', 'Maryland', 'Mississippi', 'North Carolina', 'Oklahoma', 'South Carolina', 'Tennessee', 'Texas', 'Virginia', 'West Virginia') ~ 'South',
                                State %in% c('Alaska', 'Arizona', 'California', 'Colorado', 'Hawaii', 'Idaho', 'Montana', 'Nevada', 'New Mexico', 'Oregon', 'Utah', 'Washington', 'Wyoming') ~ 'West',
                                TRUE ~ NA_character_
                        ))
                
                # Store the processed data
                data(allData)
                
                # Update choices for selectInput based on the columns of the loaded data
                updateSelectInput(session, "rowVar", choices = names(allData))
                updateSelectInput(session, "colVar", choices = names(allData))
        })
        
        # Render the main data table
        output$dataTable <- renderDT({
                datatable(data(), options = list(pageLength = 5, autoWidth = TRUE))
        }, server = FALSE)
        
        # Handle Excel file download
        output$downloadData <- downloadHandler(
                filename = function() {
                        paste0("data-summary-", Sys.Date(), ".xlsx")
                },
                content = function(file) {
                        wb <- createWorkbook()
                        addWorksheet(wb, "Main Data")
                        writeData(wb, "Main Data", data())
                        
                        # Prepare and add summary indicators data as a new sheet
                        summary_data <- data() %>%
                                group_by(Regions) %>%
                                summarise(TotalDeaths = sum(Deaths, na.rm = TRUE),
                                          AvgAgeAdjustedDeathRate = mean(`Age-adjusted Death Rate`, na.rm = TRUE)) %>%
                                ungroup() %>%
                                arrange(desc(TotalDeaths))
                        addWorksheet(wb, "Summary Indicators")
                        writeData(wb, "Summary Indicators", summary_data)
                        
                        saveWorkbook(wb, file, overwrite = TRUE)
                }
        )
        
        # Render pivot table UI upon request
        output$pivotTable <- renderUI({
                req(input$createPivot, data())
                pt <- PivotTable$new()
                pt$addData(data())
                pt$addColumnDataGroups(input$colVar)
                pt$addRowDataGroups(input$rowVar)
                # Define summary calculation for the pivot table
                pt$defineCalculation(calculationName = "Total", summariseExpression = paste0("sum(", input$valVar, ")"))
                pt$evaluatePivot()
                pivottabler(pt)
        })
        
        # Render plot for deaths by region over time
        output$deathsPlot <- renderPlot({
                req(data()) # Ensure data is loaded
                # Summarize and plot data
                data_summarized <- data() %>%
                        group_by(Year, Regions) %>%
                        summarise(TotalDeaths = sum(Deaths, na.rm = TRUE)) %>%
                        ungroup()
                ggplot(data_summarized, aes(x = Year, y = TotalDeaths, color = Regions, group = Regions)) +
                        geom_line() + geom_point() + theme_minimal() +
                        labs(title = "Total Deaths by Region Over Time", x = "Year", y = "Total Deaths", color = "Region") +
                        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
        })
        
        # Render summary table UI
        output$summaryTable <- renderUI({
                req(data()) # Ensure data is available
                summary_tbl <- data() %>%
                        select(Year, Regions, Deaths) %>%
                        tbl_summary(by = Regions, type = list(Year ~ "continuous", Deaths ~ "continuous"), missing = "no") %>%
                        as_gt() 
        })
        
        # Handle dynamic report download
        output$downloadReport <- downloadHandler(
                filename = function() {
                        paste("dynamic-report-", Sys.Date(), ".docx", sep="")
                },
                content = function(file) {
                        # Render R Markdown document, passing summarized data and full dataset as parameters
                        rmarkdown::render("report_template.Rmd",
                                          output_file = file,
                                          params = list(plot_data = data(), all_data = data()))
                }
        )
        
        # Render summary indicators data table
        output$summaryIndicatorsTable <- renderDataTable({
                req(data()) # Ensure data is loaded
                summary_data <- data() %>%
                        group_by(Regions) %>%
                        summarise(TotalDeaths = sum(Deaths, na.rm = TRUE),
                                  AvgAgeAdjustedDeathRate = mean(`Age-adjusted Death Rate`, na.rm = TRUE)) %>%
                        ungroup() %>%
                        arrange(desc(TotalDeaths))
                datatable(summary_data, options = list(pageLength = 10, autoWidth = TRUE))
        })
        output$pivotTableOutput <- renderRpivotTable({
                req(data())  # Ensure data is available
                rpivotTable(data())
        })
        
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
