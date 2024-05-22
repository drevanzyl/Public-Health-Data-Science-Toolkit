
# Function to install and load required packages
install_load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# List of required packages
packages <- c("shiny", "readxl", "dplyr", "janitor", "openxlsx", "flextable",
              "gtsummary", "here", "fs", "rpivotTable", "DT", "stringr",
              "tidyr", "snakecase", "purrr", "tibble")

# Install and load packages
install_load_packages(packages)

# Define the user interface
ui <- fluidPage(
  titlePanel("Compare and Combine Spreadsheets"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      fileInput("files", "Choose Excel Files", multiple = TRUE, accept = ".xlsx"),
      selectInput("sheetName", "Select Sheet Name", choices = NULL),
      numericInput("headerRow", "Header Row", value = 1, min = 1),
      actionButton("process", "Process Files"),
      downloadButton("dataXLSX", "Download Combined XLSX"),
      textOutput("missingSheets")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About this App",
                 h3("Overview"),
                 p("This app allows you to compare and combine multiple Excel spreadsheets. You can upload multiple files, select a specific sheet, and process the data to see an overview of the combined data, column names, and any missing sheets."),
                 h3("Instructions"),
                 tags$ol(
                   tags$li("Upload the Excel files by clicking 'Choose Excel Files' and selecting the files from your computer."),
                   tags$li("Select the sheet name you want to compare and combine using the 'Select Sheet Name' dropdown."),
                   tags$li("Specify the header row number using the 'Header Row' numeric input."),
                   tags$li("Click 'Process Files' to process the uploaded files and combine the data."),
                   tags$li("Download the combined data by clicking 'Download Combined XLSX'.")
                 )
        ),
        tabPanel("Data Overview", DTOutput("filesTable")),
        tabPanel("Column Names", DTOutput("columnNamesTable")),
        tabPanel("Missing Sheets", DTOutput("missingSheetsTable")),
        tabPanel("RPivotTable", rpivotTableOutput("pivotTableOutput"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Update available sheet names based on uploaded files
  observe({
    req(input$files)
    sheets <- unique(unlist(lapply(input$files$datapath, excel_sheets)))
    updateSelectInput(session, "sheetName", choices = sheets)
  })
  
  # Process the data when the 'Process Files' button is clicked
  processedData <- eventReactive(input$process, {
    req(input$files, input$sheetName, input$headerRow)
    filePaths <- input$files$datapath
    fileNames <- input$files$name
    
    # Store names of files missing the selected sheet
    missing <- c()
    
    # Combine data from all uploaded files that contain the selected sheet
    filesList <- lapply(seq_along(filePaths), function(i) {
      if (input$sheetName %in% excel_sheets(filePaths[i])) {
        read_excel(filePaths[i], sheet = input$sheetName, skip = input$headerRow - 1) %>%
          rename_with(~ ifelse(grepl("^\\.\\.\\.", .), sub("^\\.\\.\\.", "x_", .), .), everything()) %>%
          clean_names() %>%
          mutate(`Source File` = fileNames[i])
      } else {
        missing <<- c(missing, fileNames[i])
        NULL
      }
    }) %>% 
      discard(is.null) %>%
      map(~ mutate_all(.x, as.character)) %>%
      bind_rows() %>%
      select(`Source File`, everything())
    
    list(data = filesList, missing = missing)
  })
  
  # Generate a table of column names and their occurrences when the 'Process Files' button is clicked
  columnNamesData <- eventReactive(input$process, {
    req(input$files, input$sheetName, input$headerRow)
    filePaths <- input$files$datapath
    fileNames <- input$files$name
    
    # Combine data from all uploaded files that contain the selected sheet
    filesList <- lapply(seq_along(filePaths), function(i) {
      if (input$sheetName %in% excel_sheets(filePaths[i])) {
        data <- read_excel(filePaths[i], sheet = input$sheetName, skip = input$headerRow - 1) %>%
          rename_with(~ ifelse(grepl("^\\.\\.\\.", .), sub("^\\.\\.\\.", "x_", .), .), everything()) %>%
          clean_names() %>%
          mutate(`Source File` = fileNames[i])
        
        tibble(
          `Source File` = fileNames[i],
          `Column_Name` = names(data)
        )
      } else {
        NULL
      }
    }) %>%
      discard(is.null) %>%
      bind_rows()
    
    # Calculate frequency of each column name across all files
    columnNamesCount <- filesList %>%
      group_by(`Column_Name`) %>%
      summarise(Frequency = n(), .groups = 'drop')
    
    # Join frequency data back to the original detailed list
    columnNamesData <- left_join(filesList, columnNamesCount, by = "Column_Name") %>% 
      arrange(Frequency) %>%
      filter(Column_Name != "Source File")
    
    columnNamesData
  })
  
  # Create a reactive expression for the missing sheets data
  missingSheetsData <- eventReactive(input$process, {
    req(processedData())
    missing <- processedData()$missing
    if (length(missing) > 0) {
      tibble(Missing_Files = missing)
    } else {
      tibble(Missing_Files = "All files contain the selected sheet.")
    }
  })
  
  # Render the processed data table in the UI using DT::renderDataTable
  output$filesTable <- renderDT({
    req(processedData())
    datatable(processedData()$data, options = list(autoWidth = TRUE))
  })
  
  # Render the column names table in the UI using DT::renderDataTable
  output$columnNamesTable <- renderDT({
    req(columnNamesData())
    datatable(columnNamesData(), options = list(autoWidth = TRUE))
  })
  
  # Render the missing sheets table in the UI using DT::renderDataTable
  output$missingSheetsTable <- renderDT({
    req(missingSheetsData())
    datatable(missingSheetsData(), options = list(autoWidth = TRUE))
  })
  
  # Interactive pivot table for the combined data
  output$pivotTableOutput <- renderRpivotTable({
    req(processedData())
    rpivotTable(processedData()$data)
  })
  
  # Display filenames of workbooks missing the selected sheet in a text output
  output$missingSheets <- renderText({
    req(processedData())
    if (length(processedData()$missing) > 0) {
      paste("Missing sheet in files:", paste(processedData()$missing, collapse = ", "))
    } else {
      "All files contain the selected sheet."
    }
  })
  
  # Handle the download of the combined data as an Excel file
  output$dataXLSX <- downloadHandler(
    filename = function() {
      paste("data-", input$sheetName, "-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(list(
        ProcessedData = processedData()$data, 
        ColumnNamesData = columnNamesData(),
        MissingSheetsData = missingSheetsData()
      ), file, asTable = TRUE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
