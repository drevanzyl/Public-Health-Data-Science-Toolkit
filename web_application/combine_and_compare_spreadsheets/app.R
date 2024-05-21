# # Function to install and load required packages
install_load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# # List of required packages
packages <- c("shiny", "readxl", "dplyr", "janitor", "openxlsx", "flextable",
              "gtsummary", "here", "fs", "rpivotTable", "DT", "stringr",
              "tidyr", "snakecase", "purrr")

# Install and load packages
install_load_packages(packages)



# Define the user interface
ui <- fluidPage(
  titlePanel("Compare and Combine Spreadsheets"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose Excel Files", multiple = TRUE, accept = ".xlsx"),
      selectInput("sheetName", "Select Sheet Name", choices = NULL),
      textOutput("missingSheets"),
      actionButton("process", "Process Files"),
      downloadButton("dataXLSX", "Download Combined XLSX")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview", DTOutput("filesTable")),
        tabPanel("Column Names", DTOutput("columnNamesTable")),
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
    req(input$files, input$sheetName)
    filePaths <- input$files$datapath
    fileNames <- input$files$name
    
    # Store names of files missing the selected sheet
    missing <- c()
    
    # Combine data from all uploaded files that contain the selected sheet
    filesList <- lapply(seq_along(filePaths), function(i) {
      if (input$sheetName %in% excel_sheets(filePaths[i])) {
        read_excel(filePaths[i], sheet = input$sheetName, col_types = "text") %>%
          clean_names() %>%
          mutate(`Source File` = fileNames[i])
      } else {
        missing <<- c(missing, fileNames[i])
        NULL
      }
    }) %>% bind_rows() %>%
      select(`Source File`, everything()) %>%
      mutate_all(toupper)
    
    list(data = filesList, missing = missing)
  })
  
  # Generate a table of column names and their occurrences
  columnNamesData <- reactive({
    req(input$files, input$sheetName)
    fileNames <- input$files$name
    filePaths <- input$files$datapath
    
    # Extract and clean column names for each file
    columnNamesData <- map_dfr(seq_along(filePaths), ~ {
      if (!is.na(filePaths[.x])) {
        sheets <- excel_sheets(filePaths[.x])
        if (input$sheetName %in% sheets) {
          clean_names(read_excel(filePaths[.x], sheet = input$sheetName, col_types = "text", col_names = FALSE, n_max = 1)) %>%
            mutate(`Source File` = fileNames[.x])
        } else {
          NULL
        }
      }
    }) %>%
      pivot_longer(cols = contains("x"), names_to = "Column Position", values_to = "Column Name") %>%
      mutate(`Column Name` = to_snake_case(coalesce(`Column Name`, paste(`Source File`, "_", `Column Position`)), unique_sep = NULL)) %>%
      select(-`Column Position`)
    
    # Calculate frequency of each column name across all files
    columnNamesCount <- columnNamesData %>%
      group_by(`Column Name`) %>%
      summarise(Frequency = n(), .groups = 'drop')
    
    # Join frequency data back to the original detailed list
    columnNamesData <- left_join(columnNamesData, columnNamesCount, by = "Column Name")
    
    columnNamesData
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
  
  # Interactive pivot table for the combined data
  output$pivotTableOutput <- renderRpivotTable({
    req(processedData())
    rpivotTable(processedData()$data)
  })
  
  # Display filenames of workbooks missing the selected sheet
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
      paste("data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(list(ProcessedData = processedData()$data, ColumnNamesData = columnNamesData()), file, asTable = TRUE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
