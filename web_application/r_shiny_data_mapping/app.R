library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(openxlsx)

# Define the apply_data_mapping function here

apply_data_mapping <- function(original_data_path, mapping_path) {
        # Load the mapping rules from the Excel file
        mapping_df <- read_excel(mapping_path)
        
        # Fill NA in 'Field' to identify mappings that span multiple rows
        mapping_df <- mapping_df %>% fill(Field, .direction = "down")
        
        # For New Field Name, where NA appears, it should inherit the first New Field name
        mapping_df <- mapping_df %>% fill(`New Field`, .direction = "down")
        
        # Prepare the structure for value mappings
        value_mappings <- list()
        for(i in 1:nrow(mapping_df)) {
                field <- mapping_df$Field[i]
                new_field <- mapping_df$`New Field`[i]
                original_value <- mapping_df$`Range/Values`[i]
                new_value <- mapping_df$`New Range/Values`[i]
                if(is.null(value_mappings[[new_field]])) {
                        value_mappings[[new_field]] <- list(original_field = field, value_mapping = list())
                }
                if(!is.na(original_value) && !is.na(new_value)) {
                        value_mappings[[new_field]]$value_mapping[[as.character(original_value)]] <- new_value
                }
        }
        
        # Load the original data
        original_data_df <- read_csv(original_data_path, col_types = cols(.default = "c"))
        
        # Apply the mappings
        transformed_data_df <- original_data_df
        for(new_field in names(value_mappings)) {
                mapping <- value_mappings[[new_field]]
                original_field <- mapping$original_field
                if(original_field %in% colnames(transformed_data_df)) {
                        value_mapping <- mapping$value_mapping
                        transformed_data_df[[new_field]] <- transformed_data_df[[original_field]]
                        if(length(value_mapping) > 0) {
                                for(original_value in names(value_mapping)) {
                                        new_value <- value_mapping[[original_value]]
                                        transformed_data_df[[new_field]][transformed_data_df[[original_field]] == original_value] <- new_value
                                }
                        }
                }
        }
        
        # Determine column order
        column_order <- unique(c(unlist(lapply(names(value_mappings), function(x) c(value_mappings[[x]]$original_field, x))), names(original_data_df)))
        
        # Rearrange and return the dataframe
        rearranged_df <- transformed_data_df[, column_order, drop = FALSE]
        return(rearranged_df)
}


ui <- fluidPage(
        titlePanel("Data Mapping Application"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("dataFile", "Choose CSV Data File",
                                  accept = c(".csv")),
                        fileInput("mappingFile", "Choose Mapping Excel File",
                                  accept = c(".xlsx", ".xls")),
                        downloadButton("downloadData", "Download Mapped Data")
                ),
                mainPanel(
                        # Optional: Add any output components here
                )
        )
)

server <- function(input, output) {
        # Reactively read the original data and mapping files when new files are uploaded
        reactiveData <- reactive({
                validate(
                        need(input$dataFile, "Please upload the data file."),
                        need(input$mappingFile, "Please upload the mapping file.")
                )
                apply_data_mapping(input$dataFile$datapath, input$mappingFile$datapath)
        })
        
        # Generate the download file
        output$downloadData <- downloadHandler(
                filename = function() { "mapped_data.xlsx" },
                content = function(file) {
                        mappedData <- reactiveData()
                        write.xlsx(mappedData, file)
                }
        )
}

# Run the application
shinyApp(ui = ui, server = server)
