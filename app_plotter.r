library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)

ui <- fluidPage(
  titlePanel("Dynamic Threshold Checker"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting the sample
      selectInput("sample_selection", "Choose Sample",
                  choices = c("Sample 2" = "2", "Sample 3" = "3", "Sample 4" = "4", 
                              "Sample 5" = "5", "Sample 6" = "6", "Sample 7" = "7")),
      
      sliderInput("minor_threshold_x", "Minor: Minor Upper Threshold",
                  min = 0, max = 1.5, value = 0.7, step = 0.1),
      sliderInput("minor_threshold_y", "Minor: Major Lower Threshold",
                  min = 0, max = 1.5, value = 1, step = 0.1),
      sliderInput("major_threshold_y", "Major: Major Upper Threshold",
                  min = 0, max = 1.5, value = 0.5, step = 0.1),
      sliderInput("major_threshold_x", "Major: Minor Lower Threshold",
                  min = 0, max = 1.5, value = 1, step = 0.1),
      
      # Checkbox for toggling gene names
      checkboxInput("show_gene_names", "Show Gene Names", FALSE)
    ),
    mainPanel(
      plotOutput("volcanoPlot")
    )
  )
)

server <- function(input, output) {
  output$volcanoPlot <- renderPlot({
    # Construct the file name based on the selected sample
    file_name <- paste0("combined_data_", input$sample_selection, ".xlsx")
    
    # Read the selected file
    df <- read_excel(file_name, sheet = 1)
    
    # Process the data
    df <- df %>%
      mutate(
        minor = x_fold_minor <= input$minor_threshold_x & x_fold_major >= input$minor_threshold_y,
        major = x_fold_minor >= input$major_threshold_x & x_fold_major <= input$major_threshold_y,
        condition = case_when(
          minor & !major ~ "minor",
          major ~ "major",
          TRUE ~ "other"
        )
      )
    
    # Create the plot
    p <- ggplot(df, aes(x = x_fold_minor, y = x_fold_major)) +
      geom_point(aes(color = condition), alpha = 0.5) +
      scale_color_manual(values = c("minor" = "red", "major" = "blue", "other" = "black")) +
      theme_minimal() +
      labs(title = paste("Volcano Plot: Sample", input$sample_selection),
           x = "x_fold_minor",
           y = "x_fold_major",
           color = "Condition") +
      geom_hline(yintercept = input$minor_threshold_y, linetype = "dashed", color = "green") +
      geom_vline(xintercept = input$minor_threshold_x, linetype = "dashed", color = "green") +
      geom_hline(yintercept = input$major_threshold_y, linetype = "dashed", color = "purple") +
      geom_vline(xintercept = input$major_threshold_x, linetype = "dashed", color = "purple") +
      theme(legend.position = "right")
      
    # Conditionally add gene names
    if(input$show_gene_names) {
      p <- p + geom_text(aes(label = df[[1]]), vjust = 1, hjust = 1, size = 3, check_overlap = TRUE)
    }
    
    print(p)
  })
}

shinyApp(ui = ui, server = server)

