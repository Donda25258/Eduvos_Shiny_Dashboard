# Load required packages
library(shiny)
library(tidyverse)

# ---- User Interface ----
ui <- fluidPage(
  titlePanel("Eduvos Graduate Survey Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select a study field (e.g., IT, Data Science, Computer Science)
      selectInput("studyField", "Select Study Field:",
                  choices = unique(survey_final$StudyField),
                  selected = unique(survey_final$StudyField)[1]),
      hr(),
      helpText("This dashboard presents key insights on tool usage and employment among Eduvos IT graduates.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Programming Languages", 
                 plotOutput("plangPlot")),
        tabPanel("Databases", 
                 plotOutput("dbPlot")),
        tabPanel("Web Frameworks", 
                 plotOutput("webPlot")),
        tabPanel("Employment", 
                 plotOutput("employmentPlot"))
      )
    )
  )
)

# ---- Server Function ----
server <- function(input, output, session) {
  
  # Reactive data filtered by the selected study field
  filteredData <- reactive({
    survey_final %>% filter(StudyField == input$studyField)
  })
  
  # Plot for top 10 Programming Languages
  output$plangPlot <- renderPlot({
    filteredData() %>%
      separate_rows(ProgLang, sep = ";") %>%   # Split multi-valued responses
      mutate(ProgLang = str_trim(ProgLang)) %>%  # Remove extra spaces
      filter(ProgLang != "") %>%
      count(ProgLang, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(ProgLang, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Top 10 Programming Languages in", input$studyField),
           x = "Programming Language",
           y = "Count") +
      theme_minimal()
  })
  
  # Plot for top 10 Databases
  output$dbPlot <- renderPlot({
    filteredData() %>%
      separate_rows(Databases, sep = ";") %>%
      mutate(Databases = str_trim(Databases)) %>%
      filter(Databases != "") %>%
      count(Databases, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(Databases, n), y = n)) +
      geom_col(fill = "lightgreen") +
      coord_flip() +
      labs(title = paste("Top 10 Databases in", input$studyField),
           x = "Database",
           y = "Count") +
      theme_minimal()
  })
  
  # Plot for top 10 Web Frameworks
  output$webPlot <- renderPlot({
    filteredData() %>%
      separate_rows(WebFramework, sep = ";") %>%
      mutate(WebFramework = str_trim(WebFramework)) %>%
      filter(WebFramework != "") %>%
      count(WebFramework, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(WebFramework, n), y = n)) +
      geom_col(fill = "orange") +
      coord_flip() +
      labs(title = paste("Top 10 Web Frameworks in", input$studyField),
           x = "Web Framework",
           y = "Count") +
      theme_minimal()
  })
  
  # Plot for Employment Distribution (using the long-format employment data)
  output$employmentPlot <- renderPlot({
    # We filter the long-format dataset for the selected StudyField
    survey_clean_long %>%
      filter(StudyField == input$studyField) %>%
      group_by(Employment) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = count / sum(count)) %>%
      ggplot(aes(x = "", y = prop, fill = Employment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = paste("Employment Distribution in", input$studyField),
           fill = "Employment Status") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
  })
  
}

# ---- Run the Application ----
shinyApp(ui = ui, server = server)
