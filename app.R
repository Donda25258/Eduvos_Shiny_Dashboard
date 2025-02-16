# app.R

library(shiny)
library(tidyverse)

# ---------------------------
# 1. Data Import & Cleaning
# ---------------------------
# Read the CSV (make sure graduate_survey.csv is in the same folder as app.R)
survey_data <- read_csv("graduate_survey.csv")

# Select & rename relevant columns
survey_selected <- survey_data %>%
  select(
    Campus, StudyField, Branch, Role, EduLevel, ProgLang,
    Databases, Platform, WebFramework, Industry, AISearch,
    `AIToolCurrently Using`, Employment
  ) %>%
  rename(AITool = `AIToolCurrently Using`)

# Drop rows missing key columns
survey_clean <- survey_selected %>%
  drop_na(Campus, StudyField, ProgLang)

# Standardize Campus names (example standardization)
survey_clean <- survey_clean %>%
  mutate(
    Campus = case_when(
      Campus %in% c("Durban Campus", "Umhlanga Campus") ~ "Durban Campus",
      Campus %in% c("Port Elizabeth Campus", "Nelson Mandela Bay Campus") ~ "Gqeberha Campus",
      Campus %in% c("Nelspruit Campus", "Mbombela Campus") ~ "Mbombela/Nelspruit Campus",
      TRUE ~ Campus
    )
  )

# Subset to top 5 campuses by response count
campus_counts <- survey_clean %>% count(Campus, sort = TRUE)
top_campuses <- campus_counts %>% top_n(5, n) %>% pull(Campus)
survey_final <- survey_clean %>% filter(Campus %in% top_campuses)

# Create a long-format version for Employment data
survey_clean_long <- survey_final %>%
  separate_rows(Employment, sep = ";") %>%
  mutate(
    Employment = str_trim(Employment),
    Employment = case_when(
      Employment == "Employed, full-time" ~ "Full-time",
      Employment == "Independent contractor, freelancer, or self-employed" ~ "Freelance",
      Employment == "Employed, part-time" ~ "Part-time",
      Employment == "Not employed, but looking for work" ~ "Unemployed, seeking",
      Employment == "Not employed, and not looking for work" ~ "Unemployed, not seeking",
      TRUE ~ Employment
    )
  )

# ---------------------------
# 2. Shiny UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Eduvos Graduate Survey Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("studyField", "Select Study Field:",
                  choices = unique(survey_final$StudyField),
                  selected = unique(survey_final$StudyField)[1]),
      hr(),
      helpText("This dashboard presents key insights on tool usage, including cloud platforms and AI tools, and employment among Eduvos IT graduates.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Programming Languages", plotOutput("plangPlot")),
        tabPanel("Databases", plotOutput("dbPlot")),
        tabPanel("Web Frameworks", plotOutput("webPlot")),
        tabPanel("Cloud Platforms", plotOutput("platformPlot")),
        tabPanel("AI Tools", 
                 plotOutput("aiSearchPlot"),
                 plotOutput("aiToolPlot"))
      )
    )
  )
)

# ---------------------------
# 3. Shiny Server
# ---------------------------
server <- function(input, output, session) {
  
  # Reactive data filtered by the selected study field
  filteredData <- reactive({
    survey_final %>% filter(StudyField == input$studyField)
  })
  
  # A) Top 10 Programming Languages
  output$plangPlot <- renderPlot({
    filteredData() %>%
      separate_rows(ProgLang, sep = ";") %>%
      mutate(ProgLang = str_trim(ProgLang)) %>%
      filter(ProgLang != "") %>%
      count(ProgLang, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(ProgLang, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Top 10 Programming Languages in", input$studyField),
        x = "Programming Language",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # B) Top 10 Databases
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
      labs(
        title = paste("Top 10 Databases in", input$studyField),
        x = "Database",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # C) Top 10 Web Frameworks
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
      labs(
        title = paste("Top 10 Web Frameworks in", input$studyField),
        x = "Web Framework",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # D) Top 10 Cloud Platforms
  output$platformPlot <- renderPlot({
    filteredData() %>%
      separate_rows(Platform, sep = ";") %>%
      mutate(Platform = str_trim(Platform)) %>%
      filter(Platform != "") %>%
      count(Platform, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(Platform, n), y = n)) +
      geom_col(fill = "lightblue") +
      coord_flip() +
      labs(
        title = paste("Top 10 Cloud Platforms in", input$studyField),
        x = "Cloud Platform",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  
  # E) Top 10 AI Search Tools
  output$aiSearchPlot <- renderPlot({
    filteredData() %>%
      separate_rows(AISearch, sep = ";") %>%
      mutate(AISearch = str_trim(AISearch)) %>%
      filter(AISearch != "") %>%
      count(AISearch, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(AISearch, n), y = n)) +
      geom_col(fill = "salmon") +
      coord_flip() +
      labs(
        title = paste("Top 10 AI Search Tools in", input$studyField),
        x = "AI Search Tool",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # F) Top 10 AI Developer Tools
  output$aiToolPlot <- renderPlot({
    filteredData() %>%
      separate_rows(AITool, sep = ";") %>%
      mutate(AITool = str_trim(AITool)) %>%
      filter(AITool != "") %>%
      count(AITool, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(AITool, n), y = n)) +
      geom_col(fill = "plum") +
      coord_flip() +
      labs(
        title = paste("Top 10 AI Developer Tools in", input$studyField),
        x = "AI Developer Tool",
        y = "Count"
      ) +
      theme_minimal()
  })
}

# ---------------------------
# 4. Run the Application
# ---------------------------
shinyApp(ui, server)
