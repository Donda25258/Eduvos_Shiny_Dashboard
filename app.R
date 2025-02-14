# Load required packages
library(shiny)
library(tidyverse)

# ---------------------------
# 1. Data Import & Cleaning
# ---------------------------

# Read the CSV (ensure graduate_survey.csv is in the same folder as app.R)
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

# (Optional) Standardize campus names or other categorical columns if needed:
survey_clean <- survey_clean %>%
  mutate(
    Campus = case_when(
      Campus %in% c("Durban Campus", "Umhlanga Campus") ~ "Durban Campus",
      Campus %in% c("Port Elizabeth Campus", "Nelson Mandela Bay Campus") ~ "Gqeberha Campus",
      Campus %in% c("Nelspruit Campus", "Mbombela Campus") ~ "Mbombela/Nelspruit Campus",
      TRUE ~ Campus
    )
    # You can add more standardizations for EduLevel, AITool, etc. if you wish
  )

# Subset to top 5 campuses
campus_counts <- survey_clean %>%
  count(Campus, sort = TRUE)

top_campuses <- campus_counts %>%
  top_n(5, n) %>%
  pull(Campus)

survey_final <- survey_clean %>%
  filter(Campus %in% top_campuses)

# Create a "long" version of data for Employment
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
      # Dropdown to select a study field
      selectInput(
        "studyField", "Select Study Field:",
        choices = unique(survey_final$StudyField),
        selected = unique(survey_final$StudyField)[1]
      ),
      hr(),
      helpText("This dashboard presents key insights on tool usage and employment among Eduvos IT graduates.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Programming Languages", plotOutput("plangPlot")),
        tabPanel("Databases", plotOutput("dbPlot")),
        tabPanel("Web Frameworks", plotOutput("webPlot")),
        tabPanel("Employment", plotOutput("employmentPlot"))
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
    survey_final %>%
      filter(StudyField == input$studyField)
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
  
  # D) Employment Distribution (pie chart)
  output$employmentPlot <- renderPlot({
    survey_clean_long %>%
      filter(StudyField == input$studyField) %>%
      group_by(Employment) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(prop = count / sum(count)) %>%
      ggplot(aes(x = "", y = prop, fill = Employment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(
        title = paste("Employment Distribution in", input$studyField),
        fill = "Employment Status"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )
  })
}

# ---------------------------
# 4. Run the Application
# ---------------------------
shinyApp(ui = ui, server = server)
