# Eduvos Graduate Survey Dashboard

## Overview
This Shiny application analyzes survey data from Eduvos IT graduates to identify:
- The top tools (programming languages, databases, web frameworks, AI tools, etc.)
- Employment distributions and other insights by study field.

It was built using **R** and the **tidyverse** suite of packages, following the data manipulation and visualization approaches in [*R for Data Science*](https://r4ds.had.co.nz/) by Hadley Wickham and Garrett Grolemund.

## Features
1. **Interactive Dashboard**: Users can select a study field (e.g., IT, Data Science, Computer Science) from a dropdown.
2. **Multiple Tabs**:
   - **Programming Languages**: Bar chart of the top languages used by graduates.
   - **Databases**: Bar chart of the top databases.
   - **Web Frameworks**: Bar chart of popular frameworks.
   - **Employment**: Polar (pie) chart showing employment status distribution.
3. **Data Tidying**:
   - Multi-response columns (e.g., `ProgLang`, `Databases`) are split using `tidyr::separate_rows()`.
   - Categorical variables are standardized (e.g., merging campus name variations).
4. **Data Visualization**:
   - **ggplot2** is used for bar charts and pie charts, consistent with best practices from *R for Data Science*.

## Files
- **app.R**: The main Shiny app file containing the UI and server logic.
- **survey_clean.R** (or similar): R script that cleans and standardizes the raw survey data (Question 1).
- **README.md**: This file, providing instructions and overview.

## Getting Started

### Prerequisites
- **R** (version 4.0 or higher recommended)
- **RStudio** (optional but recommended)
- **tidyverse**: `install.packages("tidyverse")`
- **shiny**: `install.packages("shiny")`
- **rsconnect** (optional, for deployment): `install.packages("rsconnect")`

### Installation
1. **Clone or download** this repository.
2. Open **app.R** in RStudio.

### Usage
1. Run the data cleaning script (e.g., `survey_clean.R`) to create `survey_final` and `survey_clean_long`.
2. In RStudio, open **app.R** and click **Run App** or run:
   ```r
   shiny::runApp()
