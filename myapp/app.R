library(shiny)
library(plotly)
library(dplyr)

# Load the data from the fixed CSV file
recode_data=read.csv("updated_recode_project_keywords_20240919_cosine1.csv", stringsAsFactors = F)


similarity_matrix <- recode_data %>% 
  select(title, Engineering, Computing, Clinical.Sciences, Public.Health,
         Mathematics, Business.and.Management, Physics, Chemistry, 
         Materials, Bioengineering)
# programming language used
prog_lang <- recode_data %>% 
  select(title, Python, R, Fortran, C_Plus_Plus)

# Define UI
ui <- fluidPage(
  titlePanel("Project Similarity Radar Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("project", "Select a Project:", choices = similarity_matrix$title)
    ),
    mainPanel(
      plotlyOutput("radarPlot"),
      plotlyOutput("radarPlot_lang")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  selected_project_data <- reactive({
    similarity_matrix %>%
      filter(title == input$project) %>%
      select(-title)
  })
  
  selected_lang_data <- reactive({
    prog_lang %>%
      filter(title == input$project) %>%
      select(-title)
  })
  
  output$radarPlot <- renderPlotly({
    radar_data <- as.data.frame(selected_project_data())
    req(radar_data)
    
    plot_ly(
      type = 'scatterpolar',
      r = as.numeric(radar_data[1, ]),
      theta = colnames(radar_data),
      fill = 'toself'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 1)
          )
        ),
        showlegend = FALSE
      )
  })
  
  output$radarPlot_lang <- renderPlotly({
    radar_lang <- as.data.frame(selected_lang_data())
    req(radar_lang)
    
    plot_ly(
      type = 'scatterpolar',
      r = as.numeric(radar_lang[1, ]),
      theta = colnames(radar_lang),
      fill = 'toself'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 1)
          )
        ),
        showlegend = FALSE
      )
  })
  
  # Trigger initial rendering
  observe({
    updateSelectInput(session, "project", selected = similarity_matrix$title[2])
    updateSelectInput(session, "project", selected = similarity_matrix$title[1])
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

# to deploy the shinyApp on GitHub
# organize code in myapp subdir (rename this script to app.R) and run the following command in console
# shinylive::export(appdir = "myapp", destdir = "docs")
# upload to GitHub repo and set page to the docs folder



