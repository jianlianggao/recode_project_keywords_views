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
server <- function(input, output) {
    output$radarPlot <- renderPlotly({
      selected_project <- similarity_matrix %>%
        filter(title == input$project) %>%
        select(-title)
    
    # Prepare data for radar chart
    radar_data <- as.data.frame((selected_project))
    #radar_data <- rbind(rep(1, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)
    #colnames(radar_data) <- colnames(similarity_matrix)[-1]

    # Plot radar chart using Plotly
    fig <- plot_ly(
      type = 'scatterpolar',
      r = as.numeric(radar_data[1, ]),
      theta = colnames(radar_data),
      fill = 'toself'
    ) 
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        ),
        showlegend = F
      )
    
    fig
  })
    output$radarPlot_lang <- renderPlotly({
      selected_lang <- prog_lang %>%
        filter(title == input$project) %>%
        select(-title)
      
      # Prepare data for radar chart
      radar_lang <- as.data.frame((selected_lang))
      #radar_data <- rbind(rep(1, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)
      #colnames(radar_data) <- colnames(similarity_matrix)[-1]
      
      # Plot radar chart using Plotly
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.numeric(radar_lang[1, ]),
        theta = colnames(radar_lang),
        fill = 'toself'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,1)
            )
          ),
          showlegend = F
        )
      
      fig
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# to deploy the shinyApp on GitHub
# organize code in myapp subdir (rename this script to app.R) and run the following command in console
# shinylive::export(appdir = "myapp", destdir = "docs")
# upload to GitHub repo and set page to the docs folder



