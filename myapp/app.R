library(shiny)
library(plotly)
library(dplyr)

# Load the data from the fixed CSV file
recode_data=read.csv("updated_recode_project_keywords_20240926_gensim2.csv", stringsAsFactors = F)


similarity_matrix <- recode_data %>% 
  select(title, Engineering.gensim, Computing.gensim, Clinical.Sciences.gensim, Public.Health.gensim,
         Mathematics.gensim, Business.and.Management.gensim, Physics.gensim, Chemistry.gensim, 
         Materials.gensim, Bioengineering.gensim)

names(similarity_matrix) <- 
  c("title", "Engineering", "Computing", "Clinical Sciences", "Public Health",
    "Mathematics", "Business and Management", "Physics", "Chemistry", 
    "Materials", "Bioengineering")
# programming language used
prog_lang <- recode_data %>% 
  select(title, Python, R, Fortran, C_Plus_Plus)

# Define UI
ui <- fluidPage(
  titlePanel("Project Similarity Radar Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("project", "Select a Project:", choices = similarity_matrix$title),
      checkboxGroupInput(
        inputId = "research",
        label = "Your Research:",
        choices = names(similarity_matrix)[-1]
      ),
      #textInput("research", "Your research:"),
      #actionButton("calculate", "Calculate Similarity"),
      #verbatimTextOutput("result")
    ),
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("radarPlot")),
        column(6, plotlyOutput("radarPlot_lang"))
      ),
      fluidRow(
        column(12, textOutput("selected_values")),
        column(12, uiOutput("recommendations"))
        #column(12, textOutput("enteredText2")),
        #column(12, verbatimTextOutput("result"))
      )
      
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
  
  
  output$selected_values <- renderText({
    paste("You selected:", paste(input$research, collapse = ", "))
  })
  
  output$recommendations <- renderText({
    checked = paste(input$research, collapse = ", ")
    checked = strsplit(checked, ", ")[[1]]
    if (length(checked)>0) {
      output_titles = (similarity_matrix %>% 
                         arrange(desc(across(all_of(checked)))) %>% 
                         select(title) %>% 
                         head(3))$title
      HTML(paste("Recommended Projects: <br>", HTML(paste(output_titles, collapse = "<br> "))))
    }
    
  })
  
  # Trigger initial rendering
  observe({
    updateSelectInput(session, "project", selected = similarity_matrix$title[2])
    updateSelectInput(session, "project", selected = similarity_matrix$title[1])
    #print(input$choices)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

# to deploy the shinyApp on GitHub
# organize code in myapp subdir (rename this script to app.R) and run the following command in console
# shinylive::export(appdir = "myapp", destdir = "docs")
# upload to GitHub repo and set page to the docs folder


