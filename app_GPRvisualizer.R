
if(!require("devtools")) install.packages("devtools")

devtools::install_github("emanuelhuber/RGPR")
library(RGPR)

if(!require("shiny")) install.packages("shiny")
library(shiny)

options(shiny.maxRequestSize = 30*1024^2)

# Define UI for data upload app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Uploading GPR Files"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose GPR File",
                multiple = TRUE
      )
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      # tableOutput("contents")
      plotOutput("plot1",width = "800px", height = "400px") 
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  # output$contents <- renderTable({
    output$plot1 <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    # print(input$file1$datapath)
    tryCatch(
      {
        x <- RGPR::readGPR(dsn = input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
      plot(x)    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

