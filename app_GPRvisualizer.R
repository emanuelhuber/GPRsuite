
if(!require("devtools")) install.packages("devtools")
devtools::install_local("/media/huber/Seagate1TB/UNIBAS/PROJECTS/RGPR/CODE/RGPR", force = TRUE)
# devtools::install_local("/mnt/data/huber/Documents/WORKNEW/RGPR_PROJECT/RGPR",
#                         force = TRUE)
# devtools::install_local("/mnt/data/huber/Documents/WORKNEW/GPR_Project/RGPR", force = TRUE)

# devtools::install_github("emanuelhuber/RGPR")
library(RGPR)

library(shiny)
# library(RGPR)

options(shiny.maxRequestSize = 30*1024^2)
# 
# setwd("/mnt/data/huber/Documents/WORKNEW/GPR_Project/DEVELOPMENT/FILE_FORMAT/ImpulseRadar")
# 
# readGPR(dsn  = "CO_examples/CO example_0001_0.iprb",
#         dsn2 = "CO_examples/CO example_0001_0.iprh")


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose GPR File",
                multiple = TRUE #,
                # accept = c("text/csv",
                           # "text/comma-separated-values,text/plain",
                           # ".csv")
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      # tableOutput("contents")
      plotOutput("plot1",width="800px",height="400px")
      
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
      print(input$file1$datapath)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        # TWO OPTIONS FOR GPR DATA FORMAT WITH ASCII HEADER FILES:
        # 1. Uploat .dt1 and then ask for file .hd
        # 2. Multiple upload:
        #     - sort the file
        #     - read
        # if(length(input$file1$datapath) > 1){
        #   dsn <- input$file1$datapath[1]
        #   dsn2 <- input$file1$datapath[2]
        # }else{
        #   dsn <- input$file1$datapath[1]
        #   dsn2 <- NULL
        # }
        print(input$file1$datapath)
        x <- RGPR::readGPR(dsn = input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
      plot(x)
      
      
    # 
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

