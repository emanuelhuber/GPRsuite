

#Sorted it. Based on the help of a previous post: avoid double refresh of plot in shiny



# Sorted it. Based on the help of a previous post: avoid double refresh of plot in shiny

library(shiny)
library(RGPR)

ui <- basicPage(
  #actionButton("edit","edit"),
  fileInput("file1", "Choose GPR File",
            multiple = TRUE 
  ),
  # Input: Select separator ----
  radioButtons("edit", "Edit",
               choices = c(Draw = "draw",
                           Delete = "delete",
                           Select = "select"),
               selected = "draw"),
  actionButton("delete_selected", "Delete selected"),
  plotOutput("plot1", 
             click = "plot_click", 
             brush = brushOpts(id = "plot_brush")
             ),
  downloadButton("downloadData", "Download points ("),
  
  verbatimTextOutput("info"),
  # verbatimTextOutput("click_info"),
  tableOutput('table')
)


server <- function(input, output) {
  output$value <- renderPrint({ input$edit })
   # rv <- list()
  # rv[["m"]] <- data.frame(x = as.numeric(), y = as.numeric())
  # 
  # rv <- reactiveValues(m = data.frame(x = 0, y = 0))
  # 
  # vals <- reactiveValues(
  #   keeprows = rep(TRUE, nrow(rv$m))
  # )
  
  click_saved <- reactiveValues(singleclick = NULL)
  # brush_saved <- reactiveValues(brushedPoints(rv$m, xvar = "x", yvar = "y", input$plot_brush)
  #brush_saved <- reactiveValues(singlebrush = NULL)
  # output$click_info <- renderPrint({
  #   # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
  #   # were a base graphics plot, we'd need those.
  #   nearPoints(rv$m, input$plot1click, addDist = TRUE)
  # })
  
  observeEvent(eventExpr = input$plot_click, 
     handlerExpr = { 
       click_saved$singleclick <- input$plot_click
       if (input$edit == "draw"){
        print(click_saved$singleclick$x)
         xy <- data.frame(click_saved$singleclick$x,
                 click_saved$singleclick$y)
         names(xy) <- c("x", "y")
        rv$m <- rbind(rv$m, xy)
       }else if(input$edit == "delete"){
         if(nrow(rv$m) > 0){
             i <- nearPoints(rv$m,  click_saved$singleclick, 
                             xvar = "x", yvar = "y", addDist = TRUE,
                             allRows = TRUE)
             rv$m <- rv$m[!i$selected_,]
         }
       }
       rv$rmrows <- rep(FALSE, nrow(rv$m))
    }
  )
  
  # output$brush_info <- renderPrint({
  #   brushedPoints(mtcars2, input$plot1_brush)
  # })
  
  brush_saved <- reactiveValues(singlebrush = NULL)
  observeEvent(eventExpr = input$plot_brush,
    handlerExpr = {
     brush_saved$singlebrush <- input$plot_brush
     if(input$edit == "select"){
        j <- brushedPoints(rv$m, brush_saved$singlebrush,
                           xvar = "x", yvar = "y",
                           allRows = TRUE)
        # print(j[["selected_"]])
         rv$rmrows <- j[["selected_"]]
         
      }
    }
  )
  
  
  observeEvent(input$delete_selected, {
    if(input$edit == "select"){
      if(!is.null(rv$rmrows)){
        # print("***", rv$rmrows, "***")
        rv$m <- rv$m[!rv$rmrows, ]
        rv$rmrows <- rep(FALSE, nrow(rv$m))
      }
    }
  })
  
  
  # Toggle points that are clicked
 # observeEvent(input$plot_click, {
  #  res <- nearPoints(mtcars, input$plot_click, allRows = TRUE)
  #  
  #  vals$keeprows <- xor(vals$keeprows, res$selected_)
  #})
  
  rv <- reactiveValues(m = data.frame(x = numeric(), y = numeric()),
                       rmrows =c())
  
  # vals <- reactiveValues(
  #   keeprows = rep(TRUE, nrow(rv$m))
  # )
  
  xplt <- reactiveValues(v = NULL)
  
  observeEvent(input$file1, {
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
        xplt$v <- RGPR::readGPR(dsn = input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  # x <- frenkeLine00
  
  output$plot1 <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    if(!is.null(xplt$v)){
      plot(xplt$v)
    }else{
      plot(0,0, type = "n")
    }
    # plot(0, 0, type='l')
    if(length(rv$m$x[-1] > 2)){
      lines(rv$m$x, rv$m$y)
    }
    # if(length(rv$rmrows) == nrow(rv$m)){
    if(!is.null(rv$rmrows)){
      points(rv$m$x[!rv$rmrows], rv$m$y[!rv$rmrows])
      points(rv$m$x[rv$rmrows], rv$m$y[rv$rmrows], pch = 20, col = "red")
    }else{
      points(rv$m$x, rv$m$y)
    }
    title(length(rv$m$x))
  })
  
  output$info <- renderText({
    paste0(unlist(click_saved$singleclick))
  })
  
  
  .whichMin <- function(x,y){
    which.min(abs(x-y))
  }
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("delineation_",  Sys.Date(), ".csv")
    },
    content = function(file) {
      if(nrow(rv$m) > 0 ){
          if(length(xplt$v@coord) == 0){
            xplt$v@coord <- matrix(0, nrow = ncol(xplt$v), ncol = 3)
            xplt$v@coord[,1] <- xplt$v@pos
          }
          # xvalues <- posLine(xplt$v@coord)
          xval <- relTrPos(xplt$v)
          i <- sapply(rv$m$x, .whichMin, xval)
          j <- sapply(rv$m$y, .whichMin, xplt$v@depth)
          test <- i >= 0 & i <= length(xplt$v) &
            j >= 0 & j <= nrow(xplt$v)
          if(any(!test)) warning("there is a problem")
          print(cbind(i, j))
      }
      write.csv(cbind(i, j), file, row.names = FALSE)
      # write.csv(rv$m, file, row.names = FALSE)
    }
  )
  
  # output$click_info <- renderText({
  #   paste0(unlist(click_saved$singleclick))
  # })

  
  
  
  # Toggle points that are brushed, when button is clicked
  #observeEvent(input$exclude_toggle, {
  #  res <- brushedPoints(mtcars, input$plot_brush, allRows = TRUE)
  #  
  #  vals$keeprows <- xor(vals$keeprows, res$selected_)
  #})
  
  observeEvent(eventExpr = input$edit, handlerExpr = {
    # if (input$edit > 0) {
    #   rv$m <- rbind(rv$m,unlist(click_saved$singleclick))
    # }
  })
  
  output$table <- renderTable({
    if (is.null(rv$m)) {return()}
    # print("***",rv$rmrows, "***")
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  )
  
}

shinyApp(ui, server)

