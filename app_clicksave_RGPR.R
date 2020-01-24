

#Sorted it. Based on the help of a previous post: avoid double refresh of plot in shiny



# Sorted it. Based on the help of a previous post: avoid double refresh of plot in shiny

library(shiny)
library(RGPR)

ui <- basicPage(
  #actionButton("edit","edit"),
  # Input: Select separator ----
  radioButtons("edit", "Edit",
               choices = c(Draw = "draw",
                           Delete = "delete",
                           Select = "select"),
               selected = "draw"),
  plotOutput("plot1", 
             click = "plot_click", 
             brush = brushOpts(id = "plot_brush")
             ),
  downloadButton("downloadData", "Download"),
  
  actionButton("delete_selected", "Delete selected"),
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
  x <- frenkeLine00
  
  output$plot1 <- renderPlot({
    plot(x)
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
          if(length(x@coord) == 0){
            x@coord <- matrix(0, nrow = ncol(x), ncol = 3)
            x@coord[,1] <- x@pos
          }
          # xvalues <- posLine(x@coord)
          xval <- relTrPos(x)
          i <- sapply(rv$m$x, .whichMin, xval)
          j <- sapply(rv$m$y, .whichMin, x@depth)
          test <- i >= 0 & i <= length(x) &
            j >= 0 & j <= nrow(x)
          if(any(!test)) warning("there is a problem")
      }
      write.csv(rv$m, file, row.names = FALSE)
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

