
if(!require("shiny")) install.packages("shiny")
library(shiny)


ui <- basicPage(
  radioButtons("edit", "Edit",
               choices = c(Draw = "draw",
                           Delete = "delete",
                           Select = "select"),
               selected = "draw"),
  plotOutput("plot1", 
             click = "plot_click", 
             brush = brushOpts(id = "plot_brush")
             ),
  actionButton("delete_selected", "Delete selected"),
  verbatimTextOutput("info"),
  # verbatimTextOutput("click_info"),
  tableOutput('table')
)


server <- function(input, output) {
  output$value <- renderPrint({ input$edit })
  
  click_saved <- reactiveValues(singleclick = NULL)
  
  observeEvent(eventExpr = input$plot_click, 
     handlerExpr = { 
       click_saved$singleclick <- input$plot_click
       if (input$edit == "draw"){
        # print(click_saved$singleclick$x)
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
  
  
  rv <- reactiveValues(m = data.frame(x = numeric(), y = numeric()),
                       rmrows =c())
  
  output$plot1 <- renderPlot({
    plot(0, 0, type='l')
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
    paste0( unlist(click_saved$singleclick))
  })
  
  # output$click_info <- renderText({
  #   paste0(unlist(click_saved$singleclick))
  # })
 
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

