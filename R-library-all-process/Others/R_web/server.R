library(shiny)
library(ggplot2)
 
shinyServer(function(input, output) {
 
  output$plot <- reactivePlot(function() {
 
    p <- ggplot(dataset, aes_string(x=input$x, y=input$y)) + geom_point()
 
    if (input$color != 'None')
      p <- p + 
        aes_string(color=input$color) + 
        theme(legend.position="top")
 
    if (input$smooth)
      p <- p + 
        geom_smooth() + 
        theme(legend.position="top")
 
    print(p)
 
  }, height=400)
 
})