library(ggplot2)
library(reshape2)
library(plyr)
library(plotly)

table = read.table("RPKM_table.txt", header=TRUE)
ncols = length(table)
libraries = names(table[2:ncols])


server <- function(input, output) {
   output$scatterPlot <- renderPlotly( {

    melt_table = melt(table, id=c("GENE",input$xlibrary))
    sub_table=subset(melt_table,variable == input$ylibrary)
    names(sub_table)[names(sub_table)==input$xlibrary] <- "library1"    
    
    p <- ggplot(sub_table, aes(library1, value, text=GENE)) + 
       geom_point(size=0.5, alpha=0.2)+
       scale_x_log10(limits=c(0.1,100000))+
       scale_y_log10(limits=c(0.1,100000))+
       geom_abline(intercept = 0, slope = 1)+
       labs(x=input$xlibrary, y=input$ylibrary)+
       theme(aspect.ratio=1)+
       theme_bw()
       
       
    pp <- ggplotly(p)
    
    print(pp)
   })



}




ui <- pageWithSidebar(

  # Application title
  headerPanel("Example - Basic RPKM scatter"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("xlibrary", label = "select library for x-axis:",
                choices = libraries),
    
    selectInput("ylibrary", label = "select library for y-axis:",
                choices = libraries)

  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
   
    plotlyOutput("scatterPlot")
  ))
shinyApp(ui = ui, server = server)
