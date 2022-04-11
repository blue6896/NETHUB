
library(shiny)
library(corrplot)

#nn,samples,case,numhub,bighub,bigwgt,disconnected,meanval,sdval,binaryopt

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 3, min = 1, max = 5),
  sliderInput(inputId = "bighub",
              label = "Choose a number",
              value = 3, min = 1, max = 5),
  sliderInput(inputId = "binaryopt",
              label = "Choose the type of data",
              value = 0, min = 0, max = 1),
  plotOutput("corrplot")
)

server = function (input, output){
  
  source('simulated_data_codes.R')
    nn=50
    samples=500
    case=1 
    numhub=5 
    bighub=5
    bigwgt=1 
    disconnected=15
    meanval= 0
    sdval=1
    binaryopt=F
    
    set.seed(100)
    a=simulated_data_codes(nn=nn,
                           samples=samples,
                           case=case,
                           numhub=numhub,
                           bighub=bighub,
                           bigwgt=bigwgt,
                           disconnected=disconnected,
                           meanval=meanval,
                           sdval=sdval,
                           binaryopt=binaryopt)
    
   # library(corrplot)
  #  corrplot(a$originconx, tl.cex= 0.6)
    
    
    
    output$corrplot = renderPlot({
      a=simulated_data_codes(nn=nn,
                             samples=samples,
                             case=case,
                             numhub=input$num,
                             bighub=input$bighub,
                             bigwgt=bigwgt,
                             disconnected=disconnected,
                             meanval=meanval,
                             sdval=sdval,
                             binaryopt=input$binaryopt)
      corrplot(cor(a$simudata))
  #    library(igraph)
  #    network <- graph_from_adjacency_matrix( a$originconx, weighted=T, mode="undirected", diag=F)
      # Basic chart
 #     plot(network, vertex.size=5)
    })
  
}

shinyApp(ui = ui, server = server)
