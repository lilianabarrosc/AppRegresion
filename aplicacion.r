#install.packages('shiny')
library('shiny')
#install.packages('shinydashboard')
library('shinydashboard')
#install.packages("devtools")
#library(devtools)
#install_github("mariytu/RegressionLibs") #Para usar esto hay que tener instalado devtools
library('RegressionLibs')
#install.packages("Amelia")
library('Amelia')
#install.packages("VIM")
library('VIM')
#install.packages("clusterSim")
library('clusterSim')

source('opcionesDashboard.r')
source('analisisExploratorio.r')
source('data.r')

#variable global que contendra el nombre de los archivos de la bd
data_sets <- list("iris" = 1, "airquality" = 2, "cars" = 3, "new" = 4)

#Cuerpo de la p�gina
body <- dashboardBody(
  tabItems( 
    #Tab del home
    tabItem(tabName = "home",
            h2("Working...")
          ),
    #Tab del data
    tabItem(tabName = "data",
            viewData()
    ),
    #Inicio tabs Analisis exploratorio (funcionalidades en el archivo analisisExploratorio.r)
    tabItem(tabName = "visualization",
            tabsVisualization("visualization", "Scatter plot1", "Scatter plot2")
    ),
    tabItem(tabName = "mvalues",
            tabsMissingValues("Missing values", "Plot 1", "Plot 2","Plot 3")
    ),
    tabItem(tabName = "nremoval",
            h2("Working...")
    ),
    tabItem(tabName = "normalization",
            normalizations("Normalization")
    ),
    tabItem(tabName = "dreduccion",
            tabsDimensionalityReduction("Dimensionality reduccion", "PCA", "SVD", 
                                        "Colinearity test", "Attribute selection")
    ),
    tabItem(tabName = "odetection",
            h2("Working...")
    )
    #Fin tabs Analisis exploratorio
  )
)
#--------------------Cliente-------------------
#head() y sidebar() son funciones contenidas en el archivo opcionesDashboard.r
ui <- dashboardPage(head(), sidebar(), body)

#--------------------Servidor-------------------

server <- function(input, output, session) {
  
  # -------------> Lectura de archivo
#   output$contents <- renderDataTable(
#     
#     #inFile <- input$file1
#     
#     #if (is.null(inFile))
#      # return(NULL)
#     
#     #read.csv(inFile$datapath, header=TRUE, sep=',', quote = '"')
#     datatable(iris, colnames = c('Data set', 'Size', 'Date', 'Dimensions', 'Actions'))
#   )
  
  #----------> data set
  
  output$ui <- renderUI({
    if (is.null(input$select_file))
      return()
    switch(input$select_file,
           '4' = fileInput('file1', 'Choose CSV File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv'))
    )
  })
  
  #Seleccion de data set a utilizar
  file <- reactive({
    inFile <- input$file1
    if (is.null(input$select_file))
      return()
    switch(input$select_file,
           '1'= iris,
           '2'= airquality,
           '3'= read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ","),
           '4'= read.csv(inFile$datapath)
    )
  })
  
  #muestro un resumen del data set seleccionado
  output$str_data <- renderPrint({
    str(file())
  })
  
  #muestro un sumary del data set seleccionado
  output$summary_data <- renderPrint({#renderDataTable(
    summary(file())
   # options = list(paging = FALSE, searching = FALSE)
  })
  
  #----------> dimensionalidad del archivo
  #Con dim puedo saber la cantidad de atributos y observaciones que posee el archivo,
  #en dim(data)[1] se pueden encontrar la cantidad de observaciones y en dim(data)[2] la 

  #*************************************************
  #----------> Sacar columnas con valores nominales
  
  only_file_nums <- reactive({
    aux <- data.frame(file())
    nums <- sapply(aux, is.numeric)
    aux[ , nums]
  }) 
  
  #----------> Graficos de visualizacion
  
  #Actualizo el m�ximo del slider con el valor del tama�o del archivo seleccionado
  output$slider_range_range_density <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("x1", label = "X", min = 1, 
                  max = dim(file())[2], value = c(1,4)),
      sliderInput("y1", label = "Y", min = 1, 
                  max = dim(file())[2], value = 2),
      sliderInput("z1", label = "Observations", min = 1, 
                  max = dim(file())[1], value = c(1, dim(file())[1]))
    )
  })
#   max_variables <- dim(file())[2]
#   
#   updateSliderInput(session, "x1",
#                     max = dim(file())[2])
#   updateSliderInput(session, "y1",
#                     max = dim(file())[2])
#   updateSliderInput(session, "z1",
#                     max = dim(file())[1])
  
  #seleccion de atributos y observaciones del data set
  dat1 <- reactive({
    only_file_nums()[input$z1[1]:input$z1[2],input$x1[1]:input$x1[2]]
  })
  
  #Grafico correspondiente a scatterPlot con un grafico de densidad 
  output$scatter1 <- renderPlot({
    #ir.pca <- prcomp(dat1(), center = TRUE, scale. = TRUE)
    #c <- c(input$x1[1]:input$x1[2])
    #sacar columnas con datos nominales
    ScatterplotMatrix(dat1(), c(input$x1[1]:input$x1[2]), only_file_nums()[,input$y1], names(only_file_nums())[[input$y1]])
  })
  
#   #Slider visualizacion grafico de histograma
#   output$slider_range_range_hitograma <- renderUI({
#     box(
#       title = "Range", width = 6, solidHeader = TRUE,
#       background = "aqua",
#       sliderInput("x2", label = "X", min = 1, 
#                   max = dim(file())[2], value = c(1, dim(file())[2])),
#       sliderInput("y2", label = "Y", min = 1, 
#                   max = dim(file())[2], value = 2),
#       sliderInput("z2", label = "Observations", min = 1, 
#                   max = dim(file())[1], value = c(1, dim(file())[1]))
#     )
#   })
#   
#   #seleccion de atributos y observaciones del data set
#   dat2 <- reactive({
#     file()[input$z2[1]:input$z2[2],input$x2[1]:input$x2[2]]
#   })
  
  #*********************************************
  #---------------> Graficos correspondientes a missing values
 
   #Slider visualizacion grafico de missing values Amelia
  output$slider_range_range_amelia <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("attributes", label = "Attributes", min = 1, 
                  max = dim(file())[2], value = c(1,4)),
      sliderInput("observation", label = "Observation", min = 1, 
                  max = dim(file())[1], value = c(1, (dim(file())[1])/2))
    )
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 1
  selectedData1 <- reactive({
    file()[input$observation[1]:input$observation[2], 
               input$attributes[1]:input$attributes[2]]
  })
  
  #Opcion 1 (libreria Amelia)
  output$missing1 <- renderPlot({
    missmap(selectedData1(), main = "Missing values vs observed")
  })
  
#   #---------descarga del grafico opcion 1
# #   downloadInput <- reactive({
# #     switch(input$radio,
# #            '1' = '.png',
# #            '2' = '.svg',
# #            '3' = '.pdf')
# #   })
#   
  #Slider visualizacion grafico de missing VIM option1
  output$slider_range_range_option1 <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("attributes2", label = "Attributes", min = 1, 
                  max = dim(file())[2], value = c(1, 4)),
      sliderInput("observation2", label = "Observation", min = 1, 
                  max = dim(file())[1], value = c(1, (dim(file())[1])/2))
    )
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 2
  selectedData2 <- reactive({
    file()[input$observation2[1]:input$observation2[2], 
           input$attributes2[1]:input$attributes2[2]]
  })
  
  #Opcion 2 (libreria VIM)
  output$missing2 <- renderPlot({
    aggr(selectedData2(), col=c('red','dark grey'), numbers=TRUE, 
         sortVars=TRUE, labels=names(data), cex.axis=.8, gap=1, 
         ylab=c("Histogram of missing data","Pattern"))
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$slider_range_range_option2 <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("x3", label = "X", min = 1, 
                  max = dim(file())[2], value = c(1, 4)),
      sliderInput("y3", label = "Y", min = 1, 
                  max = dim(file())[2], value = 2),
      sliderInput("z3", label = "Observations", min = 1, 
                  max = dim(file())[1], value = c(1, dim(file())[1]))
    )
  })
  
  #Obtengo la seleccion de atributos a comparar para la Opcion 3
  dat3 <- reactive({
    file()[input$z3[1]:input$z3[2],input$x3[1]:input$x3[2]]
  })
  
  #Option 3 (matricial)
  output$missing3 <- renderPlot({
    scattmatrixMiss(dat3(), interactive = F, highlight = c(names(file())[[input$y3]]))
  })
  
  #************************************************
  #-------------> Normalization
  #salida dinamica de rango para normalizacion
  output$range <- renderUI({
    if (is.null(input$normalizationType))
      return()
    switch(input$normalizationType,
           '2' =  tags$div( class = 'col-sm-8',
                            tags$div( class = 'col-sm-4',
                                      numericInput("min", label = "Min", value = 0)
                            ),
                            tags$div( class = 'col-sm-4',
                                      numericInput("max", label = "Max", value = 1)
                            )
                  ),
           '3'= tags$div( class = 'col-sm-8',
                          selectInput("type_normalization", label = "Other normalization", 
                                      choices = list("without normalization" = "n0", 
                                                     "standardization ((x-mean)/sd)" = "n1", 
                                                     "positional standardization ((x-median)/mad)" = "n2",
                                                     "unitization ((x-mean)/range)" = "n3", 
                                                     "positional unitization ((x-median)/range)" = "n3a",
                                                     "unitization with zero minimum ((x-min)/range)" = "n4",
                                                     "normalization in range <-1,1> ((x-mean)/max(abs(x-mean)))" = "n5",
                                                     "positional normalization in range <-1,1> ((x-median)/max(abs(x-median)))" = "n5a",
                                                     "quotient transformation (x/sd)" = "n6",
                                                     "positional quotient transformation (x/mad)" = "n6a",
                                                     "quotient transformation (x/range)" = "n7",
                                                     "quotient transformation (x/max)" = "n8",
                                                     "quotient transformation (x/mean)" = "n9",
                                                     "positional quotient transformation (x/median)" = "n9a",
                                                     "quotient transformation (x/sum)" = "n10",
                                                     "quotient transformation (x/sqrt(SSQ))" = "n11",
                                                     "normalization ((x-mean)/sqrt(sum((x-mean)^2)))" = "n12",
                                                     "positional normalization ((x-median)/sqrt(sum((x-median)^2)))" = "n12a",
                                                     "normalization with zero being the central point ((x-midrange)/(range/2))" = "n13") 
                                      ),
                          radioButtons("type", label = "Type",
                                       choices = list("Column" = "column", "Row" = "row")
                          )
           )
    )
  })
  
  #obtengo el tipo de normalizacion seleccionada y aplico la normalizacion correspondiente
  normalization_type <- reactive({
    if (is.null(input$normalizationType))
      return()
    switch(input$normalizationType,
           '1'= normalizeData(file()),
           '2'= normalizeData(file(), input$min, input$max),
           '3'= data.Normalization(file(),type=input$type_normalization ,normalization= input$type)
    )
  })
  
  #muestro los primeros 10 atributos del data set original
  output$original_data <- renderPrint({#renderDataTable(
    file()[1:10,]
    #options = list(paging = FALSE, searching = FALSE)
  })
  
  #muestro os primeros 10 atributos del data set normalizado
  output$normalized_data <- renderPrint({#renderDataTable(
    normalization_type()[1:10,]
    #options = list(paging = FALSE, searching = FALSE)
  })
  
  #muestro un sumary
  output$summary_normalization <- renderPrint({#renderDataTable(
    summary(normalization_type())
    #options = list(paging = FALSE, searching = FALSE)
  })
  
  
  #************************************************
  #-------------> Reduccion de la dimencionalidad
  
  #Slider visualizacion grafico PCA
  output$slider_range_range_pca <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("attributes3", label = "Attributes", min = 1, 
                  max = dim(file())[2], value = c(1, 4)),
      sliderInput("observation3", label = "Observation", min = 1, 
                  max = dim(file())[1], value = c(1, (dim(file())[1])/2))
    )
  })
  
  #Obtengo la seleccion de atributos y observaciones para pca
  selectedDataPCA <- reactive({
    file()[input$observation3[1]:input$observation3[2], 
           input$attributes3[1]:input$attributes3[2]]
  })
  
  #grafico de PCA
  output$pca <- renderPlot({
    #iris.x <- iris[,1:4]
    ir.pca <- prcomp(selectedDataPCA(), center = TRUE, scale. = TRUE)
    elbowPlot(ir.pca)
  })
  
  
}

#App
shinyApp(ui, server)

