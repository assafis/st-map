## app.R ##
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(DT)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('psd_functions_shiny.R')

choices<-seq(1:100)
names(choices)<-seq(1:100)


psd.data<<-readRDS(paste0(dataDir,"ny_psd_points_repeat.RData")) 
#points.data<<-readRDS(paste0(dataDir,"ny_points_data_repeat.RData")) 

fractions<-substring(names(psd.data)[-c(1)],3)
names(fractions)<-substring(names(psd.data)[-c(1)],3)

eca.names<-c("ECaV.15","ECaV.075","ECaH.1","ECaH.05")
names(eca.names)<-c("ECaV.15","ECaV.075","ECaH.1","ECaH.05")

bm.files <- list.files(paste0(dataDir,"benchmark/"))
names(bm.files)<-bm.files




ui <- dashboardPage(
  
  dashboardHeader(title = "Soil texture"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Texture Triangle", tabName = "triangle", icon = icon("tree")),
      menuItem("Dataset", tabName = "dataset", icon = icon("th")),
      menuItem("Measurements", tabName = "measurements", icon = icon("ruler")),
      menuItem("Distribution", tabName = "distribution", icon = icon("lines-leaning")),
      menuItem("Correlation", tabName = "correlation", icon = icon("torii-gate")),
      menuItem("Fractions analysis", tabName = "fractions", icon = icon("galactic-republic")),
      #menuItem("Points PSD", tabName = "pointsPSD", icon = icon("map-marker")),
      menuItem("Point Histogram", tabName = "PointHist", icon = icon("chart-column")),
      menuItem("Kriging", tabName = "kriging", icon = icon("map")),
      menuItem("ECa", tabName = "ECa", icon = icon("layer-group")),
      menuItem("Models", tabName = "models", icon = icon("palette")),
      menuItem("Benchmark", tabName = "benchmark", icon = icon("chart-gantt")),
      menuItem("Corr. benchmark", tabName = "corBenchmark", icon = icon("chart-gantt")),
      
      #      menuItem("Texture - kriged", tabName = "textkrig", icon = icon("map")),
      selectInput("data.set", h5(strong("Dataset")),
                  choices = list("Sasa Gadash 4/2023"="sasa_4_23","NY 3/2025 I"="ny_3_25_I","NY 3/2025 II"="ny_3_25_II",
                                 "Beeri Gadash 9/2024"="beeri_gadash_9_24","Beeri Hohova N 12/2024"="beeri_hohova_n_12_24"),
                  selected = "sasa_4_23",width="100%"),
      selectInput("sample", h5(strong("Sample")),choices = list("QC"=1,"Grid"=2,"QCvar"=3,"All"="all"),selected=1,width="80%"), #"QC+Grid"=12,"QC+QCvar"=13,"QCVar+Grid"=23
      selectInput("horizon", h5(strong("Horizon")),choices = list("A","B"),selected = "A",width="50%"),
      selectInput("cs.limit", h5(strong("Clay/Silt limit (μm)")),choices = list(2,3,4,5,6,7,8),selected = 2,width="80%")
    ),
    useShinyjs(),  # Set up shinyjs
    #extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }")
    includeCSS("style.css")
  ),
  
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$head(tags$style(HTML('.checkbox-inline, .radio-inline {margin-bottom: 9px;}'))),
    
    tabItems(
      
      # tab content - dataset
      tabItem(tabName = "dataset",
              fluidRow(
                column(width = 12,class="alignCenter",
                       h3(strong("Soil texture analysis")),
                       h4(textOutput("cur_dataset")),
                       h5(textOutput("date_dataset")),
                       h5(textOutput("area_dataset"))
                ),
                column(width = 12,
                       h3(strong("Dataset"))
                ),
                column(width = 2,
                       h5(strong("ECa")),
                       tags$ul(
                         tags$li("Vertical: 0.75, 1.5 [m]"),
                         tags$li("Horizontal: 0.5, 1 [m]")
                       )
                ),
                column(width = 3,
                       h5(strong("PSD: silt/clay limit")),
                       tags$ul(
                         tags$li("clay3, silt3: limit=3 μm"),
                         tags$li("etc.")
                       )
                )
                
                
              ),
              fluidRow(
                column(width=12,class="alignCenter",
                       withSpinner(dataTableOutput("soilTable", height = 1200),color="#4040c0")
                )
              )
              
              
      ),
      
      
      # tab content - dataset
      tabItem(tabName = "measurements",
              fluidRow(
                column(width = 12,
                       h3(strong("Measurements"))
                )
              ),
              fluidRow(
                column(width = 1,offset =1,
                       selectInput("m.var", h5(strong("Variable")),choices =list("H","d","D","clay","silt","sand"),selected = "clay")
                ),
                # column(width = 2,
                #        radioButtons("m.depth", h5(strong("Horizon")),inline=T,
                #                     choices = 
                #                       list("All"="all","A: 0-20 cm"="A","B: 20-40 cm"="B"),
                #                     selected = "A")
                # ),
                column(width = 2,
                       radioButtons("m.sort", h5(strong("Sort by")),inline=T,
                                    choices = list("Point number"="point","Value"="value"),
                                    selected = "point")
                ),
                
                
                # column(width = 2,offset=0,
                #        selectInput("m.data", h5(strong("Data processing (repeats)")),
                #                    choices = list("raw","mean","max sand"="max.sand","max silt"="max.silt","max clay"="max.clay"),
                #                    selected = "raw")
                # ),
                column(width = 12,hr(class="mt-5"))
              ),
              
              fluidRow(
                column(width = 12,class="my-5",
                       withSpinner(plotOutput("measurePlot", height = 600),color="#a050c0")
                )
              ),
              fluidRow(
                column(width=8,class="alignCenter",offset = 2,
                       h3("Summary statistics"),
                       withSpinner(dataTableOutput("measureTable", height = 800),color="#4040c0")
                )
              )
      ),
      
      
      
      # tab content - points PSD
      tabItem(tabName = "pointsPSD",
              
              fluidRow(
                column(width = 12,
                       h3("Soil texture by points")
                )
              ),
              fluidRow(
                # column(width = 2,
                #        radioButtons("p.depth", h5(strong("Horizon")),inline=T,
                #                     choices = 
                #                       list("All"="all","A: 0-20 cm"="A","B: 40-60 cm"="B"),
                #                     selected = "A")
                # ),
                column(width = 2,offset=0,
                       selectInput("p.data", h5(strong("Data processing")),
                                   choices = list("raw","mean"),
                                   selected = "mean")
                ),
                column(width = 12,hr(class="mt-5"))
              ),
              
              fluidRow(
                column(width = 6,class="my-5",
                       column(width = 4,offset =2,
                              selectInput("p.variable1", h5(strong("Variable")),choices =list("H","d","D","clay","silt","sand"),selected = "clay")
                       ),
                       column(width = 12,
                              withSpinner(plotOutput("pointsBoxPlot1", height = 300),color="#C09050")
                       )
                ),
                column(width = 6,class="my-5",
                       column(width = 4,offset =2,
                              selectInput("p.variable2", h5(strong("Variable")),choices =list("H","d","D","clay","silt","sand"),selected = "silt")
                       ),
                       column(width = 12,
                              withSpinner(plotOutput("pointsBoxPlot2", height = 300),color="#309050")
                       )
                )
              )
              
      ),
      
      # tab content - Distribution
      tabItem(tabName = "distribution",
              
              fluidRow(
                column(width = 2,
                       h4("Particle Size Distribution")
                )
                # ,
                # column(width = 2,
                #        radioButtons("d.depth", h5(strong("Horizon")),inline=T,
                #                     choices = 
                #                       list("All"="all","A: 0-20 cm"="A","B: 20-40 cm"="B"),
                #                     selected = "A")
                # )
              ),
              
              fluidRow(
                column(width = 12,class="my-5",
                       withSpinner(plotOutput("distPlot", height = 400),color="#C09050")
                )
              ),
              
              fluidRow(
                hr()
              )
      ),
      
      
      # tab content - Histograms
      tabItem(tabName = "PointHist",
              fluidRow(
                column(width = 2,
                       # radioButtons("h.depth", h5(strong("Horizon")),inline=T,
                       #              choices = 
                       #                list("All"="all","A: 0-20 cm"="A","B: 20-40 cm"="B"),
                       #              selected = "A")
                ),
                column(width = 3,offset=0,
                       radioButtons("h.I", h5(strong("I - number of bins")),inline=T,
                                    choices = list(3, 8, 20, 40, 60, 100),
                                    selected = 100)
                )
              ),
              fluidRow(
                column(width = 12,
                       withSpinner(plotOutput("histPointplot", height = 960),color="#7DCEE1")
                )
              )
      ),
      
      # tab content - Triangle
      tabItem(tabName = "triangle",
              
              fluidRow(
                column(width = 4,
                       h3("Texture triangle")
                )
              ),
              fluidRow(
                column(width = 3,
                       # radioButtons("tri.depth", h5(strong("Horizon")),inline=T,
                       #              choices = 
                       #                list("All"="all","A: 0-20 cm"="A","B: 20-40 cm"="B"),
                       #              selected = "B")
                )
                
              ),
              fluidRow(
                hr(class="mt-5 mb-5")
              ),
              fluidRow(
                
                column(width = 1,offset=1,class="mt-5",
                       radioButtons("tri.color", h5(strong("Color by")),inline=F,
                                    choices = list("Texture class"="texture","D","clay","silt","sand","sample"),
                                    selected = "texture")
                ),
                column(width = 6,offset=0,class="my-5",
                       box(width = 12,solidHeader = T,
                           withSpinner(plotOutput("trianglePlot", height = 720),color="#C09050")
                       )
                ),
                column(width = 3,offset=0,class="my-5",
                       box(width = 12,solidHeader = T,
                           HTML('<img src="soiltexture_usda.png" style="max-width:100%;height:auto;margin:0;"/>')
                       ),
                       box(width = 12,solidHeader = T,title = "Class distribution",
                           withSpinner(plotlyOutput("textureHistPlot", height = 300),color="#C03090")
                       )
                )
                
              ),
              
              fluidRow(
                
                column(width = 8,offset=2,
                       box(width = 12,
                           h3("Sand ~ Clay"),
                           withSpinner(plotOutput("diagramPlot", height = 720),color="#C09050")
                       )
                )
              )
              
              
              
      ),
      
      
      # Tab - Correlation
      tabItem(tabName = "correlation",
              fluidRow(
                column(width = 2,
                       h2("Correlation")
                ),
                column(width = 2,
                       # radioButtons("c.depth", h5(strong("Horizon")),inline=T,
                       #              choices = list("All"="all","A: 0-20 cm"="A","B: 20-40 cm"="B"),
                       #              selected = "A")
                ),
                column(width = 2,offset=0,
                       # selectInput("c.data", h5(strong("Data processing")),
                       #             choices = list("raw","mean"),
                       #             selected = "mean")
                )
              ),
              
              fluidRow(
                column(width = 1,offset = 1,
                       radioButtons("xitem", h4("X item"),
                                    choices = 
                                      list("Clay"="clay","Silt"="silt","Sand"="sand","H","d","D",
                                           "ECaV 1.5m"="ECaV.15","ECaV 0.75m"="ECaV.075","ECaH 1m"="ECaH.1","ECaH 0.5m"="ECaH.05"),
                                    selected = "sand")
                ),
                column(width = 7,
                       h2(),
                       h2(),
                       withSpinner(plotOutput("corplot", height = 640),color="#E46726")
                ),
                column(width = 2,
                       radioButtons("yitem", h4("Y item"),
                                    choices = 
                                      list("Clay"="clay","Silt"="silt","Sand"="sand","H","d","D",
                                           "ECaV 1.5m"="ECaV.15","ECaV 0.75m"="ECaV.075","ECaH 1m"="ECaH.1","ECaH 0.5m"="ECaH.05"),
                                    selected = "ECaH.05")
                )),
              fluidRow(
                column(width = 12,class="alignCenter mt-5",
                       h3("Correlation Matrix")
                )),
              fluidRow(
                column(width = 2,offset = 1,
                       
                       box(width=12,
                           checkboxGroupInput("cgvars", label = h4("Variables:"),
                                              choices = 
                                                list("Clay"="clay","Silt"="silt","Sand"="sand","H","d","D",
                                                     "ECaV 1.5m"="ECaV.15","ECaV 0.75m"="ECaV.075","ECaH 1m"="ECaH.1","ECaH 0.5m"="ECaH.05"),
                                              selected = c("clay","silt","sand","H","d","D","ECaV.15","ECaV.075","ECaH.1","ECaH.05"))
                       )
                ),
                
                column(width = 7,
                       withSpinner(plotOutput("cgramplot", height = 640),color="#6D9EC1"),
                       h2(""),
                       column(width = 4,
                              radioButtons("mtd", h4("Display:"),inline=T,
                                           choices = list("Square"="square","Circle"="circle"),
                                           selected = "square")
                       ),
                       column(width = 4,
                              radioButtons("labs", h4("Label size:"),inline=T,
                                           choices = list(2,3,4,5,6),
                                           selected = 4)
                       ),
                       column(width = 4,
                              radioButtons("axsize", h4("Axis text size:"),inline=T,
                                           choices = list(10,11,12,13,14,15),
                                           selected = 12)
                       )
                )
              ),
              fluidRow(
                column(width=8,class="alignCenter",offset = 2,
                       h3("p values"),
                       withSpinner(dataTableOutput("pTable", height = 900),color="#4040c0")
                )
                ),
              fluidRow(
                column(width = 12,class="alignCenter mt-5",
                       h3("ECa ~ PSD")
                )),
              fluidRow(
                column(width = 2,offset = 1,
                       box(width = 12,
                           selectInput("ep.eca", h5(strong("ECa layer")),
                                       choices = eca.names,
                                       selected = "ECaH.1")
                       )
                ),
                column(width = 7,
                       withSpinner(plotOutput("ep.cor.plot", height = 480),color="#6D9EC1"),
                )
              )
              
      ),
      
      
      
      # tab content - Fractions analysis
      tabItem(tabName = "fractions",
              
              fluidRow(
                column(width = 12,
                       h3("Particle size fractions analysis")
                )
              ),
              fluidRow(
                # column(width = 2,offset = 1,
                #        radioButtons("f.depth", h5(strong("Horizon")),inline=T,
                #                     choices = 
                #                       list("All"="all","A: 0-20 cm"="A","B: 20-40 cm"="B"),
                #                     selected = "A")
                # ),
                column(width = 12,hr(class="mt-5"),h2(""))
              ),
              
              
              fluidRow(
                column(width = 2,
                       box(width = 10,
                           selectInput("f.xitem", h5(strong("ECa layer")),
                                       choices = eca.names,
                                       selected = "ECaV.15")
                       ),
                       box(width = 10,
                           selectInput("f.fraction", h5(strong("Fraction")),
                                       choices = fractions,
                                       selected = "2.421")
                       )
                ),
                
                column(width = 8,
                       withSpinner(plotOutput("fractionsPlot", height = 360),color="#A05090")
                )
              ),
              
              fluidRow(
                column(width = 12,h4("Correlation matrix: ECa ~ PSD")),class="alignCenter",
                column(width = 12,offset = 0,
                       withSpinner(dataTableOutput("fractionsSgramTable", height = 180),color="#70a0F0")
                )
              ),
              
              fluidRow(
                column(width = 12,h4("Correlation matrix plot: ECa ~ PSD")),class="alignCenter",
                column(width = 12,
                       withSpinner(plotOutput("fractionsSgramPlot", height = 240),color="#107010")
                )
              ),
              
              fluidRow(
                column(width = 12,h5("Highest correlation ECa ~ particle size [µm]"),class="alignCenter"),
                column(width = 4,offset = 4,
                       withSpinner(verbatimTextOutput("fractionsTableTop"),color="#50F0B0")
                )
              ),
              
              
      ),
      
      
      # Kriging tab content
      tabItem(tabName = "kriging",
              fluidRow(
                column(width = 4,
                       h4(strong("Kriging interpolation"))
                )
              ),
              
              fluidRow(
                # column(width = 2,
                #        radioButtons("k.depth", h5(strong("Horizon")),inline=T,
                #                     choices = 
                #                       list("All"="all","A: 0-20 cm"="A","B: 40-60 cm"="B"),
                #                     selected = "A")
                # ),
                # column(width = 2,offset=0,
                #        selectInput("k.data", h5(strong("Data processing")),
                #                    choices = list("raw","mean","max sand"="max.sand","max silt"="max.silt","max clay"="max.clay"),
                #                    selected = "mean")
                # ),
                
                column(width = 2,
                       selectInput("kitem", h4("Variable"),
                                   choices = list("Clay"="clay","Silt"="silt","Sand"="sand","D"),
                                   selected = "clay")
                ),
                
                column(width = 2, 
                       radioButtons("gridres", h4("Grid resolution"),inline=T,
                                    choices = list("10m"=10,"1m"=1),
                                    selected = 10)
                )
              ),
              
              fluidRow(
                column(width = 4,
                       h5("spatial model"),
                       withSpinner(plotOutput("varPlot", height = 400),color="#0dc5c1"),
                       
                       h5("sampling points distribution"),
                       withSpinner(plotOutput("kHistPlot", height = 350),color="#2d75C1")
                ),
                column(width = 8,
                       box(id="maink",width=12,
                           withSpinner(plotOutput("krigPlot", height = 800),color="#9030B0"),
                           div(id="dlbutton.krig",
                               downloadButton("dlRaster.krig", "Download Raster")
                           )
                       )
                       
                )
              )
      ),
      
      
      
      
      # Second tab content
      tabItem(tabName = "ECa",
              fluidRow(
                column(width = 12,
                       h3("ECa")
                )
              ),
              fluidRow(
                column(width = 12,
                       withSpinner(plotOutput("eca.plot", height = 800),color="#9030B0")
                )
              )
      ),
      
      
      # Models tab content
      tabItem(tabName = "models",
              fluidRow(
                column(width = 9,
                       h3("Spatial Prediction of target variable"),
                       h4("ML model fitted using subset of kriged response variable and feature layers, then predicted on entire plot.")
                )
              ),
              fluidRow(
                column(width = 12, 
                       hr(),
                       h2(" ")
                )
              ),
              fluidRow(
                column(width = 5,
                       selectInput("mdv", h3("Response Variable"),width="60%",
                                   choices = list("Soil texture class"="texture","D","clay","silt","sand"),selected = "texture")
                ),
                column(width = 7,
                       checkboxGroupInput("miv", label=h3("Features"), inline=T,
                                          choices =  list("ECaV 1.5m"="ECaV_1m","ECaV 0.75m"="ECaV_05m","ECaH 1m"="ECaH_1m","ECaH 0.5m"="ECaH_05m"),
                                          selected = c("ECaV_1m","ECaV_05m","ECaH_1m","ECaH_05m"))
                ),
                hr()
              ),
              
              fluidRow(
                column(width = 5,
                       h4("Sample points"),
                       withSpinner(plotOutput("mpointsplot", height = 540),color="#70D030"),
                       # withSpinner(plotOutput("mpointshist", height = 250),color="#C03090"),
                       # h4("Variogram"),
                       # withSpinner(plotOutput("mvarplot", height = 300),color="#0dc5c1"),
                       # h4("Kriged"),
                       # withSpinner(plotOutput("mkrigplot", height = 350),color="#C09030"),
                       # h4("Krigging variance"),
                       # withSpinner(plotOutput("m.variance.plot", height = 350),color="#b0f070")
                       
                ),
                column(width = 7,
                       box(width=12,title=h4("Input layers"),
                           withSpinner(plotOutput("ivplot", height = 800),color="#603090")
                       )
                       # box(width=12,title=h4("IV Variance"),
                       #     withSpinner(plotOutput("iv.var.plot", height = 720),color="#603090")
                       # ),
                       # box(width=12,title=h4("ECa variance filter"),style="text-align:center;",
                       #     column(width = 12,
                       #            radioButtons("iv.var.level", h5(strong("Include variance under:")),inline=T,
                       #                         choices = list("All"="all","10%"=10,"15%"=15,"20%"=20,"50%"=50),
                       #                         selected = 20)
                       #     ),
                       #     column(width = 12,
                       #      withSpinner(plotOutput("iv.var.uni.plot", height = 720),color="#603090")
                       #     )
                       # )
                )),
              
              
              fluidRow(
                column(width=12,
                       h3("Interpolate / Reduce by variance")
                ),
                column(width=12,
                       column(width=6,offset = 1,
                              radioButtons("m.var.limit", h5(strong("Variance limit factor:")),inline=T,
                                           choices = list(5,10,20,50,100,150,200,300,500), selected = 100)
                       ),
                       column(width=5,
                              radioButtons("mdv.smooth", h5(strong("Smoothing (mean) window size:")),inline=T,
                                           choices = list("None"=0,3,5,7,9,15), selected = 0)
                       ),
                       column(width=8,offset = 4,h4(strong("Actual training/test set"))),
                       column(width=5,
                              box(width=12,title=h4("Response variable"),solidHeader=T,
                                  withSpinner(plotOutput("mdv.plot", height = 640),color="#303070")
                              )
                       ),
                       column(width=7,
                              box(width=12,title=h4("Features, reduced by response's variance"),solidHeader=T,
                                  withSpinner(plotOutput("iv.vareduce.plot", height = 800),color="#3000F0")
                              )
                       )
                )
              ),
              
              fluidRow(
                hr(),
                column(width = 12, 
                       h2("Prediction Model settings"),
                       column(width = 12,
                              h4("Algorithm: Random Forest"),
                              #  tags$a(href="https://www.rdocumentation.org/packages/randomForest/", target="_blank", "[R package]")
                       ),
                       column(width = 2,
                              selectInput("ntree", h5(strong("Number of trees")),width="70%",
                                          choices = list("None"=1,2,5,10,20,50,100),
                                          selected = 1)
                       ),
                       column(width = 3,
                              selectInput("ncv", h5(strong("Number of cross-validation folds")),width="60%",
                                          choices = list(2,5,10,20,30),
                                          selected = 2)
                       ),
                       column(width = 3,
                              radioButtons("smooth.w", h5(strong("Smoothing window size (px)")),inline=T,
                                           choices = list("None"=0,"3x3"=3,"5x5"=5,"7x7"=7,"9x9"=9,"15x15"=15),
                                           selected = 0)
                       ),
                       column(width = 2,
                              selectInput("v.set", h5(strong("Validation set ",em("Ground truth"),":")),
                                          choices = list("QC"=1,"Grid"=2,"QCvar"=3,"Two other samples"=0),
                                          selected = 2)
                       ),
                       column(width =1,
                              selectInput("v.sid", h5(strong("Seed")),
                                          choices = list(123,234,345,456,567,678,789,890,901,012),
                                          selected = 567)
                       )
                )
              ),
              fluidRow(
                hr(),
                column(width = 12,
                       h3("Output"),
                       box(id="mainm",width=12,title = h4("Pixel-by-pixel prediction"),
                           withSpinner(plotOutput("modelplot", height = 920),color="#9030B0")
                       )
                )
              ),
              fluidRow(
                column(width = 12,id="Dtexture",
                       h3("D ➛ Soil Texture class"),
                       box(id="m.Dtexture",width=12,
                           withSpinner(plotOutput("m.Dtext.plot", height = 920),color="#50C0C0")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       div(id="dlbutton",
                           downloadButton("dlRasterPredict", "Download Raster")
                       )                
                )
                
              )
      ), # tab
      
      
      # Tab - Benchmark
      tabItem(tabName = "benchmark",
              fluidRow(
                column(width = 3,
                       h2("Models Benchmark")
                )
              ),
              fluidRow(
                column(width = 3,
                       selectInput("bm.file", h5(strong("File")),choices = bm.files,width="80%")
                ),
                column(width = 1,
                       selectInput("bm.depth", h5(strong("Depth")),choices=list("All"="all","A: 0-20 cm"="A","B: 40-60 cm"="B"),selected="all")
                ),
                column(width = 1,
                       selectInput("bm.sample", h5(strong("Sample")),choices=list("All"="all","QC","Grid","QCvar"),selected="all")
                ),
                column(width = 2,
                       selectInput("bm.response", h5(strong("Response variable")),choices=list("All"="all","Texture"="texture","D"),selected="all")
                ),
              ),
              fluidRow(
                column(width = 2,offset = 0,
                       radioButtons("bm.xitem", h4("X item"),
                                    choices = 
                                      list("Sample"="uid","Plot"="plot","Response variable"="response","Number of trees"="ntrees","Variance factor"="var.factor","Clay/Silt limit"="cs.limit","Depth"="depth","Seed"="sid","Validation set"="validation.set"),
                                    selected = "uid")
                ),
                column(width = 8,
                       h2(),
                       h2(),
                       withSpinner(plotOutput("bench.plot", height = 480),color="#E46726")
                ),
                column(width = 2,
                       radioButtons("bm.yitem", h4("Y item"),
                                    choices = 
                                      list("Validation accuracy ind. (Sasa)"="acc.v","Validation accuracy (own)"="acc.v.own","Training/Test accuracy"="acc.tr","Runtime"="runtime"),
                                    selected = "acc.v")
                )
              ),
              fluidRow(
                column(width=4,offset = 4,class="alignCenter",
                       h2(" "),
                       h4(textOutput("bench.title"),class="alignCenter"),
                       withSpinner(dataTableOutput("bench.table", height = 280),color="#4040c0")
                )
              ),
              
              fluidRow(
                column(width=12,offset = 0,class="alignCenter",
                       h3("Top instances"),
                       withSpinner(dataTableOutput("bench.table.top", height = 840),color="#4080e0")
                )
              )
              
      ), # /tab
      
      
      
      # Tab - Benchmark
      tabItem(tabName = "corBenchmark",
              fluidRow(
                column(width = 3,
                       h2("Correlation Benchmark"),
                       h3("Sasa")
                )
              ),
              fluidRow(
                column(width = 1,
                       selectInput("bmc.depth", h5(strong("Depth")),choices=list("All"="all","A: 0-20 cm"="A","B: 40-60 cm"="B"),selected="all")
                ),
                column(width = 1,
                       selectInput("bmc.sample", h5(strong("Sample")),choices=list("All"="all","QC","Grid","QCvar"),selected="all")
                ),
                column(width = 2,
                       selectInput("bmc.display", h5(strong("Display by")),
                                   choices=list("Clay/Silt limit"="cs.limit","Depth"="depth","Sample"="sample","Sample+depth"="sample.depth","UID"="cs.limit"),
                                   selected="uid")
                )
              ),
              fluidRow(
                column(width = 2,offset = 0,
                       radioButtons("bmc.xitem", h4("X item"),
                                    choices = list("Clay"="clay","Silt"="silt","Sand"="sand","D"="D"),
                                    selected = "clay")
                ),
                column(width = 8,
                       h2(),
                       h2(),
                       withSpinner(plotOutput("bench.cor.plot", height = 320),color="#E46726")
                ),
                column(width = 2,
                       radioButtons("bmc.yitem", h4("Y item"),
                                    choices = c(eca.names,"all"),selected = "ECaH.1")
                )
              ),
              fluidRow(
                column(width=4,offset = 4,class="alignCenter",
                       h2(" "),
                       h4(textOutput("bench.cor.title"),class="alignCenter"),
                       withSpinner(dataTableOutput("bench.cor.table", height = 280),color="#4040c0")
                )
              ),
              
              fluidRow(
                column(width=12,offset = 0,class="alignCenter",
                       h3("Top instances"),
                       withSpinner(dataTableOutput("bench.cor.table.top", height = 840),color="#4080e0")
                )
              )
              
      ), # /tab
      
      
      
      
      #  tab content
      tabItem(tabName = "textkrig",
              fluidRow(
                column(width = 12,
                       h3("Soil Texture by Kriging")
                )
              ),
              fluidRow(
                
              ),
              fluidRow(
                column(width = 10,offset=1,
                       withSpinner(plotOutput("textkrig.plot", height = 800),color="#9030B0"),
                       div(id="dlbuttonText",
                           downloadButton("dlRasterTexture", "Download Raster")
                       )       
                ) 
              )
      )
      
      
      
      
    )
  )
) # ui




withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}




server <- function(input, output) {
  #set.seed(122)
  shinyjs::hide(id="Dtexture")
  
  # gradual load perimeter files
  observeEvent(input$data.set, {
    shinyjs::hide(id="sample")
    shinyjs::hide(id="horizon")
    shinyjs::hide(id="v.set")
    if(input$data.set=="beeri_gadash_9_24"){
      if(!exists("perimeter.be.gadash.1")){ 
        perimeter.be.gadash.1 <<- sf::st_read(paste0(dataDir,"perimeter_gadash_1.shp"))
        perimeter.df.be.gadash.1<<-as(perimeter.be.gadash.1, 'Spatial')
      }
      shinyjs::show(id="horizon")
    } else {
      if(input$data.set=="beeri_hohova_n_12_24"){
        if(!exists("perimeter.be.hohova.n")){ 
          perimeter.be.hohova.n <<- sf::st_read(paste0(dataDir,"perimeter_hohova_n.shp"))
          perimeter.df.be.hohova.n<<-as(perimeter.be.hohova.n, 'Spatial')
        }
      }
      if(input$data.set=="sasa_4_23"){
        shinyjs::show(id="horizon")
        shinyjs::show(id="sample")
        shinyjs::show(id="v.set")
        if(!exists("perimeter.sasa")){ 
          perimeter.sasa <<- sf::st_read(paste0(dataDir,"perimeter_sasa.shp"))
          perimeter.df.sasa<<-as(perimeter.sasa, 'Spatial')
        }
      }
    }
    
  })
  
  
  
  # Models D chart
  observeEvent(input$mdv, {
    if(input$mdv=="D"){
      shinyjs::show(id="Dtexture")
    } else {
      shinyjs::hide(id="Dtexture")
    }
  })
  
  
  ###
  # Table from CSV file
  
  #-----------------------------------------------------------------------------
  #  render data table
  #-----------------------------------------------------------------------------
  output$soilTable <- renderDT(
    getPSData(data.set=input$data.set,depth=input$horizon,sample=input$sample),
    options = list(class = "display nowrap compact", # style
                   pageLength = 100,paging = FALSE,scrollX = 500,scrollY = 720
    )
  )
  
  output$cur_dataset <- renderText({
    switch (input$data.set,
            "ny_3_25_I" = "Neve Ya'ar - Model Farm",
            "ny_3_25_II" = "Neve Ya'ar - Model Farm (repeat)",
            "beeri_gadash_9_24" = "Beeri Gadash",
            "beeri_hohova_n_12_24" = "Beeri Hohova North (Regenerative)",
            "sasa_4_23"="Sasa Gadash",
            stop("Invalid input")
    )
  })
  
  output$date_dataset <- renderText({
    switch (input$data.set,
            "ny_3_25_I" = "3/2025",
            "ny_3_25_II" = "3/2025",
            "beeri_gadash_9_24" = "9/2024",
            "beeri_hohova_n_12_24" = "12/2024",
            "sasa_4_23"="4/2023",
            stop("Invalid input")
    )
  })
  
  output$area_dataset <- renderText({
    switch (input$data.set,
            "ny_3_25_I" = "10 Ha",
            "ny_3_25_II" = "10 Ha",
            "beeri_gadash_9_24" = "5 Ha",
            "beeri_hohova_n_12_24" = "2.5 Ha",
            "sasa_4_23"="6.5 Ha",
            stop("Invalid input")
    )
  })
  
  # Points PSD
  output$pointsBoxPlot1 <- renderPlot({
    pBox(data.type=input$p.data,vi=input$p.variable1,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$pointsBoxPlot2 <- renderPlot({
    pBox(data.type=input$p.data,vi=input$p.variable2,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  
  
  # Correlation  
  output$corplot <- renderPlot({
    gCorrs(xitem=input$xitem, yitem=input$yitem,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$cgramplot <- renderPlot({
    gCgram(items=input$cgvars,labs=input$labs,mtd=input$mtd,axsize=input$axsize,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })  
  
    output$pTable <- renderDT(
    gCgram(pout="p",items=input$cgvars,labs=input$labs,mtd=input$mtd,axsize=input$axsize,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample),
    options = list(class = "display nowrap compact", # style
                   pageLength = 10,paging = FALSE,scrollX = 500,scrollY = 200
    )
  )
  
  output$ep.cor.plot <- renderPlot({
    epCor(eca.layer=input$ep.eca,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  # output$histBinsplot <- renderPlot({
  #   HistBinsPlot(i.bins=input$ibins,plot="hist",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  # })
  
  output$histPointplot <- renderPlot({
    HistPointPlot(I=input$h.I,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  
  
  
  
  # fractions
  output$fractionsPlot <- renderPlot({
    pFractions(xitem=input$f.xitem,fr=input$f.fraction,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$fractionsSgramPlot <- renderPlot({
    pFractionsSgram(pout="plot",data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$fractionsSgramTable <- renderDT(
    pFractionsSgram(pout="table",data.set=input$data.set,depth=input$horizon,sample=input$sample),
    options = list(class = "display nowrap compact", # style
                   pageLength = 4,paging = FALSE,scrollX = 500,scrollY = 200
    )
  )
  
  output$fractionsTableTop <- renderText(
    pFractionsSgram(pout="tableTop",data.set=input$data.set,depth=input$horizon,sample=input$sample)
  )
  
  output$measurePlot <- renderPlot({
    pMeasures(var=input$m.var,sortby=input$m.sort,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$measureTable <- renderDT(
    pMeasures(pout="table",var=input$m.var,sortby=input$m.sort,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample),
    options = list(class = "display nowrap compact", # style
                   pageLength = 4,paging = FALSE,scrollX = 500,scrollY = 200
    )
  )
  
  
  
  # Triangle  
  output$trianglePlot <- renderPlot({
    psdTriangle(to.plot="tri",color=input$tri.color,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$textureHistPlot <- renderPlotly({
    psdTriangle(to.plot="hist",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$diagramPlot <- renderPlot({
    psdTriangle(to.plot="diagram",color=input$tri.color,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  
  # Distribution
  output$distPlot <- renderPlot({
    psdDistribution(cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  
  # Kriging  
  output$varPlot <- renderPlot({
    psdKrige(kitem=input$kitem,toPlot="var",resol=input$gridres,data.type="mean",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$kHistPlot <- renderPlot({
    psdKrige(kitem=input$kitem,toPlot="hist",resol=input$gridres,data.type="mean",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })
  
  output$krigPlot <- renderPlot({
    psdKrige(kitem=input$kitem,toPlot="krig",resol=input$gridres,data.type="mean",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })  
  
  
  output$eca.plot <- renderPlot({
    ECaPlots(data.set = input$data.set)
  })  
  
  
  
  
  
  # Prediction Models
  output$mdesc <- renderUI({
    HTML(varDesc(item=input$mdv) )
  })  
  
  output$mpointsplot <- renderPlot({
    gModelsPoints(mdv=input$mdv,toPlot="var",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })  
  
  output$mpointshist <- renderPlot({
    gModelsPoints(mdv=input$mdv,toPlot="hist",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })  
  
  output$mvarplot <- renderPlot({
    gModelsKrig(mdv=input$mdv,toPlot="var",cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
    
  })  
  
  output$mkrigplot <- renderPlot({
    gModelsKrig(mdv=input$mdv,toPlot="krig",var.factor=1,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })  
  
  output$m.variance.plot <- renderPlot({
    gModelsKrig(mdv=input$mdv,toPlot="variance",var.factor=1,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })  
  
  output$mdv.plot <- renderPlot({
    gModelsKrig(mdv=input$mdv,toPlot="dataset",var.factor=input$m.var.limit,mdv.smooth.w=input$mdv.smooth,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample)
  })  
  
  output$modelplot <- renderPlot({
    gModels(mdv=input$mdv,miv=input$miv,toPlot="model",ntree=input$ntree,ncv=input$ncv,var.factor=input$m.var.limit,smooth.w=input$smooth.w,mdv.smooth.w=input$mdv.smooth,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample,v.set=input$v.set,sid=input$v.sid)
  })
  
  output$m.Dtext.plot <- renderPlot({
    gModels(mdv=input$mdv,miv=input$miv,toPlot="model",ntree=input$ntree,ncv=input$ncv,var.factor=input$m.var.limit,smooth.w=input$smooth.w,D2text=T,mdv.smooth.w=input$mdv.smooth,cs.limit=input$cs.limit,data.set=input$data.set,depth=input$horizon,sample=input$sample,v.set=input$v.set,sid=input$v.sid)
  })  
  
  # Download current model prediction raster - Texture
  output$dlRasterPredict <- downloadHandler(
    filename = function() {
      #paste(input$dataset, ".csv", sep = "")
      paste0(input$data.set,"_predicted_texture.tif")
    },
    content = function(file) {
      # write.csv(datasetInput(), file, row.names = FALSE)
      file.copy(paste0(dataDir,"krigResults/",input$data.set,"_predicted_texture.tif"), file)
      print(paste0(dataDir,"krigResults/",input$data.set,"_predicted_texture.tif"))
    }
  )
  
  output$ivplot <- renderPlot({
    #print(paste0("ny_predicted_",input$mdv,"_",input$mod.depth,".tif"))
    #print(paste0("/home/ai/Documents/Data/ny/krigResults/ny_predicted_",input$mdv,"_",input$mod.depth,".tif"))
    gModelsIV(miv=input$miv,data.set=input$data.set)
  })  
  
  output$iv.vareduce.plot <- renderPlot({
    gModelsIVareduce(miv=input$miv,var.factor=input$m.var.limit,mdv=input$mdv,data.set=input$data.set,depth=input$horizon,sample=input$sample,cs.limit=input$cs.limit)
  })  
  
  #output$iv.var.plot <- renderPlot({ gModelsIV(miv=input$miv,var.t=T) })
  #output$iv.var.uni.plot <- renderPlot({ gModelsIVvar(miv=input$miv,var.level=input$iv.var.level) })
  
  
  
  
  ### Benchmark
  output$bench.plot <- renderPlot({
    gBenchmark(bm.file=input$bm.file,xitem=input$bm.xitem, yitem=input$bm.yitem,sample=input$bm.sample,depth=input$bm.depth,response=input$bm.response)
  })
  
  output$bench.title <- renderText({
    paste0(yitem=input$bm.yitem," by ",xitem=input$bm.xitem)
  })
  
  output$bench.table <- renderDT(
    gBenchmark(bm.file=input$bm.file,xitem=input$bm.xitem, yitem=input$bm.yitem,output="table",sample=input$bm.sample,depth=input$bm.depth,response=input$bm.response),
    options = list(class = "display nowrap compact", # style
                   pageLength = 100,paging = FALSE,scrollX = 500,scrollY = 420,searching = FALSE
    )
  )
  
  output$bench.table.top <- renderDT(
    gBenchmark(bm.file=input$bm.file,xitem=input$bm.xitem, yitem=input$bm.yitem,output="table.top",sample=input$bm.sample,depth=input$bm.depth,response=input$bm.response),
    options = list(class = "display nowrap compact", # style
                   pageLength = 100,paging = FALSE,scrollX = 500,scrollY = 720,searching = FALSE
    )
  )
  
  
  ### Correlation Benchmark
  output$bench.cor.plot <- renderPlot({
    gCorrBenchmark(display.c=input$bmc.display,xitem=input$bmc.xitem,yitem=input$bmc.yitem,sample=input$bmc.sample,depth=input$bmc.depth)
  })
  
  output$bench.cor.title <- renderText({
    paste0(yitem=input$bmc.yitem," by ",xitem=input$bmc.xitem)
  })
  
  output$bench.cor.table <- renderDT(
    gCorrBenchmark(display.c=input$bmc.display,xitem=input$bmc.xitem, yitem=input$bmc.yitem,output="table",sample=input$bmc.sample,depth=input$bmc.depth),
    options = list(class = "display nowrap compact", # style
                   pageLength = 100,paging = FALSE,scrollX = 500,scrollY = 420,searching = FALSE
    )
  )
  
  output$bench.cor.table.top <- renderDT(
    gCorrBenchmark(display.c=input$bmc.display,xitem=input$bmc.xitem, yitem=input$bmc.yitem,output="table.top",sample=input$bmc.sample,depth=input$bmc.depth),
    options = list(class = "display nowrap compact", # style
                   pageLength = 100,paging = FALSE,scrollX = 500,scrollY = 720,searching = FALSE
    )
  )
  
  
  
  # Texture by Kriging
  output$textkrig.plot <- renderPlot({ textKrige() })
  
  output$dlRasterTextureK <- downloadHandler(
    filename = function() {
      #paste(input$dataset, ".csv", sep = "")
      paste0("texture_kriged.tif")
    },
    content = function(file) {
      # write.csv(datasetInput(), file, row.names = FALSE)
      file.copy(paste0(dataDir,"krigResults/texture_kriged.tif"), file)
    }
  )
  
  
  
  # print console output
  # observe({
  #   withConsoleRedirect("console", {
  #     if(input$cSwitch=="clusters"){
  #       #print(clusters(nclust=input$nclust,anclayers=input$anclayers))
  #     } else if(input$cSwitch=="indices"){
  #       #print(clustersValidity(anclayers=input$anclayers))
  #     }
  #   })  
  # })
  
  
  
  
  
  
  
  # Download current kriging raster
  output$dlRaster.krig <- downloadHandler(
    filename = function() {
      #paste(input$dataset, ".csv", sep = "")
      paste0(input$data.set,"_",input$kitem,"_kriged.tif")
    },
    content = function(file) {
      # write.csv(datasetInput(), file, row.names = FALSE)
      file.copy(paste0(dataDir,"krigResults/",input$data.set,"_",input$kitem,"_kriged.tif"), file)
    }
  )
  
  
  
  
  
}

shinyApp(ui, server)
