ui <- navbarPage("Neuron Data",
  tabPanel("2D Plot",
    fluidRow(
    # Sidebar panel
      column(2,
        uiOutput("plotVars"),
        fluidRow(
          column(6,
            numericInput("compNum","Compartment Number:",51,min = 1,max = 101,step = 1)
          ),
          column(6,
            numericInput("avgNum","Moving AVG Width:",20,min = 1,max = 100,step = 1)
          )
        ),
        checkboxInput("avgBool", "Turn on Moving Average", FALSE),
        checkboxInput("normal", "Normalize Y Axis", FALSE),
        textInput("color", "Color:", value = "#111111"),
        br(),
               
               # Time start and stop selection, divs were used for styling to force the two on the same line
               div(
                 style = "display: inline-block;vertical-align:center; width: 45%;",
                 numericInput(
                   "tStart",
                   "Start Time:",
                   0,
                   min = 0,
                   max = 100,
                   step = .01
                 )
               ),
               div(
                 style = "display: inline-block;vertical-align:center; width: 45%;",
                 numericInput(
                   "tStop",
                   "Stop Time:",
                   100,
                   min = 0,
                   max = 100,
                   step = .01
                 )
               ),
               br(),
        # Bottom 4 buttons of 2d plot UI
        fluidRow(
          column(6,
            actionButton("generate", "Generate")
          ),
          column(6,
            actionButton("keep", "Keep Plot")
          )
        ),
        br(),
        fluidRow(
          column(6,
            actionButton("clear", "Clear Plot")
          ),
          column(6,
            actionButton("save", "Save Plot")
          )
        )
      ),
                 
      # 2d Plot panel
      column(10,
        plotOutput("mainPlot", height = 600)
      )
    )
    # end of 2d plot tab
  ), 
  tabPanel("Profile",
    fluidRow(
      # Sidebar panel
      column(2,
        uiOutput("varProfile"),
        fluidRow(
          column(6,
            numericInput("yLowerLim", "Y Min", 0),
            numericInput("tProfileStart","Start",.005,min = .005,max = 100,step = .01)
          ),
          column(6,
            numericInput("yUpperLim", "Y Max", 1),
            numericInput("tProfileStop","End",100,min = .005,max = 100,step = .01)
          )
        ),
        numericInput("tProfileIncrement","Time Increment",.005,min = .005,max = 10,step = .005),
        checkboxInput("profileAnimate", "Turn on Animation", FALSE),
        fluidRow(
          column(6,
            actionButton("profileStart", "Start")
          ),
          column(6,
            actionButton("profileStop", "Stop")
          )
        )
      ),
      
      # profile plot panel
      column(10,
        plotOutput("profilePlot", height = 600)
      )
    )
    # end of profile tab
  ),
  
  tabPanel("3D Plot",
    fluidRow(
      # Sidebar panel
      column(3,
        fluidRow(
          column(12,
            uiOutput("plotVars3D")
          )
        ),
        fluidRow(
          # compartment picking UI
          column(4,
            numericInput("compX", "Compartment Number:",51,min = 1,max = 101,step = 1)
          ),
          column(4,
             numericInput("compY", "Compartment Number:",51,min = 1, max = 101,step = 1)
          ),
          column(4,
              numericInput("compZ", "Compartment Number:",51,min = 1,max = 101,step = 1)
          )
        ),
        fluidRow(
          column(4,
            numericInput("tStart3d", "Start Time:",.005,min = .005,max = 100,step = .1)
          ),
          column(4,
            numericInput("tStop3d", "Stop Time:",100,min = .005,max = 100,step = .1)
          )
        ),
        fluidRow(
          column(6,
            numericInput("theta", "Theta",0,min = -180,max = 180,step = 5)
          ),
          column(6,
            numericInput("phi", "Phi",0,min = -180,max = 180,step = 5)
          )
        ),
        fluidRow(
          column(6,
            numericInput("xMin", "X Min",0,min = -200,max = 100,step = .1),
            numericInput("yMin", "Y Min",0,min = -200,max = 100,step = .1),
            numericInput("zMin", "Z Min",-150,min = -200,max = 100,step = 1)
          ),
          column(6,
            numericInput("xMax", "X Max",1,min = -200,max = 100,step = .1),
            numericInput("yMax", "Y Max",1,min = -200,max = 100,step = .1),
            numericInput("zMax", "Z Max",50,min = -200,max = 100,step = 1)
          )
        ),
        checkboxInput("avgBool3d", "Turn on Moving AVG:", FALSE),
        numericInput("avgNum3d", "Moving AVG Width:",20,min = 1,max = 100,step = 1),
        checkboxInput("animateBool", "Turn on Animation:", FALSE),
        fluidRow(
          column(6,
            numericInput("timeAnimate", "Time Plotted",.5,min = .1,max = 2,step = .1)
          ),
          column(6,
            numericInput("incrementsAnimate", "Increments",.1,min = .01,max = 1,step = .01)
          )
        ),
        fluidRow(
          column(4,
            actionButton("startAnimate", "Start")
          ),
          column(4,
            actionButton("stopAnimate", "Stop")
          ),
          column(4,
            actionButton("resetAnimate", "Reset")
          )
        )
      ),
      column(9,
        # 3d plot panel
        plotOutput("plot3d", height = 600),
        fluidRow(
          column(6,
            verbatimTextOutput("startTimeAnimate")
          ),
          column(6,
            verbatimTextOutput("endTimeAnimate")
          )
        )
      )
    )
    # end of 3d tab
  ),
  tabPanel("Data Set",
    fluidRow(
      column(10,
        textInput("dataFolder", "Data Folder:",value = "./data/",width = "100%")
      ),
      column(2,
        actionButton("updateData", "Update Dataset")
      )
    ),
    fluidRow(
      verbatimTextOutput("setup")
    )
  )
) 