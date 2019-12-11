#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library("plot3D")
library(scales)
source("UsefulFunctions.R")
source("loadDataDynamic.R")

# define data and plot
dataDir <- "/mnt/c/Users/Joey/Desktop/InternalElectrode/Only Flanking Stim (EXTRA ONSET)/"
keepPlot <- ggplot()    # Keeps current saved plot
initData(dataDir)

# MAIN PAGE FUNCTIONS (2D Plot)
generateVector <- function(vectorVar, nodeNumString, tStartString, tStopString, avgBool, avgNumString, respectTo){
    # generate the vector from DATA given the input parameters
    nodeNum = as.numeric(nodeNumString)
    tStart = as.numeric(tStartString)
    tStop = as.numeric(tStopString)
    avgNum = as.numeric(avgNumString)
    if(vectorVar == "Time"){
        vec <- getTimeStep(tStart, tStop)
    }else{
        vec <- getVarTimeBound(vectorVar, nodeNum, tStart, tStop)
    }
    
    if(!missing(respectTo)){
        if(respectTo == "Time"){
            vecRespect = getTimeStep(tStart,tStop)
        }else{
            vecRespect = getVarTimeBound(vectorVar, nodeNum, tStart, tStop)
        }
        vec <- calcDiff(vecRespect, vec)
    }
    
    if(avgBool){
        vec <- movingAverage(vec, avgNum)
    }

    vec
} 

generatePlot <- function(settings){
    # settings is a vector containing the following strings
    # [1]   xvar (v=Voltage, m=MGate, etc)
    # [2]   yvar same as xvar
    # [3]   node number
    # [4]   tStart 
    # [5]   tStop
    # [6]   color
    # [7]   avgNum
    # [8]   avgBool
    # [9]   deriv, same as xvar, "N/A"=nothing (ie show raw y var)
    # [10]  normalize T/F

    cat("Current Settings: ")
    cat(settings)
    cat("\n")

    plot <- keepPlot   # Build plot from last stored plot

    # Generate x axis data
    xData <- generateVector(settings[1], settings[3], settings[4], settings[5], settings[8], settings[7])
    
    # Generate y axis data
    if(settings[9] == "N/A"){
        yData <- generateVector(settings[2], settings[3], settings[4], settings[5], settings[8], settings[7])
    }else{
        # There is a partial derivative being displayed
        yData <- generateVector(settings[2], settings[3], settings[4], settings[5], settings[8], settings[7], settings[9])
        # we will throw away the first and last index of both x and y data because of 
        #   how the deriv is calculated
        yData <- yData[-1]
        xData <- xData[-1]
        yData <- yData[-length(yData)]
        xData <- xData[-length(xData)]
    }
    
    if(settings[10]) yData <- rescale(yData)

    plot <- plot+geom_path(aes(x=xData,y=yData), color=settings[6], size=1)+
                xlab(settings[1])+ylab(settings[2])+
                ggtitle(sprintf("%s vs %s",settings[2],settings[1]))
        
    plot
}

generateName <- function(settings){
    # Generate file name for a given plot settings
    # Using the form "<variable>_<nodeNum>.png"
    newName <- sprintf("%s_vs_%s_Node%s.png",settings[2],settings[1],settings[3])
    newName
}

# PROFILE PLOT FUNCTIONS
generateProfile <- function(tProfile, variable, yMin, yMax){
    # Generate the profile plot (V vs node) at a given time
    vData <- getVarRow(variable, tProfile*200+1)
    x <- 1:length(vData)
    plot <- ggplot() + geom_path(aes(x=x,y=vData), color="#EE2222", size=.5)+
                geom_point(aes(x=x,y=vData), color="#EE2222",size=2)+
                xlab("Node")+ylab(variable)+ylim(yMin,yMax)+
                ggtitle(sprintf("%s @t=%f",variable, tProfile*200))
    plot
}

# 3D PLOT FUNCTIONS
generate3D <- function(nodeNum, tStart, tStop, avgBool, avgNum, theta, phi, mlims, hlims, vlims){
    # generate a 3d plot with x=m, y=h, z=v
    x <- generateVector("MGate", nodeNum, tStart, tStop, avgBool, avgNum)
    y <- generateVector("HGate", nodeNum, tStart, tStop, avgBool, avgNum)
    z <- generateVector("Voltage", nodeNum, tStart, tStop, avgBool, avgNum)
    
    plot <- scatter3D(x,y,z, 
                      xlab="m",ylab="h",zlab="v",
                      axis.scales=FALSE, xlim=mlims, ylim=hlims, zlim=vlims,
                      bty="g", theta=as.numeric(theta), phi=as.numeric(phi), type="l", ticktype="detailed", lwd=2)
    plot
}

ui <- navbarPage("Neuron Data",
    tabPanel("2D Plot",
        fluidRow(
            # Sidebar panel
            column(2,
                uiOutput("plotVars"),
                fluidRow(
                    column(6,
                        numericInput("nodeNum", "Node Number:", 51, min=1, max=101, step=1)
                    ),
                    column(6,
                        numericInput("avgNum", "Moving AVG Width:", 20, min=1, max=100, step=1)
                    )
                ),
                checkboxInput("avgBool", "Turn on Moving Average", FALSE),
                checkboxInput("normal", "Normalize Y Axis", FALSE),
                textInput("color", "Color:", value = "#111111"),
                br(),
               
                # Time start and stop selection, divs were used for styling to force the two on the same line
                div(style="display: inline-block;vertical-align:center; width: 45%;",numericInput("tStart", "Start Time:", 0, min=0, max=100, step=.01)),
                div(style="display: inline-block;vertical-align:center; width: 45%;",numericInput("tStop", "Stop Time:", 100, min=0, max=100, step=.01)),
                br(),
                
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

            # Main panel
            column(10,
                plotOutput("mainPlot", height=600)
            )
        )
    ),
    tabPanel("Profile",
        fluidRow(
            # Sidebar panel
            column(2,
                numericInput("tProfile", "Time", 0, min=.005, max=100, step=.005),
                uiOutput("varProfile"),
                column(6,
                    numericInput("yLowerLim", "Y Min", 0)
                ),
                column(6,
                    numericInput("yUpperLim", "Y Max", 1)
                )
            ),
            column(10,
                plotOutput("profilePlot", height=600)
            )
        )
    ),
    tabPanel("3D Plot",
        fluidRow(
            # Sidebar panel
            column(2,
                numericInput("nodeNum3d", "Node Number:", 51, min=1, max=101, step=1),
                numericInput("tStart3d", "Start Time:", .005, min=.005, max=100, step=.1),
                numericInput("tStop3d", "Stop Time:", 100, min=.005, max=100, step=.1),
                numericInput("avgNum3d", "Moving AVG Width:", 20, min=1, max=100, step=1),
                checkboxInput("avgBool3d", "Turn on Moving AVG:", FALSE),
                fluidRow(
                    column(6,
                        numericInput("theta","Theta",0,min=-180,max=180,step=5)
                    ),
                    column(6,
                        numericInput("phi","Phi",0,min=-180,max=180,step=5)
                    )
                ),
                sliderInput("mlim", "M Axis", min=0, max=1, value=c(0,1)),
                sliderInput("hlim", "H Axis", min=0, max=1, value=c(0,1)),
                sliderInput("vlim", "V Axis", min=-200, max=100, value=c(-150,50))
            ),
            column(10,
                plotOutput("plot3d", height=600)
            )
        )
    ),
    tabPanel("Data Set",
        fluidRow(
            column(10,
                textInput("dataFolder", "Data Folder:", value="/mnt/c/Users/Joey/Desktop/InternalElectrode/Only Flanking Stim (EXTRA ONSET)/", width="100%")
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # MAIN PAGE (2D Plot)
    generateSettings <- function(){
        c(input$xvar,input$yvar,input$nodeNum,input$tStart,input$tStop,input$color,input$avgNum,input$avgBool,input$deriv,input$normal)
    }
    
    output$plotVars <- renderUI({
        fluidRow(
            fluidRow(
                column(6, style='padding-left:5px;',
                    selectInput("yvar", "Y Variable:",
                        c("Time", names))
                ),
                column(6,
                    selectInput("deriv", "Respect To:",
                        c("N/A", "Time", names))
                )
            ),
            fluidRow(
                column(12, style='padding-left:5px;',
                    selectInput("xvar", "X Variable:",
                        c("Time", names))
                )
            )
        )
    })

    # Render new main plot when the generate button is clicked
    output$mainPlot <- renderPlot({
        input$generate
        cat("GENERATING NEW PLOT\n")
        settings <- isolate(generateSettings())
        mainPlot <- isolate(generatePlot(settings))
        mainPlot
    })
    
    # Save the plot to the plots folder when button is clicked
    observeEvent(input$save,{
        cat("SAVING\n")
        plot <- generatePlot(generateSettings())
        plotName <- generateName(generateSettings())
        ggsave(filename = sprintf("./Plots/%s",plotName), plot, width = 5, height = 3, dpi=150)
    })
    
    # Keep any plot for layering with other data
    observeEvent(input$keep,{
        cat("KEEPING\n")
        # note that to overwrite session variables the '<<-' syntax is used
        keepPlot <<- generatePlot(generateSettings())
    })
    
    # Clear the current layered plot
    observeEvent(input$clear,{
        cat("CLEARING\n")
        # note that to overwrite session variables the '<<-' syntax is used
        keepPlot <<- ggplot()
        mainPlot <-ggplot()
    })

    # PROFILE
    output$varProfile <- renderUI({
        selectInput("variableProfile", "Variable", names)    
    })

    output$profilePlot <- renderPlot({
        profilePlot <- generateProfile(input$tProfile, input$variableProfile, input$yLowerLim, input$yUpperLim)
        profilePlot
    })

    # 3D PLOT
    output$plot3d <- renderPlot({
        plot3d <- generate3D(input$nodeNum3d,input$tStart3d,input$tStop3d,input$avgBool3d,input$avgNum3d,input$theta,input$phi,input$mlim,input$hlim,input$vlim)
        plot3d
    })

    # Data selection
    observeEvent(input$updateData,{
        # There are two possible errors that can occur, the data folder doesn't exist, or just the Setup.txt doesn't
        tryCatch({
            txt <- readLines(paste(input$dataFolder, "Setup.txt", sep=""), warn=FALSE)
            txt <- paste(txt, collapse="\n")
            output$setup <- renderPrint({cat(txt)})
        },
        error=function(cond){
            output$setup <- renderPrint({cat(paste("NO Setup.txt file\n",cond, sep=""))})
        })
        
        tryCatch({
            initData(input$dataFolder)
        },
        error=function(cond){
            output$setup <- renderPrint({cat(paste("COULD NOT FIND DATA FOLDER\n",cond, sep=""))})
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
