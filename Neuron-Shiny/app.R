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
source("UsefulFunctions.R")

# define data and plot
DATA <- getDataAllDF("/mnt/c/Users/Joey/Desktop/InternalElectrode/Only Center Stim (NO ONSET)/")
#dataAvg <- movingAverageDF(DATA,20)
keepPlot <- ggplot()    # Keeps current saved plot

generateVector <- function(vectorVar, nodeNum, tStart, tStop, avgBool, avgNum){
    # generate the vector from DATA given the input parameters
    colNum = 0
    if(vectorVar=="v"){
        colNum = as.numeric(nodeNum)+203
    }else if(vectorVar=="m"){
        colNum = as.numeric(nodeNum)+1
    }else if(vectorVar=="h"){
        colNum = as.numeric(nodeNum)+102
    }else if(vectorVar=="t"){
        colNum = 1
    }

    vec <- DATA[DATA$Time>as.numeric(tStart) & DATA$Time<as.numeric(tStop), colNum]

    if(avgBool){
        vec <- movingAverage(vec, as.numeric(avgNum))
    }

    vec
} 

generatePlot <- function(settings){
    # settings is a vector containing the following strings
    # [1]   variable (v=Voltage, m=MGate, etc)
    # [2]   node number
    # [3]   tStart 
    # [4]   tStop
    # [5]   color
    # [6]   avgNum
    # [7]   avgBool

    cat("Current Settings: ")
    cat(settings)
    cat("\n")

    plot <- keepPlot   # Build plot from last stored plot

    # Generate x axis data
    if(settings[1] == "pp") {
        vectorVar <- "m"
    }else{
        vectorVar <- "t"
    }
    xData <- generateVector(vectorVar, settings[2], settings[3], settings[4], settings[7], settings[6])

    # Generate y axis data
    if(settings[1] == "pp") {
        vectorVar <- "h"
    }else{
        vectorVar <- settings[1]
    }
    yData <- generateVector(vectorVar, settings[2], settings[3], settings[4], settings[7], settings[6])

    plot <- plot+geom_path(aes(x=xData,y=yData), color=settings[5], size=1)
        
    plot
}

generateName <- function(settings){
    # Generate file name for a given plot settings
    # Using the form "<variable>_<nodeNum>.png"
    newName <- sprintf("%s_%s.png",settings[1],settings[2])
    newName
}

ui <- fluidPage(
    fluidRow(
        # Sidebar panel
        column(2,
            titlePanel("NEURON DATA"),
            selectInput("variable", "Variable:",
                        c("Voltage" = "v",
                          "M Gate" = "m",
                          "H Gate" = "h",
                          "Phase Plane"="pp")),
            fluidRow(
                column(6,
                    numericInput("nodeNum", "Node Number:", 51, min=1, max=101, step=1)
                ),
                column(6,
                    numericInput("avgNum", "Moving AVG Width:", 20, min=1, max=100, step=1)
                )
            ),
            checkboxInput("avgBool", "Turn on Moving Average:", FALSE),
            selectInput("color", "Color:",c("BLACK"="#111111","RED"="#FF2222","BLUE"="#2222FF","GREEN"="#22FF22")),
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
            plotOutput("mainPlot", height=700)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Render a new plot when inputs change
    output$mainPlot <- renderPlot({
        input$generate
        cat("GENERATING NEW PLOT\n")
        settings <- isolate(c(input$variable,input$nodeNum,input$tStart,input$tStop, input$color, input$avgNum, input$avgBool))
        mainPlot <- isolate(generatePlot(settings))
        mainPlot
    })
    
    # Save the plot to the plots folder when button is clicked
    observeEvent(input$save,{
        cat("SAVING\n")
        plot <- generatePlot(c(input$variable, input$nodeNum, input$tStart, input$tStop,input$color, input$avgNum, input$avgBool))
        plotName <- generateName(c(input$variable, input$nodeNum, input$tStart, input$tStop, input$color, input$avgNum, input$avgBool))
        ggsave(filename = sprintf("./Plots/%s",plotName), plot, width = 5, height = 3, dpi=150)
    })
    
    # Keep any plot for layering with other data
    observeEvent(input$keep,{
        cat("KEEPING\n")
        # note that to overwrite session variables the '<<-' syntax is used
        keepPlot <<- generatePlot(c(input$variable, input$nodeNum, input$tStart, input$tStop, input$color, input$avgNum, input$avgBool))
    })
    
    # Clear the current layered plot
    observeEvent(input$clear,{
        cat("CLEARING\n")
        # note that to overwrite session variables the '<<-' syntax is used
        keepPlot <<- ggplot()
        mainPlot <-ggplot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
