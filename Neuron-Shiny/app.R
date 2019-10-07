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
DATA <- getDataAllDF("./Data/")
#dataAvg <- movingAverageDF(DATA,20)
keepPlot <- ggplot()    # Keeps current saved plot

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
    
    # Set the time range for the data and whether the avg or raw data is used
    
    temp <- DATA[DATA$Time>as.numeric(settings[3]) & DATA$Time<as.numeric(settings[4]),]
    
    if(settings[1] == "v"){ # Generate a voltage plot
        if(settings[7]){
            temp[,as.numeric(settings[2])+203] = movingAverage(temp[,as.numeric(settings[2])+203],as.numeric(settings[6]))
        }
        plot <- plot+
            geom_path(aes(x=temp$Time,y=temp[,as.numeric(settings[2])+203]), color=settings[5], size=1)+
            ylab("Voltage (mV)")+ylim(-120,40)+
            xlab("Time (ms)")+
            ggtitle(sprintf("Node %s Voltage",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    if(settings[1] == "m"){ # Generate M Gate plot
        if(settings[7]){
            temp[,as.numeric(settings[2])+1] = movingAverage(temp[,as.numeric(settings[2])+1],as.numeric(settings[6]))
        }
        plot <- plot+
            geom_path(aes(x=temp$Time,y=temp[,as.numeric(settings[2])+1]), color=settings[5], size=1)+
            ylab("M Gate")+ylim(0,1)+
            xlab("Time (ms)")+
            ggtitle(sprintf("Node %s M Gate",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    if(settings[1] == "h"){ # Generate a H Gate plot
        if(settings[7]){
            temp[,as.numeric(settings[2])+102] = movingAverage(temp[,as.numeric(settings[2])+102],as.numeric(settings[6]))
        }
        plot <- plot+
            geom_path(aes(x=temp$Time,y=temp[,as.numeric(settings[2])+102]), color=settings[5], size=1)+
            ylab("H Gate")+ylim(0,1)+
            xlab("Time (ms)")+
            ggtitle(sprintf("Node %s H Gate",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    if(settings[1] == "pp"){ # Generate a Phase Plane Plot
        if(settings[7]){
            temp[,as.numeric(settings[2])+1] = movingAverage(temp[,as.numeric(settings[2])+1],as.numeric(settings[6]))
            temp[,as.numeric(settings[2])+102] = movingAverage(temp[,as.numeric(settings[2])+102],as.numeric(settings[6]))
        }
        plot <- plot+
            geom_path(aes(x=temp[,as.numeric(settings[2])+1], y=temp[,as.numeric(settings[2])+102]), color=settings[5], size=1)+
            ylab("H Gate")+ylim(0,1)+
            xlab("M Gate")+xlim(0,1)+ggtitle(sprintf("Node %s H Gate",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    plot
}

generateName <- function(settings){
    # Generate file name for a given plot settings
    # Using the form "<variable>_<nodeNum>.png"
    newName <- sprintf("%s_%s.png",settings[1],settings[2])
    newName
}

ui <- fluidPage(
    # Application title
    titlePanel("NEURON Data Analysis"),

    # Sidebar with a slider input and save button
    sidebarLayout(
        sidebarPanel(
            selectInput("variable", "Variable:",
                        c("Voltage" = "v",
                          "M Gate" = "m",
                          "H Gate" = "h",
                          "Phase Plane"="pp")),

            numericInput("nodeNum", "Node Number:", 51, min=1, max=101, step=1, width="50%"),
            numericInput("avgNum", "Moving Average Width:", 20, min=1, max=100, step=1, width="50%"),
            checkboxInput("avgBool", "Turn on Moving Average:", FALSE),
            selectInput("color", "Color:",c("BLACK"="#111111","RED"="#FF2222","BLUE"="#2222FF","GREEN"="#22FF22")),
            br(),
            # Graph updating buttons
            actionButton("generate", "Generate"),
            actionButton("keep", "Keep Plot"),
            actionButton("clear", "Clear Plot"),
            br(),
            # Time start and stop selection, divs were used for styling to force the two on the same line
            div(style="display: inline-block;vertical-align:center; width: 45%; margin-top: 10px;",numericInput("tStart", "Start Time:", 0, min=0, max=100, step=.01)),
            div(style="display: inline-block;vertical-align:center; width: 45%; margin-top: 10px;",numericInput("tStop", "Stop Time:", 100, min=0, max=100, step=.01)),
            br(),
            actionButton("save", "Save Plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mainPlot")
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
