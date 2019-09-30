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
vPlot <- ggplot()

generatePlot <- function(settings){
    # settings is a vector containing the following strings
    # [1]   variable (v=Voltage, m=MGate, etc)
    # [2]   node number
    cat("Current Settings: ")
    cat(settings)
    cat("\n")
    
    plot <- ggplot()    # if improper settings are sent, an empty graph will be generated
    
    temp <- DATA[DATA$Time>as.numeric(settings[3]) & DATA$Time<as.numeric(settings[4]),]
    
    if(settings[1] == "v"){ # Generate a voltage plot
        plot <- ggplot(data=temp)+
            geom_path(aes(x=temp$Time,y=temp[,as.numeric(settings[2])+203]), color="#111111", size=1)+
            ylab("Voltage (mV)")+ylim(-120,40)+
            xlab("Time (ms)")+
            ggtitle(sprintf("Node %s Voltage",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    if(settings[1] == "m"){ # Generate M Gate plot
        plot <- ggplot(data=temp)+
            geom_path(aes(x=temp$Time,y=temp[,as.numeric(settings[2])+1]), color="#111111", size=1)+
            ylab("M Gate")+ylim(0,1)+
            xlab("Time (ms)")+
            ggtitle(sprintf("Node %s M Gate",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    if(settings[1] == "h"){ # Generate a H Gate plot
        plot <- ggplot(data=temp)+
            geom_path(aes(x=temp$Time,y=temp[,as.numeric(settings[2])+102]), color="#111111", size=1)+
            ylab("H Gate")+ylim(0,1)+
            xlab("Time (ms)")+
            ggtitle(sprintf("Node %s H Gate",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    if(settings[1] == "pp"){ # Generate a Phase Plane Plot
        plot <- ggplot(data=temp)+
            geom_path(aes(x=temp[,as.numeric(settings[2])+1], y=temp[,as.numeric(settings[2])+102]), color="#111111", size=1)+
            ylab("H Gate")+ylim(0,1)+
            xlab("M Gate")+xlim(0,1)+ggtitle(sprintf("Node %s H Gate",settings[2]))+
            theme(plot.title = element_text(hjust = 0.5, size=18,face="bold"),axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    }
    plot
}

generateName <- function(settings){
    # Generate file name for a given plot settings
    newName <- sprintf("%s_%s.png",settings[1],settings[2])
    newName
}

# Define UI for application that draws a histogram
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
            sliderInput("nodeNum",
                        "Node Number:",
                        min = 1,
                        max = 101,
                        value = 51),
            sliderInput("time", 
                        "Time", min = 0, 
                        max = 100, value = c(0, 100)
            ),

            br(),
            actionButton("save", "Save Plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("voltPlot", height = "800px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Render a new plot when inputs change
    output$voltPlot <- renderPlot({
        cat("GENERATING NEW PLOT\n")
        settings <- c(input$variable,input$nodeNum,input$time)
        print(settings)
        vPlot <- generatePlot(settings)
        vPlot
    })
    
    # Save the plot to the plots folder when button is clicked
    observeEvent(input$save,{
        cat("SAVING\n")
        plot <- generatePlot(c(input$variable, input$nodeNum, input$time))
        plotName <- generateName(c(input$variable, input$nodeNum,input$time))
        ggsave(filename = sprintf("./Plots/%s",plotName), plot, width = 5, height = 3, dpi=150)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
