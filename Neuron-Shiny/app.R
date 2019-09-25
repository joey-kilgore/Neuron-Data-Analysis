#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("UsefulFunctions.R")
v <- csvToDF(read.csv("Data/Voltage.csv"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Voltage Plot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("nodeNum",
                        "Node Number:",
                        min = 1,
                        max = 101,
                        value = 51)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("voltPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$voltPlot <- renderPlot({
        ggplot(data=v)+
            geom_path(aes(x=Time,y=v[,input$nodeNum+1]), color="#111111", size=2)+
            ylab("Voltage (mV)")+
            xlab("Time (ms)")+
            ggtitle("10kHz Block Central Node Voltage")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
