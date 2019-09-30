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

# define data and plot
v <- csvToDF(read.csv("Data/Voltage.csv"))
vPlot <- ggplot()

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Voltage Plot"),

    # Sidebar with a slider input and save button
    sidebarLayout(
        sidebarPanel(
            sliderInput("nodeNum",
                        "Node Number:",
                        min = 1,
                        max = 101,
                        value = 51),
            br(),
            actionButton("save", "Save Plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("voltPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Render a new plot when inputs change
    output$voltPlot <- renderPlot({
        print("GENERATING NEW PLOT")
        vPlot <- ggplot(data=v)+
            geom_path(aes(x=Time,y=v[,input$nodeNum+1]), color="#111111", size=1)+
            ylab("Voltage (mV)")+
            xlab("Time (ms)")+
            ggtitle(sprintf("10kHz Block Node %d Voltage",input$nodeNum))
        vPlot
    })
    
    # Save the plot to the plots folder when button is clicked
    observeEvent(input$save,{
        print("SAVING")
        plot <- ggplot(data=v)+
            geom_path(aes(x=Time,y=v[,input$nodeNum+1]), color="#111111", size=1)+
            ylab("Voltage (mV)")+
            xlab("Time (ms)")+
            ggtitle(sprintf("10kHz Block Node %d at BT",input$nodeNum))
        ggsave(filename = sprintf("./Plots/voltage%d.png",input$nodeNum), plot, width = 5, height = 3, dpi=150)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
