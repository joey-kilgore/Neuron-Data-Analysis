library(shiny)
library(ggplot2)
library("plot3D")
library(scales)
source("UsefulFunctions.R")
source("loadDataDynamic.R")

# define data and plot
dataDir <- "./data/"
keepPlot <- ggplot()    # Keeps current saved plot
dataCount <- 0
initData(dataDir)

# MAIN PAGE FUNCTIONS (2D Plot)
generateVector <- function(vectorVar, compNumString, tStartString, tStopString, avgBool, avgNumString, respectTo){
  # generate the vector from DATA given the input parameters
  compNum = as.numeric(compNumString)
  tStart = as.numeric(tStartString)
  tStop = as.numeric(tStopString)
  avgNum = as.numeric(avgNumString)
  if(vectorVar == "Time"){
    vec <- getTimeStep(tStart, tStop)
  }else{
    vec <- getVarTimeBound(vectorVar, compNum, tStart, tStop)
  }
  
  if(!missing(respectTo)){
    if(respectTo == "Time"){
      vecRespect = getTimeStep(tStart,tStop)
    }else{
      vecRespect = getVarTimeBound(vectorVar, compNum, tStart, tStop)
    }
    vec <- calcDiff(vecRespect, vec)
  }
  
  if(avgBool){
    vec <- movingAverage(vec, avgNum)
    vec <- vec[-(1:avgNum)]
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
  # Using the form "<variable>_<compNum>.png"
  newName <- sprintf("%s_vs_%s_Node%s.png",settings[2],settings[1],settings[3])
  newName
}

# PROFILE PLOT FUNCTIONS
generateProfile <- function(tProfile, variable, yMin, yMax){
  # Generate the profile plot (V vs node) at a given time
  vData <- getVarTime(variable, tProfile)
  x <- 1:length(vData)
  plot <- ggplot() + geom_path(aes(x=x,y=vData), color="#EE2222", size=.5)+
    geom_point(aes(x=x,y=vData), color="#EE2222",size=2)+
    xlab("Node")+ylab(variable)+ylim(yMin,yMax)+
    ggtitle(sprintf("%s @t=%.3f",variable, tProfile))
  plot
}

# 3D PLOT FUNCTIONS
generate3D <- function(settings){
  cat("SETTINGS\n")
  cat(settings)
  cat("\n")
  compX = as.numeric(settings[1])
  tStart = as.numeric(settings[2])
  tStop = as.numeric(settings[3])
  avgBool = as.logical(settings[4])
  avgNum = as.numeric(settings[5])
  theta = as.numeric(settings[6])
  phi = as.numeric(settings[7])
  xlims = c(as.numeric(settings[8]),as.numeric(settings[9]))
  ylims = c(as.numeric(settings[10]),as.numeric(settings[11]))
  zlims = c(as.numeric(settings[12]),as.numeric(settings[13]))
  xvar = settings[14]
  yvar = settings[15]
  zvar = settings[16]
  compY = as.numeric(settings[17])
  compZ = as.numeric(settings[18])
  
  # generate a 3d plot with x=m, y=h, z=v
  x <- generateVector(xvar, compX, tStart, tStop, avgBool, avgNum)
  y <- generateVector(yvar, compY, tStart, tStop, avgBool, avgNum)
  z <- generateVector(zvar, compZ, tStart, tStop, avgBool, avgNum)
  
  plot <- scatter3D(x,y,z, 
                    xlab=xvar,ylab=yvar,zlab=zvar,
                    axis.scales=FALSE, xlim=xlims, ylim=ylims, zlim=zlims,
                    bty="g", theta=as.numeric(theta), phi=as.numeric(phi), type="l", ticktype="detailed", lwd=2)
  plot
}

server <- function(input, output, session) {
  #-------------------------
  # MAIN PAGE (2D Plot)
  #-------------------------
  generateSettings <- function(){
    # generate settings gives an easier way of collecting all input values and passing them to more generic methods
    c(input$xvar,input$yvar,input$compNum,input$tStart,input$tStop,input$color,input$avgNum,input$avgBool,input$deriv,input$normal)
  }
  
  output$plotVars <- renderUI({
    # at runtime the number of variables to select from is generated based on
    #  the data set that is loaded in
    fluidRow(
      fluidRow(
        column(6, style='padding-left:25px;',
          selectInput("yvar", "Y Variable:", c(names))
        ),
        column(6,
          selectInput("deriv", "Respect To:", c("N/A", "Time", names))
        )
      ),
      fluidRow(
        column(12, style='padding-left:25px;',
          selectInput("xvar", "X Variable:", c("Time", names))
        )
      )
    )
  })
  
  observeEvent(input$yvar,{
    # each y variable can have a different number of compartments, and potentially a different time step
    # because of this, the values for the respective UI to choose these values is updated when a variable is chosen
    tStep = getTimeStepVal(input$yvar)
    tEnd = getEndTimeVal(input$yvar)
    updateNumericInput(session, "compNum", value=1, min=1, max=getNumCol(input$yvar))
    updateNumericInput(session, "tStart", value=tStep, min=tStep, max=tEnd, step=tStep)
    updateNumericInput(session, "tStop", value=tEnd, min=tStep, max=tEnd, step=tStep)
  })
  
  output$mainPlot <- renderPlot({
    # Render new main plot when the generate button is clicked
    input$generate
    req(input$xvar)         # required to ensure the UI has loaded prior to generating the graph
    cat("GENERATING NEW PLOT\n")
    settings <- isolate(generateSettings())
    mainPlot <- isolate(generatePlot(settings))
    mainPlot
  })
  
  observeEvent(input$save,{
    # Save the plot to the plots folder when button is clicked
    cat("SAVING\n")
    plot <- generatePlot(generateSettings())
    plotName <- generateName(generateSettings())
    ggsave(filename = sprintf("./Plots/%s",plotName), plot, width = 5, height = 3, dpi=150)
  })

  observeEvent(input$keep,{
    # Keep any plot for layering with other data
    cat("KEEPING\n")
    # note that to overwrite session variables the '<<-' syntax is used
    keepPlot <<- generatePlot(generateSettings())
  })
  
  
  observeEvent(input$clear,{
    # Clear the current layered plot
    cat("CLEARING\n")
    # note that to overwrite session variables the '<<-' syntax is used
    keepPlot <<- ggplot()
    mainPlot <- ggplot()
  })
  
  #-------------------------
  # PROFILE tab
  #-------------------------
  waitProfile <- reactiveValues()             # reactive to stare all reactive variables
  waitProfile$timer <- reactiveTimer(Inf)     # reactiveTimer is used to update the plot
  waitProfile$counter <- 0                    # keeps track of the current time to be plotted
  waitProfile$increment <- 1                  # the increment to counter between each plot
  
  observeEvent(waitProfile$timer(),{    
    # this increments the value of the counter when the timer triggers
    if(input$profileAnimate == TRUE){
      waitProfile$counter <- waitProfile$counter+input$tProfileIncrement
      if(waitProfile$counter > input$tProfileStop){
        waitProfile$counter <- input$tProfileStart
      }
    }
  })
  
  observeEvent(input$profileStart,{
    # update the waitProfile variables if the start button has been clicked
    if(input$profileAnimate == TRUE){
      waitProfile$counter <- input$tProfileStart
      waitProfile$increment <- input$tProfileIncrement
      waitProfile$timer <- reactiveTimer(250)
    }
  })
  
  observeEvent(input$profileStop,{
    # if the stop button is clicked, setting the reactiveTimer to an Inf delay between actions
    #  means it won't react anymore and is the standard way of turning off a reactiveTimer
    waitProfile$timer <- reactiveTimer(Inf)
  })
  
  output$varProfile <- renderUI({
    # the variables to select from are generated at render time to reflect the data set being used
    selectInput("variableProfile", "Variable", names)    
  })
  
  observeEvent(input$variableProfile,{
    # depending on the variable selected, the number of compartments or time steps could be different
    #  so when a variable is selected the respective UI elements must be updated
    tStart = getTimeStepVal(input$variableProfile)
    tEnd = getEndTimeVal(input$variableProfile)
    updateNumericInput(session, "tProfileStart", value=tStart, min=tStart, max=tEnd, step=tStart) 
    updateNumericInput(session, "tProfileStop", value=tEnd, min=tStart, max=tEnd, step=tStart)
    updateNumericInput(session, "tProfileIncrement", value=tStart, min=tStart, max=tEnd, step=tStart)
  })
  
  output$profilePlot <- renderPlot({
    # generate the profile plot, using the waitProfile counter as the time step if the user
    #  has selected to animate the plot
    if(input$profileAnimate == FALSE){
      profilePlot <- generateProfile(input$tProfileStart, input$variableProfile, input$yLowerLim, input$yUpperLim)
    } else {
      profilePlot <- generateProfile(waitProfile$counter, input$variableProfile, input$yLowerLim, input$yUpperLim)
    }
    profilePlot
  })
  
  #-------------------------
  # 3D Plot
  #-------------------------
  waits <- reactiveValues()   # reactive to store all reactive variables
  waits$counter <- 0
  waits$increment <- 1
  waits$timer <- reactiveTimer(Inf)   # the timer will be used to tell when the plot should update
  
  observeEvent(waits$timer(),{    
    # this increments the value of the counter when the timer triggers
    if(input$animateBool){
      waits$counter <- waits$counter+waits$increment
      if(waits$counter > input$tStop3d){
        waits$counter <- input$tStart3d
      }
    }
  })
  
  observeEvent(input$startAnimate,{   
    # when the start button is clicked the waits variable is initialized with the appropriate values
    if(input$animateBool == TRUE){
      waits$counter <- input$tStart3d
      waits$increment <- input$incrementsAnimate
      waits$timer <- reactiveTimer(150)
    }
  })
  
  observeEvent(input$stopAnimate,{    
    # on the stop button, the reactive timer is turned off
    waits$timer <- reactiveTimer(Inf)   # Inf is used to essentially turn the timer off
  })
  
  observeEvent(input$resetAnimate,{
    # clicking the reset button just sets the counter to the first time step
    waits$counter <- input$tStart3d
  })
  
  
  generateSettings3D <- function(){
    # this functions makes for an easy way of passing input parameters (there are a lot)
    #  to a more modular abstracted method
    # additionally this updates the text under the plot showing the start and stop times
    
    start <- input$tStart3d     # start and stop are used to hold the starting and stopping times
    stop <- input$tStop3d       #   that will be plotted on the 3D plot
    
    if(input$animateBool){
      # if the user is currently using the animation feature, then we will use those
      #  values from the waits variable for the correct start and stop times
      start <- waits$counter
      stop <- waits$counter + input$timeAnimate
    }        
    output$startTimeAnimate <- renderText(paste("Start Time: ", toString(start), sep="", collapse=NULL))
    output$endTimeAnimate <- renderText(paste("End Time: ", toString(stop), sep="", collapse=NULL))
    
    # this is the vector returned with all the input parameters
    c(input$compX,start,stop,input$avgBool3d,input$avgNum3d,input$theta,input$phi,input$xMin,input$xMax,input$yMin,input$yMax,input$zMin,input$zMax,input$xvar3d,input$yvar3d,input$zvar3d,input$compY,input$compZ)
  }
  
  output$plotVars3D <- renderUI({
    # the list of variables displayed for the user to select from is generated at render time to
    #  reflect the data set loaded
    fluidRow(
      fluidRow(
        column(4, style='padding-left:25px;',
          selectInput("xvar3d", "X Variable:",c(names))
        ),
        column(4,
          selectInput("yvar3d", "Y Variable:",c(names))
        ),
        column(4,
          selectInput("zvar3d", "Z Variable:",c(names))
        )
      )
    )
  })
  
  observeEvent(input$xvar3d, {
    # depending on the x variable, the respective UI for choosing compartment number and time need to be updated
    updateNumericInput(session, "compX", value=1, min=1, max=getNumCol(input$xvar3d))
    tStep = getTimeStepVal(input$xvar3d)
    tEnd = getEndTimeVal(input$xvar3d)
    updateNumericInput(session, "tStart3d", value=tStep, min=tStep, max=tEnd, step=tStep)
    updateNumericInput(session, "tStop3d", value=tEnd, min=tStep, max=tEnd, step=tStep)
  })
  
  observeEvent(input$yvar3d, {
    # the number of compartments available to choose from depend on the selected variable
    updateNumericInput(session, "compY", value=1, min=1, max=getNumCol(input$yvar3d))
  })
  
  observeEvent(input$zvar3d, {
    # the number of compartments available to choose from depend on the selected variable
    updateNumericInput(session, "compZ", value=1, min=1, max=getNumCol(input$zvar3d))
  })
  
  output$plot3d <- renderPlot({
    req(input$xvar3d)   # this ensure that the UI has been rendered prior to trying to plot
    waits$timer()       # the timer is a trigger for renderPlot
    
    settings3D <- generateSettings3D()
    plot3d <- generate3D(settings3D)
    plot3d
  })
  
  #-------------------------
  # Data Selection
  #-------------------------
  observeEvent(input$updateData,{
    # when the user selects to update the data set, we will search for the directory listed, and try 
    #  to load both the data set and the Setup.txt file
    
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