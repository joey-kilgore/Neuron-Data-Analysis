# This script is meant to be a test of dynamic loading of data sets into R
# There are no hardcoded file names
# data is loaded as a csv
# converted to a dataframe
# and then helper functions can be used to access the data
# Note: UsefulFunctions.R needs to be loaded before this script can be used

# init data
fullPath <- c()
names <- c()
dfList <- list()

# trying to load all files seperately from a folder
initData <- function(dirPath){
    # initial the session variables that store the data
    # first get all csv file names from the given directory
    fullPath <<- list.files(path = dirPath, pattern = "*.csv", full.names = T)
    names <<- list.files(path = dirPath, pattern = "*.csv$")
    dfList <<- list()
    
    # go through each file, loading the csv, converting to df and adding to the list
    for(i in 1:length(fullPath)){
        names[i] <<- tools::file_path_sans_ext(names[i])
        print(names[i])
        temp <- csvToDF(read.csv(fullPath[i], FALSE))
        dfList[[names[i]]] <<- temp
    }
}

getVarCol <- function(varName, nodeNum){
    # return vector of a given var and node for all timesteps
    unlist(dfList[[varName]][1:nrow(dfList[[varName]]),nodeNum+1])
}

getNumCol <- function(varName){
    # return the number of columns in the dataframe
    # (-1 for the time column)
    ncol(dfList[[varName]])-1
}

getVarRow <- function(varName, rowNum){
    # return vector of given var for all nodes at given row number
    unlist(dfList[[varName]][rowNum,2:ncol(dfList[[varName]])])
}

getVarTime <- function(varName, t){
    # return vector of given var for all nodes at given time step
    unlist(dfList[[varName]][
           dfList[[varName]]$Time>=t & dfList[[varName]]$Time<(t+getTimeStepVal(varName)),
           2:ncol(dfList[[varName]])])
}

getVarTimeBound <- function(varName, nodeNum, tStart, tStop){
    # return vector of given var and node over range of time
    unlist(dfList[[varName]][
        dfList[[varName]]$Time>tStart & dfList[[varName]]$Time<tStop,
        1+nodeNum])
}

getVarTimeStep <- function(varName, tStart, tStop){
    # return vector corresponding to the time steps taken between
    #   tStart and tStop
    unlist(dfList[[varName]][
        dfList[[varName]]$Time>tStart & dfList[[varName]]$Time<tStop,
        1])
}

getTimeStep <- function(tStart, tStop){
    # return vector corresponding to the time steps taken between
    #   tStart and tStop for the first known variable
    #   Note: that if each variable has different time steps, getVarTimeStep
    #   should be used instead of this
    unlist(dfList[[names[1]]][
        dfList[[names[1]]]$Time>tStart & dfList[[names[1]]]$Time<tStop,
        1])
}

getTimeStepVal <- function(varName){
    # return the value of the value of the first time step
    # which is also the time value between each time step
    dfList[[varName]][1,1]
}

getEndTimeVal <- function(varName){
    # return the last time step value for the varaible
    dfList[[varName]][nrow(dfList[[varName]]),1]
}
