# Neuron-Data-Analysis
R Shiny app for analyzing data from NEURON simulations with complex models.  
For help with setup see the [WIKI](https://github.com/joey-kilgore/Neuron-Data-Analysis/wiki)

## Running the application  
Make sure you change the initial data folders in app.R to folders on your machine prior to running the application. Then enter the following:  
```  
git clone https://github.com/joey-kilgore/Neuron-Data-Analysis.git
cd Neuron-Shiny  
R -e "shiny::runApp('app.R')"  
```
This will load up the data and give you a port number on your machine to access the application. More info on 'runApp' can be found [HERE](https://shiny.rstudio.com/reference/shiny/latest/runApp.html).

## How to use  
This R shiny application is setup to have multiple different tabs to allow for different visualizations to be created easily. The initial data is loaded from the ./Neuron-Shiny/data directory, but can be changed using the data set tab.

### 2D Plots
![2d Plot screenshot](https://github.com/joey-kilgore/Neuron-Data-Analysis/blob/master/docs/screenshots/2dplot.png)
These allow for viewing variables plotted against eachother for deeper analysis. The initial data set includes multiple different variables, usually plotted over time. These plots can then be stacked on top of eachother for quickly comparing different compartments.  
The UI includes the ability to pick which variable is being plotted and over what time period. Plots can then be layered using the 'Keep Plot' button, and erased using the 'Clear Plot'. Additionally plots can be saved to file using the 'Save Plot' feature.  

### Profile Plots
![Profile Plot Screenshot](https://github.com/joey-kilgore/Neuron-Data-Analysis/blob/master/docs/screenshots/profileplot.png)  
This tab allows for viewing all compartments of a specific variable plotted together at a single time step. The animations of the plot can then allow for viewing all compartments through time.  

### 3D Plots  
![3d Plot screenshot](https://github.com/joey-kilgore/Neuron-Data-Analysis/blob/master/docs/screenshots/3dplot.png)  
This tab allows for viewing 3 different variables and compartments against each other through time. The color of the plot mirrors the values of the z axis. Rotations of the plot can be done using the theta and phi values. Additionally the plots can be animated using the UI on the bottom left, and during animations can be manipulated for better viewing angles.
