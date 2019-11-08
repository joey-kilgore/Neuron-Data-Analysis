# Neuron-Data-Analysis
R Shiny app for analyzing data from NEURON simulations with complex models.  
For help with setup see the [WIKI](https://github.com/joey-kilgore/Neuron-Data-Analysis/wiki)

### Running the application  
Make sure you change the initial data folders in app.R to folders on your machine prior to running the application. Then enter the following:  
```  
cd Neuron-Shiny  
R -e "shiny::runApp('app.R')"  
```
This will load up the data and give you a port number on your machine to access the application. More info on 'runApp' can be found [HERE](https://shiny.rstudio.com/reference/shiny/latest/runApp.html).

### How to use  
When app.r is run, the 2D Plot tab has many options. It will load up various parameter from the dataset, and then combine them in a dataframe. This is done with code that already existed in the [Nerve-Block-Modeling/R Code](https://github.com/joey-kilgore/Nerve-Block-Modeling/tree/master/R%20Code). From here you can then select variables and colors to begin plotting.  
Use the generate button to update the plot window when you have selected the parameters you wish to plot.  
Use the keep button if you want to take the current parameters you have set and save them for layering another plot on top. This is similar to the [hold on](https://www.mathworks.com/help/matlab/ref/hold.html) functionality in MATLAB.  
Use the clear button when you want to remove all previous layers of the plot. This does not update the plot window, but the next time you generate a plot it will be generated from a blank state.  
The V Profile tab gives you the ability to plot all voltages across an axon at any given time.  
The 3D plot looks at the M gate, H gate, and voltage to observe the changes overtime. This can be extremely useful for looking at the steady-state of nodes during kilohertz frequency stimulation.  
The Data Set tab allows for the user to specify a different location to load data. If there is a 'Setup.txt' file in that folder, it will be shown on the screen.
