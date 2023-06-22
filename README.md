# Description:
An analytical framework (downloadable R Shiny application) to visualize and investigate drivers of surface water quality. To use this application, you need to download the zip file and open the Nutrient_Explorer.Rproj file within RStudio to run and utilize the graphical user interface locally on your computer (more details on openining and running the application below). 

# Features:
     - This application provides the user with the ability to analyze lake total nitrogen or total phosphorus data for Northeastern U.S. lakes based on LAGOS NE data (https://lagoslakes.org/lagos-ne/).
     - Alternatively, the user can upload and analyze their own surface water (lake, stream, river) dataset after properly formatting the data (instructions for how to format datasets can be found in the User Guide within the application). 
     - The user can summarize and visualize temporal and spatial patterns using a variety of approaches.
     - Subsets of the dataset can be created for further analysis using various characteristics of the variables, such as date, location, etc. 
     - Finally, the user can apply either random forest modeling or multiple linear regression to assess which predictor variables best model the spatial patterns in the water quality dataset and the models can be used to predict water quality concentrations for watersheds or locations lacking data. 

# Opening and running the Application:
     1. Download and unzip the Zip File containing the app and associated files.
     2. Open RStudio > Click on “Project” (blue box, upper right) > Select “Open Project.”
     3. Navigate to the file folder where you saved the Application.
     4. Click on the “Nutrient_Explorer.Rproj” file and select Open.
     5. On bottom right panel of RStudio, under the Files Tab, click on “ui.R” to open this R code file.
     6. On bottom right panel of RStudio, under the Files Tab, click on “server.R” to open this R code file.
     7. Check to make sure you have all the necessary packages installed before the next step.
     8. Near the Upper Right, click on the “Run App” button with the green arrow pointing to the right.  This will open the User Interface for the application in a separate window.
     9. See User Guide within the application for instructions on how to use the application.  

# Disclaimer:
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
  
For questions or comments contact Michael Pennino at pennino.michael@epa.gov
