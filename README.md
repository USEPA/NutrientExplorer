# NutrientExplorer User Guide

User Guide for Nutrient Explorer R Shiny App

Opening the Application:
     1. Download and unzip the Zip File containing the app and associated files.
     2. Open RStudio > Click on “Project” (blue box, upper right) > Select “Open Project.”
     3. Navigate to the file folder where you saved the Application.
     4. Click on the “Nutrient_Explorer.Rproj” file and select Open.
     5. On bottom right panel of RStudio, under the Files Tab, click on “ui.R” to open this R code file.
     6. On bottom right panel of RStudio, under the Files Tab, click on “server.R” to open this R code file.
     7. Check to make sure you have all the necessary packages installed before the next step.
     8. Near the Upper Right, click on the “Run App” button with the green arrow pointing to the right.  This will open the User Interface for the application in a separate window.

Step-by-Step Instructions for Using this Application:
     1. “Load Data” Tab:
       a. In the “Load Data” tab, for “Dataset Options” select either LAGOS test or LAGOS. 
       b. Click “Load this dataset”
       c. Try changing the different “Endpoint options” and “Color palette” options. 
       d. Click on the “Summary Table” button on the bottom. Try changing the “Variable group” option.
     2. “Explore Data” Tab:
       a. Check out the different sub-tabs: “Summary Info,” “Time Series,” etc.
       b. “Summary Info” tab: Select from the different “Endpoint options”
       c. “Time Series” tab: 
         i. Toggle between the different endpoints. 
         ii. Test out the interactive capabilities of the plots (e.g., zooming in or switching on and off different datasets).
       d. “Endpoint HUC2 Summaries” tab: explore the different plot options
       e. “Predictor Variables HUC2 Summaries” tab: try selecting different predictor variables and Adjusting the Y axis scale. 
       f. “Correlations” Tab: 
         i. “Bivariate” tab: select different variables, then click “Display Plots”
         ii. “Multivariate” tab: select an endpoint variable and up to 5 predictor variables, then click “Display plots.”
       g. “Maps” tab: select an “endpoint option” from the top
       h. “Misc” tab: select between the different “endpoint options” at the top
     3. “Create a Subset” tab: 
       a. Adjust the different dataset ranges variables such as the Year, Month, IWS (watershed area, ha), Elevation, lake area, lake depth, Total Nitrogen, Total Phosphorus, etc.
       b. Subset the data by selecting specific Program Names, Program Types or HUC2 zones.
       c. Click “Save my subset.”
     4. “Explore Subset” Tab:
       a. Follow the same steps as step 2 above. 
     5. “Run Models” Tab: 
       a. Select the “Random Forest” model option first and click “Choose this Model”
         i. Select the endpoint for the model run (LogTP was used for this paper). 
         ii. Mouse over the different Model parameter options such as “ntree” to learn about each. 
         iii. Select predictor variables for the model (center section of the app).
            1. Practice using the different ways of selecting variables, such as by clicking “Select all default variables.” Or by clicking “Select all in Group*”, or by clicking in the blank box for each variable group and choosing a variable from the dropdown. 
            2. Practice removing variables by either clicking on the “X” on the right-hand side of the variable name or by clicking on the variable to highlight it in blue and then pressing the backspace or delete button on your keyboard.  
            3. Notice the pop-up information when mousing over a variable to get more detailed information about the variable. 
            4. Once all variables are chosen, click “Run random forest” button on the bottom left. 
            5. Scroll down to see the initial results and figures that appear for the model
            6. Click on the “See additional plots and maps” button on the bottom right
            7. Select a variable option from the drop down and click “Display partial dependence plot”
            8. Click “Show prediction map with new dataset” at the bottom.  Try adjusting the prediction month and year.  
            9. Click “Save prediction map.”
       b. Select the “Linear regression” model option and click “Choose this Model”
         i. Select the endpoint for the model run (LogTP was used for this paper). 
         ii. Select predictor variables for the model (center section of the app).
            1. Either manually select variables, as described for the random forest model above, or click “Link variables from random forest” button at the top (and choose the number of variables to select). 
         iii. Step 1 Run Correlation Analysis
            1. Select correlation criteria (variables with correlation value above 0.9 will be flagged). 
            2. Click “Run Correlation Analysis”
               a. Scroll down and click on “only show cells with high correlation”
                  i. Based on the correlated variables (those displayed in red, with black background), manually choose one of the correlated variables and delete it from the above predictor variable selection area. 
               b. Click “Run Correlation Analysis” and repeat above until no correlated variables remain. 
         iv. Step 2 Run Subset Selection
            1. Click “Run Subset Selection” button.
            2. Scroll down and follow the instructions in red, which describe how to select the best of the five models. 
         v. Step 3 Finalize Regression Model:
            1. Scroll back up and select “Finalize Regression Model”
            2. Scroll down and look at the table and check if any VIF values are flagged.  If so, remove that variable and reselect “Finalize Regression Model”
         vi. Step 4 Make Model Predictions 
            1. Select this option and then click “Show prediction map”
            2. Click “Save prediction map” – save the file and open to see how it looks. 

Instructions for formatting new dataset:
     1. Download example data file from the Load Data Tab  
     2. Make sure that your dataset has at least one of the response variables with the exact same naming format: “TN”, “TP”, “LogTN” or “LogTP”
     3. Make sure the predictor variables and other explanatory variables like latitude and longitude, HUC2, HUC8 are present.  
