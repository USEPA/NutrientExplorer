# This is the server logic of Nutrient Explorer Shiny Downloadable graphical user interface application. 
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
# http://shiny.rstudio.com/
#
# Last Updated: 2024-04-02
#
rm(list=ls())

library("shiny")
library("shinydashboard")
#print("Before loading packages: ")
#print(pryr::mem_used())
library("shinyjs")
library("shinyBS")
#library("shinythemes")
library("shinyalert")
library("conflicted")
library("tidyr")
#library("tidyverse")
library("gtsummary")
library("tibbletime")
library("readxl")
library("leaps")
library("lubridate")
library("writexl")
library("data.table")
library("dplyr")
library("padr")
library("scales")
library("DT")
library("sp") # added on 2024-05-03 (need this package for geoR and mapview packages to work)
library("geoR")
library("sf")
#library("rgeos")
#library("rgdal")
#library("maptools")
library("mapview")
#library("USAboundaries")
library(USA.state.boundaries)
library("ggplot2")
library("ggthemes")
library("plotmo")
library("pdp")
library("gridExtra")
library("randomForest")
library("ranger")
library("raster")
library("stats")
library("leaflet")
library("leaflet.extras")
library("plotly")
library("spatialEco")
library("car")
library("QuantPsyc")
library("stringr")
library("GGally")
library("fst")
library("viridis")
library("ggpubr")
library("htmlwidgets")
library("ie2misc")
#library("devtools")
#remote::install_github("rstudio/webshot2")
#print("After loading packages: ")
#print(pryr::mem_used())

# Check RF Predictor Variable Dataset
rf_dir = "C:/Users/mpennino/OneDrive - Environmental Protection Agency (EPA)/R/Shiny/EMVL/Nutrient_Explorer/2022/Final/Nutrient_Explorer_final_paper_version/Data/"
#load(paste0(rf_dir,"all_HU8_shapes.RData"))
#dim(joined_HU8)
#class(joined_HU8)
#names(joined_HU8)
#load(paste0(rf_dir,"HUC8_2007_7_Average_MaxDepth.RData"))
#dim(HUC8_newData)
#class(HUC8_newData)
#View(HUC8_newData)
#names(HUC8_newData)

# myData1 = read_fst(paste0(rf_dir,"LAGOS_Data_Reduced_test.fst"))
# compare and find missing variables
# vars1 = data.frame(vars = names(myData1),
#                    cat1 = rep('var1',length(names(myData1))))
# vars2 = data.frame(vars = names(HUC8_newData),
#                    cat2 = rep('var2',length(names(HUC8_newData))))
# 
# vars3 = merge(vars1,vars2,by='vars',all.x=T,all.y=T)

#load(paste0(rf_dir,"LAGOS_Data_Reduced_original.RData"))
#dim(myData) # 143999    314
#load(paste0(rf_dir,"LAGOS_Data_Reduced_1985_original.RData"))
#dim(myData) # 128062    314
#load(paste0(rf_dir,"LAGOS_Data_Reduced_test_original.RData"))
#dim(myData) # 14742   314


# myData = read_fst(paste0(rf_dir,"LAGOS_Data_Reduced_test_original.fst"))
# 
# Keep only used vaiables
# myData2 = myData[,c(# Base variables
#                     'Name','LAGOS_Lake_ID','HU8ZoneID','HU8','programname','programtype','Lat','Long',
#                     "HU2","Year","Month",'Day','Date',"Elevation","MaxDepth","MeanDepth",
#                     "lake_area_ha","lake_perim_meters","iws_ha","iws_slope_mean",
#                     # Response variables
#                     'tp','LogTP','tn','LogTN',
#                     # NLCD
#                     "NLCD_pct_developed","NLCD_pct_forest", "NLCD_pct_farm","NLCD_pct_wetlands",
#                     # P inputs
#                     "P_Crop_removal.1","P_f_fertilizer.1","Legacy_P.1","P_livestock_Waste.1","P_nf_fertilizer.1",
#                     "P_Deposition.1","P_human_waste_kg.1","P_kg_ww.1","Recovered_P.1",
#                     "P_Ag_Surplus.1","NAPI.1","P_Ag_Inputs.1","P_Anthro_Inputs.1",
#                     # N Inputs
#                     "N_CBNF.1","N_Crop_N_Rem.1","N_Fert_Farm.1","N_Fert_Urban.1","N_Forest_Fire.1",
#                     "Human_N_Demand.1","N_Human_Waste.1","N_Imp_Balance.1",
#                     "N_Livestock_Waste.1","N_Manure_Recov.1","N_Rock.1","N_Total_Deposition.1","N_Total_nBNF.1",
#                     "N_Ag_Surplus.1","NANI.1","N_Ag_Inputs.1","N_Anthro_Inputs.1",
#                     # Vegetation
#                     "Vegetation","Vegetation_Lag1","Vegetation_Lag2","Vegetation_Lag3","Vegetation_YrMean",
#                     "NPP","NPP_Lag1","NPP_Lag2","NPP_Lag3","NPP_YrMean",
#                     # Weather
#                     "SNOW","SNOW_Lag1","SNOW_Lag2","SNOW_Lag3","SNOW_YrMean","FIRE","FIRE_Lag1","FIRE_Lag2",
#                     "FIRE_Lag3","FIRE_YrMean","LST","LST_Lag1","LST_Lag2","LST_Lag3","LST_YrMean","LSTAnomaly",
#                     "LSTAnomaly_Lag1","LSTAnomaly_Lag2","LSTAnomaly_Lag3","LSTAnomaly_YrMean","Precip","Precip_Lag1",
#                     "Precip_Lag2","Precip_Lag3","Precip_YrMean","Tmax","Tmax_Lag1","Tmax_Lag2",
#                     "Tmax_Lag3","Tmax_YrMean","Tmean","Tmean_Lag1","Tmean_Lag2","Tmean_Lag3","Tmean_YrMean","Tmin",
#                     "Tmin_Lag1","Tmin_Lag2","Tmin_Lag3","Tmin_YrMean",
#                     # Aerosol
#                     "AerosolSize_Aqua","AerosolSize_Aqua_Lag1","AerosolSize_Aqua_Lag2","AerosolSize_Aqua_Lag3",
#                     "AerosolSize_Aqua_YrMean","AerosolSize_Terra","AerosolSize_Terra_Lag1",
#                     "AerosolSize_Terra_Lag2","AerosolSize_Terra_Lag3","AerosolSize_Terra_YrMean","AOD_Aqua",
#                     "AOD_Aqua_Lag1","AOD_Aqua_Lag2","AOD_Aqua_Lag3","AOD_Aqua_YrMean","AOD_Terra",
#                     "AOD_Terra_Lag1","AOD_Terra_Lag2","AOD_Terra_Lag3","AOD_Terra_YrMean",
#                     # Deposition
#                     "Atmo_Pdep","Atmo_Pdep_Year.Lag1","Tot_Ndep","Tot_Ndep_Year.Lag1","Tot_Sdep","Tot_Sdep_Year.Lag1")]

#save(myData2 , file = paste0(rf_dir,"LAGOS_Data_Reduced.RData"))
#save(myData2 , file = paste0(rf_dir,"LAGOS_Data_Reduced_1985.RData"))
#save(myData2 , file = paste0(rf_dir,"LAGOS_Data_Reduced_test.RData"))
#write_fst(myData2, paste0(rf_dir,"LAGOS_Data_Reduced_test.fst"),compress = 50, uniform_encoding = TRUE)

#download_dir = "C:/Users/mpennino/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/"
#write.csv(HUC8_newData, paste0(download_dir,"HUC8_newData.csv"),row.names=F) 


source("./import_raw_data.R")
options(shiny.maxRequestSize = 500*1024^2)

if (!exists("default_search")) default_search <- ""
if (!exists("default_search_columns")) default_search_columns <- NULL

cleanMem <- function(n=10) {for (i in 1:n) gc()}



function(input, output, session) {
  
  useShinyjs()
  useShinyalert()
  selected_to_summary <- reactiveValues(selected_data=data.frame())
  group_names <- reactiveValues(Lake_Character=vector(),NLCD_Land_Use=vector(),Weather_related=vector(),Vegetation_related=vector(),Aerosol_related=vector(),N_Inventory_Raw=vector(),
                                N_Inventory_Scaled=vector(),P_Inventory_Raw=vector(),P_Inventory_Scaled=vector(),Deposition=vector(),Sampling_Info=vector())
  group_names_for_default <- reactiveValues(lake_char_names=vector(),NLCD_names=vector(),P_inventory_names=vector(),N_inventory_names=vector(),aerosol_names=vector(),
                                            vegetation_names=vector(),weather_names=vector(),deposition_names=vector())
  data_to_model <- reactiveValues(default_variable_names=vector(),group_for_names=vector(),data_subset=data.frame(),sliders_value_list=list())
  lr_model_input <- reactiveValues()
  lr_model_output <- reactiveValues()
  rf_model_input <- reactiveValues()
  rf_model_output <- reactiveValues()
  correlation_upper <- reactiveValues()
  to_download <- reactiveValues(LR_map=0,RF_map=0)
  click_States_list <- reactiveValues(ids=vector())
  click_HUC2_list <- reactiveValues(ids=vector(),huc2_names=vector())
  NE_states_list <- reactiveValues(names_in_subset=vector())
  outputOptions(output,suspendWhenHidden = FALSE)
  conflict_prefer("select","dplyr")
  conflict_prefer("union","dplyr")
  conflict_prefer("dataTableOutput", "DT")
  conflict_prefer("partial", "pdp")
  conflict_prefer("map", "maps")
  conflict_prefer("click", "shinyjs")
  conflict_prefer("box", "graphics")
  conflict_prefer("viridis_pal", "viridis")
  
  # observe({
  #   invalidateLater(50000,session)
  #   cleanMem()
  # })
  
  #####################################################################################################################
  ######################################## server code start for tabPanel "Load Data"##################################
  #####################################################################################################################
  
  loaded_data <- eventReactive(input$loadDataset,{
<<<<<<< HEAD
    print(input$dataset_option)
    if (is.null(input$uploaded_dataset)){
      if (input$dataset_option=="LAGOS test") {
        start_time <- Sys.time()
        #load("./Data/LAGOS_Data_Reduced_test.RData")
        myData <- read_fst("./Data/LAGOS_Data_less_variables_test.fst") ##YD changed
        colnames(myData)[colnames(myData)=="tn"]<- "TN"
        colnames(myData)[colnames(myData)=="tp"]<- "TP"
        colnames(myData)[colnames(myData)=="iws_roaddensity_density_mperha"]<- "iws_road_density_mperha"
        end_time <- Sys.time()
        time_to_load = difftime(end_time,start_time,units='mins')
        print(paste0("the runtime to load the test file is:",time_to_load))
      }else if(input$dataset_option=="LAGOS"){
        load("./Data/LAGOS_Data_Reduced_1985_less_variables.RData") ##YD changed
        #myData = myData2  ##YD commented this line out
        colnames(myData)[colnames(myData)=="tn"]<- "TN"
        colnames(myData)[colnames(myData)=="tp"]<- "TP"
        colnames(myData)[colnames(myData)=="iws_roaddensity_density_mperha"]<- "iws_road_density_mperha"
      }
    }else{
      shinyjs::disable("dataset_option") ##YD add
      myData<-import_raw_data(input$uploaded_dataset$datapath,"csv",has_header=TRUE)
      colnames(myData)[colnames(myData)=="tn"]<- "TN"
      colnames(myData)[colnames(myData)=="tp"]<- "TP"
      colnames(myData)[colnames(myData)=="iws_roaddensity_density_mperha"]<- "iws_road_density_mperha"
    }
    myData$HU8=as.factor(myData$HU8)
    myData$HU2=as.factor(myData$HU2)
=======
               print(input$dataset_option)
            if (is.null(input$uploaded_dataset)){
               if (input$dataset_option=="LAGOS test") {
                   start_time <- Sys.time()
                   #load("./Data/LAGOS_Data_Reduced_test.RData")
                   myData <- read_fst("./Data/LAGOS_Data_less_variables_test.fst") ##YD changed
                   colnames(myData)[colnames(myData)=="tn"]<- "TN"
                   colnames(myData)[colnames(myData)=="tp"]<- "TP"
                   colnames(myData)[colnames(myData)=="iws_roaddensity_density_mperha"]<- "iws_road_density_mperha"
                   end_time <- Sys.time()
                   time_to_load = difftime(end_time,start_time,units='mins')
                   print(paste0("the runtime to load the test file is:",time_to_load))
               }else if(input$dataset_option=="LAGOS"){
                   load("./Data/LAGOS_Data_Reduced_1985_less_variables.RData") ##YD changed
                   #myData = myData2  ##YD commented this line out
                   colnames(myData)[colnames(myData)=="tn"]<- "TN"
                   colnames(myData)[colnames(myData)=="tp"]<- "TP"
                   colnames(myData)[colnames(myData)=="iws_roaddensity_density_mperha"]<- "iws_road_density_mperha"
               }
            }else{
              shinyjs::disable("dataset_option") ##YD add
              myData<-import_raw_data(input$uploaded_dataset$datapath,"csv",has_header=TRUE)
              colnames(myData)[colnames(myData)=="tn"]<- "TN"
              colnames(myData)[colnames(myData)=="tp"]<- "TP"
              colnames(myData)[colnames(myData)=="iws_roaddensity_density_mperha"]<- "iws_road_density_mperha"
            }
              myData$HU8=as.factor(myData$HU8)
              myData$HU2=as.factor(myData$HU2)
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
    
    return(myData)
    
  })
  
  observe(loaded_data())
  
  observeEvent(input$dataInfo,{
    shinyjs::runjs("swal.close();")
  })
  
  
  radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
    
    options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
    options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
    bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
  }
  
  # MJP added
  output$waituntiluploadcomplete <- renderUI({
    div(
      h5("If uploading new dataset, wait until it says 'Upload complete':",style="color:red;font-style:bold")
    )
  })
  
  ##Yadong removed "NRSA" and "NLA" from radioButton choices in line 261
  observeEvent(input$uploaded_dataset,{
    updateRadioButtons(session,"dataset_option",choices=c("LAGOS test","LAGOS"),selected=character(0))
  })
  
  # output$uploaded_dataset <- renderUI({
  #   fileInput(inputId="waituntiluploadcomplete", label="Wait until it says 'upload complete':", multiple=FALSE,accept=c("text/csv",".csv",".RData"))
  # })
  
  
  
  output$select <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      div(br(),
          radioButtons("disp", "Display", choices = c(Head = "head",Tail="tail"),selected = "head"),
          radioTooltip("disp",choice="head",title="display the beginning rows of the dataset.",placement="right",trigger="hover"),
          radioTooltip("disp",choice="tail",title="display the last rows of the dataset.",placement="right",trigger="hover")
      )
    }
  })
  
  output$display_button <- renderUI({
    data <- loaded_data()
    
    if (length(data) > 0 ) {
      # print("After loading LAGOS dataset: ")
      # print(pryr::mem_used())
      actionButton(inputId = "displayid",label = "Display file contents",style="color:black;background-color:grey")
      
    }
  })
  
  output$download_button <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      downloadButton(outputId="downloadDataset", label="Download this dataset",style="color:black;background-color:grey") 
    }
  })
  
  output$display_map <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      withSpinner(leafletOutput("display_map_for_lakes",width=1000, height=500),type=2)
    }
  })
  
  output$display_table_button <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      actionButton(inputId="displayTable", label="Summary table",style="color:black;background-color:grey")  
    }
  })
  
  output$display_overall_text <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      verbatimTextOutput("summary_table_overall_text")
    }
  })
  
  output$display_text_continuous <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      verbatimTextOutput("summary_table_text")
    }
  })
  
  output$display_text_cat <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      verbatimTextOutput("summary_table_text_cat")
    }
  })
  
  observeEvent(input$displayid, {
    print(input$displayid %% 2 != 0)
    if (input$displayid %% 2 != 0) {
      shinyjs::show("content_table_panel")
      output$rawcontents <- renderTable({
        
        data <- loaded_data()
        # print(length(data))
        
        if (isolate(input$disp == "head")) {
          return(head(data))
        }
        else if (isolate(input$disp == "tail")) {
          return(tail(data))
        } 
      },type="html",bordered = TRUE,striped=TRUE,align="c")
    }else{
      shinyjs::hide("content_table_panel")
    }
  })
  
  
  output$downloadDataset <- downloadHandler(
    filename = function(){
      paste(input$loadDataset,"_downloaded.csv",sep="")
    },
    content = function(file){
      write.csv(loaded_data(),file,row.names=FALSE)
    }
  )
  
  
  output$end_points_option <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      div(radioButtons("end_points_choice",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=FALSE),
          style= "margin-left:50px;font-style:bold")
      
    }
  })
  
  output$color_palette_option <- renderUI({
    data <- loaded_data()
    if (length(data) > 0 ) {
      div(radioButtons("select_color",label ="Color palette options",choices=c("Blues","Greens","viridis","magma","plasma"),selected="viridis",inline=FALSE),
          style= "margin-left:50px;font-style:bold") 
    }
  })
  
  ####### this code was used to pre-process HUC2 shapefiles to SpatialPolygonsDataFrame #####################
  
  # HU2_file_names <- list.files("./Data/HU2_shapeFiles/",pattern="*.shp$",full.names=TRUE,recursive = FALSE)
  # print(length(HU2_file_names))
  # all_HU2_shapes = lapply(HU2_file_names,readOGR)
  # joined_HU2 <- do.call(bind,all_HU2_shapes)
  # save(joined_HU2,file="./all_HU2_shapes.RData")
  load("./Data/all_HU2_shapes_sf_without_holes.RData")
  # print(length(all_HU2_shapes))
  
  ####### this code was used to pre-process HUC2 shapefiles to sf objects ################################
  # library(sf)
  # library(raster)
  #  myRaster <- function(myShape){
  #  st_shape = st_read(myShape)
  #  myRaster <- st_transform(st_shape,CRS("+init=epsg:26978"))
  #  return(myRaster)
  #  }
  # 
  # HU2_file_names <- list.files("./Data/HU2_shapeFiles/",pattern="*.shp$",full.names=TRUE,recursive = FALSE)
  # print(length(HU2_file_names))
  # all_HU2_rasters = lapply(HU2_file_names,myRaster)
  # all_joined_HU2 = do.call(rbind,lapply(all_HU2_rasters,st_sf))
  # save(all_joined_HU2,file="./all_HU2_sf.RData")
  
  ####### this code was used to pre-process HUC8 shapefiles to SpatialPolygonsDataFrame #####################
  # HU8_file_names <- list.files("./Data/HU8_shapeFiles/",pattern="*.shp$",full.names=TRUE,recursive = FALSE)
  # print(length(HU8_file_names))
  # all_HU8_shapes = lapply(HU8_file_names,readOGR)
  # joined_HU8 <- do.call(bind,all_HU8_shapes_projected)
  # save(joined_HU8,file="./all_HU8_shapes.RData")
  
  ##########################Get shapefiles for States in the U.S.###########################################
  #states_in_US = us_states() # USABoundaries
  states_in_US = st_as_sf(state_boundaries_wgs84) # USA.state.boundaries
  states_in_US = states_in_US[states_in_US$NAME !='Alaska' & 
<<<<<<< HEAD
                                states_in_US$NAME !='Hawaii' &
                                states_in_US$NAME !='Puerto Rico',]
=======
                          states_in_US$NAME !='Hawaii' &
                          states_in_US$NAME !='Puerto Rico',]
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
  #print(states_in_US$NAME)
  NE = states_in_US[states_in_US$NAME == 'Minnesota' | 
                      states_in_US$NAME == 'Maine' |
                      states_in_US$NAME == 'Delaware' |
                      states_in_US$NAME == 'Wisconsin' | 
                      states_in_US$NAME == 'Iowa' |
                      states_in_US$NAME == 'Michigan' |
                      states_in_US$NAME == 'Illinois' |
                      states_in_US$NAME == 'Missouri' |
                      states_in_US$NAME == 'Indiana' |
                      states_in_US$NAME == 'Ohio' |
                      states_in_US$NAME == 'New Jersey' |
                      states_in_US$NAME == 'New York' |
                      states_in_US$NAME == 'Pennsylvania' |
                      states_in_US$NAME == 'Connecticut' |
                      states_in_US$NAME == 'Vermont' |
                      states_in_US$NAME == 'Massachusetts' |
                      states_in_US$NAME == 'New Hampshire',]
  NE_states_all = c('Minnesota','Maine','Delaware','Maryland','Wisconsin','Iowa','Michigan', 'Illinois','Indiana','Ohio','Kentucky',
                    'West Virginia','Virginia','New Jersey','New York','Pennsylvania','Connecticut', 'Vermont','Massachusetts','New Hampshire','Rhode Island',
                    'District of Columbia','North Dakota','South Dakota','Nebraska','Montana','Wyoming','Colorado','Kansas','Missouri',
                    'Oklahoma','New Mexico','Texas','Louisiana','Mississippi','Arkansas','Tennessee')
  
  states_in_US_sp = sf::as_Spatial(states_in_US)
  sf::sf_use_s2(FALSE)
  #print(class(states_in_US_sp))
  
  #print("After loading HUC2 and states layers: ")
  #print(pryr::mem_used())
  
  ## to display the map
  
  output$display_map_for_lakes <- renderLeaflet({
    data <- loaded_data()
    rf_dir = "C:/Users/mpennino/OneDrive - Environmental Protection Agency (EPA)/R/Shiny/EMVL/Nutrient_Explorer/2022/Final/Nutrient_Explorer_final_paper_version/Data/"
    
    # MJP variable categories
    #Data = read.csv("C:/Users/mpennino/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/LAGOS_Test.csv")
    #Data = read_fst(paste0(rf_dir,"LAGOS_Data_Reduced_test_original.fst"))
    #data1 = read_fst(paste0(rf_dir,"LAGOS_Data_Reduced_test.fst"))
<<<<<<< HEAD
    varnames = data.frame(varnames = names(data))
    varnames$category = NA
    varnames$category = ifelse(str_detect(varnames$varnames,"slope") | 
                                 str_detect(varnames$varnames,"Slope")|
                                 str_detect(varnames$varnames,"iws")|
                                 str_detect(varnames$varnames,"IWS")|
                                 str_detect(varnames$varnames,"shed")|
                                 str_detect(varnames$varnames,"lake_area_ha")|
                                 str_detect(varnames$varnames,"perim_meters")|
                                 str_detect(varnames$varnames,"Depth")|
                                 str_detect(varnames$varnames,"depth")|
                                 str_detect(varnames$varnames,"stream_length")|
                                 str_detect(varnames$varnames,"stream_order")|
                                 str_detect(varnames$varnames,"stream_width")|
                                 str_detect(varnames$varnames,"Elevation"),'lake_char_names',varnames$category)
    varnames$category = ifelse(str_detect(varnames$varnames,"NLCD") | 
                                 str_detect(varnames$varnames,"nlcd") |
                                 str_detect(varnames$varnames,"landuse")|
                                 str_detect(varnames$varnames,"LANDUSE"),'NLCD_names',varnames$category)
    varnames$category = ifelse(str_detect(varnames$varnames,"Aerosol") | 
                                 str_detect(varnames$varnames,"aerosol")|
                                 str_detect(varnames$varnames,"AOD"),'aerosol_names',varnames$category)
    varnames$category = ifelse(str_detect(varnames$varnames,"SNOW") | 
                                 str_detect(varnames$varnames,"FIRE") |
                                 str_detect(varnames$varnames,"LST") |
                                 str_detect(varnames$varnames,"Precip") | 
                                 str_detect(varnames$varnames,"Tmax") |
                                 str_detect(varnames$varnames,"Tmean") |
                                 str_detect(varnames$varnames,"Tmin") ,'weather_names',varnames$category)
    
    varnames$category = ifelse(str_detect(varnames$varnames,"N_")|
                                 str_detect(varnames$varnames,"TN_"),'N_inventory_names',varnames$category)                             
    varnames$category = ifelse(str_detect(varnames$varnames,"P_")|
                                 str_detect(varnames$varnames,"TP_"),'P_inventory_names',varnames$category)                             
    
    varnames$category = ifelse(str_detect(varnames$varnames,"Atmo") | 
                                 str_detect(varnames$varnames,"Ndep") |
                                 str_detect(varnames$varnames,"Pdep") |
                                 str_detect(varnames$varnames,"Sdep")|
                                 str_detect(varnames$varnames,"Dep_"),'deposition_names',varnames$category)
    varnames$category = ifelse(str_detect(varnames$varnames,"Vegetation")|
                                 str_detect(varnames$varnames,"NPP")|
                                 str_detect(varnames$varnames,"canopy")|
                                 str_detect(varnames$varnames,"Canopy"),'vegetation_names',varnames$category)                             
    
    # Remove incorrect categorizations
    #varnames$category = ifelse(str_detect(varnames$varnames,"NPP"),'P_inventory_names',varnames$category)                             
    
    lakevar = c("HU2","Year","Month","iws_ha","Elevation","lake_area_ha","MaxDepth","iws_slope_mean") # removed these variables: ,"iws_damdensity_pointsperha","iws_road_density_mperha","iws_canopy2001_mean","lake_perim_meters",
    group_names_for_default$lake_char_names = c(lakevar,as.character(na.omit(varnames[varnames$category == 'lake_char_names',]$varnames)))
    lake_group = rep("Date/Lake Characteristics",length(group_names_for_default$lake_char_names))
    
    #group_names_for_default$NLCD_names = c("nlcd_pct_0","nlcd_pct_11","nlcd_pct_31","nlcd_pct_52","nlcd_pct_71","NLCD_pct_developed","NLCD_pct_forest","NLCD_pct_farm","NLCD_pct_wetlands")
    group_names_for_default$NLCD_names = as.character(na.omit(varnames[varnames$category == 'NLCD_names',]$varnames))
    NLCD_group = rep("NLCD Land Use",length(group_names_for_default$NLCD_names))
    
    # group_names_for_default$P_inventory_names = c("P_Crop_removal.1","P_f_fertilizer.1","Legacy_P.1","P_livestock_Waste.1","P_nf_fertilizer.1","P_Deposition.1","P_human_waste_kg.1","P_kg_ww.1","Recovered_P.1",
    #                       "P_Ag_Surplus.1","NAPI.1","P_Ag_Inputs.1","P_Anthro_Inputs.1")
    group_names_for_default$P_inventory_names = as.character(na.omit(varnames[varnames$category == 'P_inventory_names',]$varnames))
    P_inventory_group = rep("P Inventory",length(group_names_for_default$P_inventory_names))
    
    # group_names_for_default$N_inventory_names = c("N_CBNF.1","N_Crop_N_Rem.1","N_Fert_Farm.1","N_Fert_Urban.1","N_Forest_Fire.1","Human_N_Demand.1","N_Human_Waste.1","N_Imp_Balance.1",
    #                       "N_Livestock_Waste.1","N_Manure_Recov.1","N_Rock.1","N_Total_Deposition.1","N_Total_nBNF.1","N_Ag_Surplus.1","NANI.1","N_Ag_Inputs.1","N_Anthro_Inputs.1")
    group_names_for_default$N_inventory_names = as.character(na.omit(varnames[varnames$category == 'N_inventory_names',]$varnames))
    N_inventory_group = rep("N Inventory",length(group_names_for_default$N_inventory_names))
    
    #group_names_for_default$vegetation_names = c("Vegetation","Vegetation_Lag1","Vegetation_Lag2","Vegetation_Lag3","Vegetation_YrMean","NPP","NPP_Lag1","NPP_Lag2","NPP_Lag3","NPP_YrMean")
    group_names_for_default$vegetation_names = as.character(na.omit(varnames[varnames$category == 'vegetation_names',]$varnames))
    vegetation_group = rep("Vegetation related",length(group_names_for_default$vegetation_names))
    
    # group_names_for_default$weather_names = c("SNOW","SNOW_Lag1","SNOW_Lag2","SNOW_Lag3","SNOW_YrMean","FIRE","FIRE_Lag1","FIRE_Lag2","FIRE_Lag3","FIRE_YrMean","LST","LST_Lag1","LST_Lag2","LST_Lag3","LST_YrMean","LSTAnomaly",
    #                   "LSTAnomaly_Lag1","LSTAnomaly_Lag2","LSTAnomaly_Lag3","LSTAnomaly_YrMean","Precip","Precip_Lag1","Precip_Lag2","Precip_Lag3","Precip_YrMean","Tmax","Tmax_Lag1","Tmax_Lag2",
    #                   "Tmax_Lag3","Tmax_YrMean","Tmean","Tmean_Lag1","Tmean_Lag2","Tmean_Lag3","Tmean_YrMean","Tmin","Tmin_Lag1","Tmin_Lag2","Tmin_Lag3","Tmin_YrMean")
    group_names_for_default$weather_names = as.character(na.omit(varnames[varnames$category == 'weather_names',]$varnames))
    weather_group = rep("Weather related",length(group_names_for_default$weather_names))
    
    # group_names_for_default$aerosol_names = c("AerosolSize_Aqua","AerosolSize_Aqua_Lag1","AerosolSize_Aqua_Lag2","AerosolSize_Aqua_Lag3","AerosolSize_Aqua_YrMean","AerosolSize_Terra","AerosolSize_Terra_Lag1",
    #                   "AerosolSize_Terra_Lag2","AerosolSize_Terra_Lag3","AerosolSize_Terra_YrMean","AOD_Aqua","AOD_Aqua_Lag1","AOD_Aqua_Lag2","AOD_Aqua_Lag3","AOD_Aqua_YrMean","AOD_Terra",
    #                   "AOD_Terra_Lag1","AOD_Terra_Lag2","AOD_Terra_Lag3","AOD_Terra_YrMean")
    group_names_for_default$aerosol_names = as.character(na.omit(varnames[varnames$category == 'aerosol_names',]$varnames))
    aerosol_group = rep("Aerosol related",length(group_names_for_default$aerosol_names))
    
    #group_names_for_default$deposition_names = c("Atmo_Pdep","Atmo_Pdep_Year.Lag1","Tot_Ndep","Tot_Ndep_Year.Lag1","Tot_Sdep","Tot_Sdep_Year.Lag1")
    group_names_for_default$deposition_names = as.character(na.omit(varnames[varnames$category == 'deposition_names',]$varnames))
    deposition_group = rep("Deposition",length(group_names_for_default$deposition_names))
    
    default_128 <- c(group_names_for_default$lake_char_names,group_names_for_default$NLCD_names,group_names_for_default$aerosol_names,group_names_for_default$weather_names,group_names_for_default$P_inventory_names,group_names_for_default$N_inventory_names,group_names_for_default$vegetation_names,group_names_for_default$deposition_names)
    
    data_to_model$default_variable_names = default_128
    
    save(default_128,file="./default_variables.RData")
    data_to_model$group_for_names = c(lake_group,NLCD_group,aerosol_group,weather_group,P_inventory_group,N_inventory_group,vegetation_group,deposition_group)
=======
  varnames = data.frame(varnames = names(data))
  varnames$category = NA
  varnames$category = ifelse(str_detect(varnames$varnames,"slope") | 
                               str_detect(varnames$varnames,"Slope")|
                               str_detect(varnames$varnames,"iws")|
                               str_detect(varnames$varnames,"IWS")|
                               str_detect(varnames$varnames,"shed")|
                               str_detect(varnames$varnames,"lake_area_ha")|
                               str_detect(varnames$varnames,"perim_meters")|
                               str_detect(varnames$varnames,"Depth")|
                               str_detect(varnames$varnames,"depth")|
                               str_detect(varnames$varnames,"stream_length")|
                               str_detect(varnames$varnames,"stream_order")|
                               str_detect(varnames$varnames,"stream_width")|
                               str_detect(varnames$varnames,"Elevation"),'lake_char_names',varnames$category)
  varnames$category = ifelse(str_detect(varnames$varnames,"NLCD") | 
                               str_detect(varnames$varnames,"nlcd") |
                               str_detect(varnames$varnames,"landuse")|
                               str_detect(varnames$varnames,"LANDUSE"),'NLCD_names',varnames$category)
  varnames$category = ifelse(str_detect(varnames$varnames,"Aerosol") | 
                               str_detect(varnames$varnames,"aerosol")|
                               str_detect(varnames$varnames,"AOD"),'aerosol_names',varnames$category)
  varnames$category = ifelse(str_detect(varnames$varnames,"SNOW") | 
                               str_detect(varnames$varnames,"FIRE") |
                               str_detect(varnames$varnames,"LST") |
                               str_detect(varnames$varnames,"Precip") | 
                               str_detect(varnames$varnames,"Tmax") |
                               str_detect(varnames$varnames,"Tmean") |
                               str_detect(varnames$varnames,"Tmin") ,'weather_names',varnames$category)
  
  varnames$category = ifelse(str_detect(varnames$varnames,"N_")|
                               str_detect(varnames$varnames,"TN_"),'N_inventory_names',varnames$category)                             
  varnames$category = ifelse(str_detect(varnames$varnames,"P_")|
                               str_detect(varnames$varnames,"TP_"),'P_inventory_names',varnames$category)                             
  
  varnames$category = ifelse(str_detect(varnames$varnames,"Atmo") | 
                               str_detect(varnames$varnames,"Ndep") |
                               str_detect(varnames$varnames,"Pdep") |
                               str_detect(varnames$varnames,"Sdep")|
                               str_detect(varnames$varnames,"Dep_"),'deposition_names',varnames$category)
  varnames$category = ifelse(str_detect(varnames$varnames,"Vegetation")|
                               str_detect(varnames$varnames,"NPP")|
                               str_detect(varnames$varnames,"canopy")|
                               str_detect(varnames$varnames,"Canopy"),'vegetation_names',varnames$category)                             
  
  # Remove incorrect categorizations
  #varnames$category = ifelse(str_detect(varnames$varnames,"NPP"),'P_inventory_names',varnames$category)                             

   lakevar = c("HU2","Year","Month","iws_ha","Elevation","lake_area_ha","MaxDepth","iws_slope_mean") # removed these variables: ,"iws_damdensity_pointsperha","iws_road_density_mperha","iws_canopy2001_mean","lake_perim_meters",
   group_names_for_default$lake_char_names = c(lakevar,as.character(na.omit(varnames[varnames$category == 'lake_char_names',]$varnames)))
   lake_group = rep("Date/Lake Characteristics",length(group_names_for_default$lake_char_names))
  
   #group_names_for_default$NLCD_names = c("nlcd_pct_0","nlcd_pct_11","nlcd_pct_31","nlcd_pct_52","nlcd_pct_71","NLCD_pct_developed","NLCD_pct_forest","NLCD_pct_farm","NLCD_pct_wetlands")
   group_names_for_default$NLCD_names = as.character(na.omit(varnames[varnames$category == 'NLCD_names',]$varnames))
   NLCD_group = rep("NLCD Land Use",length(group_names_for_default$NLCD_names))
   
   # group_names_for_default$P_inventory_names = c("P_Crop_removal.1","P_f_fertilizer.1","Legacy_P.1","P_livestock_Waste.1","P_nf_fertilizer.1","P_Deposition.1","P_human_waste_kg.1","P_kg_ww.1","Recovered_P.1",
   #                       "P_Ag_Surplus.1","NAPI.1","P_Ag_Inputs.1","P_Anthro_Inputs.1")
   group_names_for_default$P_inventory_names = as.character(na.omit(varnames[varnames$category == 'P_inventory_names',]$varnames))
   P_inventory_group = rep("P Inventory",length(group_names_for_default$P_inventory_names))
   
   # group_names_for_default$N_inventory_names = c("N_CBNF.1","N_Crop_N_Rem.1","N_Fert_Farm.1","N_Fert_Urban.1","N_Forest_Fire.1","Human_N_Demand.1","N_Human_Waste.1","N_Imp_Balance.1",
   #                       "N_Livestock_Waste.1","N_Manure_Recov.1","N_Rock.1","N_Total_Deposition.1","N_Total_nBNF.1","N_Ag_Surplus.1","NANI.1","N_Ag_Inputs.1","N_Anthro_Inputs.1")
   group_names_for_default$N_inventory_names = as.character(na.omit(varnames[varnames$category == 'N_inventory_names',]$varnames))
   N_inventory_group = rep("N Inventory",length(group_names_for_default$N_inventory_names))
   
   #group_names_for_default$vegetation_names = c("Vegetation","Vegetation_Lag1","Vegetation_Lag2","Vegetation_Lag3","Vegetation_YrMean","NPP","NPP_Lag1","NPP_Lag2","NPP_Lag3","NPP_YrMean")
   group_names_for_default$vegetation_names = as.character(na.omit(varnames[varnames$category == 'vegetation_names',]$varnames))
   vegetation_group = rep("Vegetation related",length(group_names_for_default$vegetation_names))
   
   # group_names_for_default$weather_names = c("SNOW","SNOW_Lag1","SNOW_Lag2","SNOW_Lag3","SNOW_YrMean","FIRE","FIRE_Lag1","FIRE_Lag2","FIRE_Lag3","FIRE_YrMean","LST","LST_Lag1","LST_Lag2","LST_Lag3","LST_YrMean","LSTAnomaly",
   #                   "LSTAnomaly_Lag1","LSTAnomaly_Lag2","LSTAnomaly_Lag3","LSTAnomaly_YrMean","Precip","Precip_Lag1","Precip_Lag2","Precip_Lag3","Precip_YrMean","Tmax","Tmax_Lag1","Tmax_Lag2",
   #                   "Tmax_Lag3","Tmax_YrMean","Tmean","Tmean_Lag1","Tmean_Lag2","Tmean_Lag3","Tmean_YrMean","Tmin","Tmin_Lag1","Tmin_Lag2","Tmin_Lag3","Tmin_YrMean")
   group_names_for_default$weather_names = as.character(na.omit(varnames[varnames$category == 'weather_names',]$varnames))
   weather_group = rep("Weather related",length(group_names_for_default$weather_names))
   
   # group_names_for_default$aerosol_names = c("AerosolSize_Aqua","AerosolSize_Aqua_Lag1","AerosolSize_Aqua_Lag2","AerosolSize_Aqua_Lag3","AerosolSize_Aqua_YrMean","AerosolSize_Terra","AerosolSize_Terra_Lag1",
   #                   "AerosolSize_Terra_Lag2","AerosolSize_Terra_Lag3","AerosolSize_Terra_YrMean","AOD_Aqua","AOD_Aqua_Lag1","AOD_Aqua_Lag2","AOD_Aqua_Lag3","AOD_Aqua_YrMean","AOD_Terra",
   #                   "AOD_Terra_Lag1","AOD_Terra_Lag2","AOD_Terra_Lag3","AOD_Terra_YrMean")
   group_names_for_default$aerosol_names = as.character(na.omit(varnames[varnames$category == 'aerosol_names',]$varnames))
   aerosol_group = rep("Aerosol related",length(group_names_for_default$aerosol_names))
   
   #group_names_for_default$deposition_names = c("Atmo_Pdep","Atmo_Pdep_Year.Lag1","Tot_Ndep","Tot_Ndep_Year.Lag1","Tot_Sdep","Tot_Sdep_Year.Lag1")
   group_names_for_default$deposition_names = as.character(na.omit(varnames[varnames$category == 'deposition_names',]$varnames))
   deposition_group = rep("Deposition",length(group_names_for_default$deposition_names))
   
   default_128 <- c(group_names_for_default$lake_char_names,group_names_for_default$NLCD_names,group_names_for_default$aerosol_names,group_names_for_default$weather_names,group_names_for_default$P_inventory_names,group_names_for_default$N_inventory_names,group_names_for_default$vegetation_names,group_names_for_default$deposition_names)
   
   data_to_model$default_variable_names = default_128
   
   save(default_128,file="./default_variables.RData")
   data_to_model$group_for_names = c(lake_group,NLCD_group,aerosol_group,weather_group,P_inventory_group,N_inventory_group,vegetation_group,deposition_group)
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
    
    # this part is to update and populate the choices (minimum and maximum values) for all the SelectInput and sliderInput 
    # in the later tabPanels after the dataset is loaded
    
    sliderMinMax <- function(myColumn){
      sliderMin = floor(min(myColumn,na.rm=TRUE))
      sliderMax = ceiling(max(myColumn,na.rm=TRUE))
      return(c(sliderMin,sliderMax))
    }
    
    sliders_data <- data %>% 
      dplyr::select(Year,Month,iws_ha,Elevation,lake_area_ha,MaxDepth,TN,LogTN,TP,LogTP) # iws_perimkm, ,lake_perim_meters 
    
    sliders_value_list <- lapply(sliders_data,sliderMinMax)
    data_to_model$sliders_value_list <- sliders_value_list
    
    updateSliderInput(session,"year_range",min=sliders_value_list$Year[1],max=sliders_value_list$Year[2],value=c(sliders_value_list$Year[1],sliders_value_list$Year[2])) 
    updateSliderInput(session,"month_range",min=sliders_value_list$Month[1],max=sliders_value_list$Month[2],value=c(sliders_value_list$Month[1],sliders_value_list$Month[2]))   
    updateSliderInput(session,"iws_ha_range",min=sliders_value_list$iws_ha[1],max=sliders_value_list$iws_ha[2],value=c(sliders_value_list$iws_ha[1],sliders_value_list$iws_ha[2]))   
    #updateSliderInput(session,"iws_perimkm_range",min=sliders_value_list$iws_perimkm[1],max=sliders_value_list$iws_perimkm[2],value=c(sliders_value_list$iws_perimkm[1],sliders_value_list$iws_perimkm[2]))   
    updateSliderInput(session,"elevation_range",min=sliders_value_list$Elevation[1],max=sliders_value_list$Elevation[2],value=c(sliders_value_list$Elevation[1],sliders_value_list$Elevation[2]))   
    updateSliderInput(session,"lake_area_range",min=sliders_value_list$lake_area_ha[1],max=sliders_value_list$lake_area_ha[2],value=c(sliders_value_list$lake_area_ha[1],sliders_value_list$lake_area_ha[2]))   
    #updateSliderInput(session,"lake_perimeter_range",min=sliders_value_list$lake_perim_meters[1],max=sliders_value_list$lake_perim_meters[2],value=c(sliders_value_list$lake_perim_meters[1],sliders_value_list$lake_perim_meters[2]))   
    #updateSliderInput(session,"lake_meanDepth_range",min=sliders_value_list$MeanDepth[1],max=sliders_value_list$MeanDepth[2],value=c(sliders_value_list$MeanDepth[1],sliders_value_list$MeanDepth[2]))   
    updateSliderInput(session,"lake_maxDepth_range",min=sliders_value_list$MaxDepth[1],max=sliders_value_list$MaxDepth[2],value=c(sliders_value_list$MaxDepth[1],sliders_value_list$MaxDepth[2]))   
    updateSliderInput(session,"tn_range",min=sliders_value_list$TN[1],max=sliders_value_list$TN[2],value=c(sliders_value_list$TN[1],sliders_value_list$TN[2]))   
    updateSliderInput(session,"LogTN_range",min=sliders_value_list$LogTN[1],max=sliders_value_list$LogTN[2],value=c(sliders_value_list$LogTN[1],sliders_value_list$LogTN[2]))   
    updateSliderInput(session,"tp_range",min=sliders_value_list$TP[1],max=sliders_value_list$TP[2],value=c(sliders_value_list$TP[1],sliders_value_list$TP[2]))   
    updateSliderInput(session,"LogTP_range",min=sliders_value_list$LogTP[1],max=sliders_value_list$LogTP[2],value=c(sliders_value_list$LogTP[1],sliders_value_list$LogTP[2]))   
    
    unique_programNames <- unique(sort(data$programname))
    unique_programTypes <- unique(sort(data$programtype))
    unique_HU2 <- unique(sort(data$HU2))
    updateSelectizeInput(session,"selected_program_name",choices = c("",unique_programNames),selected=NULL) 
    updateSelectizeInput(session,"selected_program_type",choices = c("",unique_programTypes),selected=NULL)
    updateSelectizeInput(session,"selected_HUC2",choices = c("",unique_HU2),selected=NULL)
    
    data_to_map <- data %>% dplyr::select(Lat,Long,TN,TP,LogTN,LogTP)
    rm(data,sliders_data)
    
    #print(paste0("inside displaying leaflet map now, the number of columns to map is:",length(data_to_map)))
    
    #print(input$select_color)
    data_to_map <- data_to_map[!is.na(as.numeric(as.character(data_to_map$Lat)))&!is.na(as.numeric(as.character(data_to_map$Long))),]
    unique_lakes_LogTP <- data_to_map %>% group_by(Lat,Long) %>% summarise(LogTP=mean(LogTP,na.rm=TRUE)) 
    unique_lakes_LogTN <- data_to_map %>% group_by(Lat,Long) %>% summarise(LogTN=mean(LogTN,na.rm=TRUE))
    unique_lakes_tp <- data_to_map %>% group_by(Lat,Long) %>% summarise(TP=mean(TP,na.rm=TRUE)) 
    unique_lakes_tn <- data_to_map %>% group_by(Lat,Long) %>% summarise(TN=mean(TN,na.rm=TRUE)) 
    unique_lakes_LogTP <- na.omit(unique_lakes_LogTP)
    unique_lakes_LogTN <- na.omit(unique_lakes_LogTN)
    unique_lakes_tp <- na.omit(unique_lakes_tp)
    unique_lakes_tn <- na.omit(unique_lakes_tn)
    
    
    color_func_LogTP=colorNumeric(palette=input$select_color,domain = unique_lakes_LogTP$LogTP)
    color_func_LogTN=colorNumeric(palette=input$select_color,domain = unique_lakes_LogTN$LogTN)
    color_func_tp=colorQuantile(palette=input$select_color,domain = unique_lakes_tp$TP,n=4)
    color_func_tn=colorQuantile(palette=input$select_color,domain = unique_lakes_tn$TN,n=4)
    #print(length(input$end_points_choice))
    
    quartile_labels <- c("1st Quartile","2nd Quartile","3rd Quartile","4th Quartile")
    
    ## loop through radio button choices for endpoint options
    
    if (input$end_points_choice=="LogTP") {
      myMap <- leaflet(unique_lakes_LogTP) %>% addTiles() %>% addResetMapButton() %>%
        setView(lng=-80.8,lat=40,zoom=4) %>%
        addCircleMarkers(lat=~Lat,lng=~Long,radius=2,color=~color_func_LogTP(LogTP),group="lakes") %>%
        addLegend(pal=color_func_LogTP,values=~LogTP,group="lakes",position='topright',opacity=1)
    }
    else if (input$end_points_choice=="LogTN") {
      myMap <- leaflet(unique_lakes_LogTN) %>% addTiles() %>% addResetMapButton() %>%
        setView(lng=-80.8,lat=40,zoom=4) %>%
        addCircleMarkers(lat=~Lat,lng=~Long,radius=2,color=~color_func_LogTN(LogTN),group="lakes") %>%
        addLegend(pal=color_func_LogTN,values=~LogTN,group="lakes",position='topright',opacity=1)
    }
    else if (input$end_points_choice=="TP") {
      myMap <- leaflet(unique_lakes_tp) %>% addTiles() %>% addResetMapButton() %>%
        setView(lng=-80.8,lat=40,zoom=4) %>%
        addCircleMarkers(lat=~Lat,lng=~Long,radius=2,color=~color_func_tp(TP),group="lakes") %>%
        addLegend(pal=color_func_tp,values=~TP,group="lakes",position='topright',opacity=1,labFormat = function(type,cuts,p){paste0(quartile_labels)})
    }
    else if (input$end_points_choice=="TN") {
      myMap <- leaflet(unique_lakes_tn) %>% addTiles() %>% addResetMapButton() %>%
        setView(lng=-80.8,lat=40,zoom=4) %>%
        addCircleMarkers(lat=~Lat,lng=~Long,radius=2,color=~color_func_tn(TN),group="lakes") %>%
        addLegend(pal=color_func_tn,values=~TN,group="lakes",position='topright',opacity=1,labFormat = function(type,cuts,p){paste0(quartile_labels)})
    }
    
    ## add HUC2 layer to the map
    
    myMap <- myMap %>%
      addPolygons(data=all_joined_HU2,
                  fillColor = "None",  
                  color="red",
                  smoothFactor = 0.5,
                  weight=2,
                  label=~huc2,
                  labelOptions = labelOptions(noHide=TRUE,textOnly =TRUE,opacity=0.5,textsize='18px',style =list("font-weight"="bold",padding="3px 8px") ),
                  group="HUC2")
    
    
    ## add states boundaries to the map
    myMap <- myMap %>%
      addPolygons(data=states_in_US_sp,
                  fillColor = "None",  
                  color="grey",
                  weight=2,
                  group="States")
    
    
    ## add layers control
    myMap <- myMap %>%
      addLegend(group="HUC2",position="bottomright",labels="HUC2",color="red") %>%
      addLegend(group="States",position="bottomright",labels="States",color="grey") %>%
      addLayersControl(
        position = c("bottomright"),
        baseGroups= c("lakes"),
        overlayGroups= c("States","HUC2"),
        options = layersControlOptions(collapsed=F, autoIndex = F)
      )
    
  })
  
  ## R function to generate groups 
  
  generateGroupList <- function(data){
    
    selected_data_1 <- data %>% 
      select(HU2,HU8,programname,programtype,Year,Month,Date,Day,iws_ha,
             lake_area_ha,iws_slope_mean,MaxDepth, 
             Name,Lat,Long,Elevation,
             TN,LogTN,TP,LogTP) # MeanDepth
    # Removed these variables: iws_damdensity_pointsperha, IWS_NHDID,borderiws,iws_tri_mean,,iws_road_density_mperha,iws_canopy2001_mean,iws_perimkm,lake_perim_meters,
    # chla, colora, colort, dkn, doc, nh4, no2, no2no3, srp, tdn, tdp, tkn, toc, ton, secchi, LAGOS_Lake_ID,iws_lakeareaha,HU8ZoneID,IWS_ZoneID,iws_pct_in_usa,
    
    selected_data_Lake <- data %>%
      select(iws_ha,lake_area_ha,MaxDepth,
             Name,Lat,Long,Elevation,iws_slope_mean)
    # removed these variables: iws_damdensity_pointsperha, IWS_NHDID,borderiws,iws_tri_mean,iws_road_density_mperha,iws_canopy2001_mean,
    # ,iws_pct_in_usa, HU8ZoneID,IWS_ZoneID,iws_lakeareaha,iws_perimkm,MeanDepth, lake_perim_meters,
    
    selected_data_Land <- data %>%
      #select(matches("^nlcd_pct_.*"))
      select(matches("^nlcd.*")|matches("^NLCD_.*")|matches("^NLCD.*")|matches("^landuse.*")|matches("^LANDUSE.*")) # mjp updated
    
    
    selected_data_Weather <- data %>%
      select(matches("^SNOW.*")|matches("^FIRE.*")|matches("^LST.*")|matches("^Tmax.*")|matches("^Tmin.*")|matches("^Tmean.*")|matches("^Precip.*"))
    # select(SNOW,SNOW_Lag1,SNOW_Lag2,SNOW_Lag3,SNOW_YrMean,FIRE,FIRE_Lag1,FIRE_Lag2,FIRE_Lag3,FIRE_YrMean,LST,LST_Lag1,LST_Lag2,LST_Lag3,LST_YrMean,LSTAnomaly,
    #      LSTAnomaly_Lag1,LSTAnomaly_Lag2,LSTAnomaly_Lag3,LSTAnomaly_YrMean,Precip,Precip_Lag1,Precip_Lag2,Precip_Lag3,
    #      Precip_YrMean,Tmax,Tmax_Lag1,Tmax_Lag2,Tmax_Lag3,Tmax_YrMean,Tmean,Tmean_Lag1,Tmean_Lag2,Tmean_Lag3,Tmean_YrMean,Tmin,Tmin_Lag1,Tmin_Lag2,Tmin_Lag3,
    #      Tmin_YrMean)
    
    selected_data_Vegetation <- data %>%
      #select(NPP,NPP_Lag1,NPP_Lag2,NPP_Lag3,NPP_YrMean,Vegetation,Vegetation_Lag1,Vegetation_Lag2,Vegetation_Lag3,Vegetation_YrMean)
      select(matches("^NPP.*")|matches("^Vegetation_.*"))
    
    selected_data_Aerosol <- data %>%
      select(matches("^AerosolSize_.*")|matches("^AOD_.*"))
    
    ## YD commented out, remove the "N_Inventory_raw" group
    # selected_data_N <- data %>%
    # select((matches("^N_.*")&!matches(".*\\.1$"))|matches("^Human_N_.*Demand$"))
    # mjp changed
    #save(selected_data_N, file="test_selected_data_N.RData")
    
    selected_data_N_Scaled <- data %>%
      select(matches("^N_.*\\.1$")|matches("^Human_N_.*\\.1$")|matches("^NANI.*\\.1$"))
    ## YD commented out, remove the "P_Inventory_raw" group
    # selected_data_P <- data %>%
    # select((matches("^P_.*")&!matches(".*\\.1$"))|matches("^Legacy_.*P$")|matches("^Recovered_.*P$"))
    
    selected_data_P_Scaled <- data %>%
      select(matches("^P_.*\\.1$")|matches("^Legacy_P.*\\.1$")|matches("^Recovered_P.*\\.1$")|matches("^NAPI.*\\.1$"))
    
    selected_data_Deposition <- data %>%
      select(matches("^Atmo_Pdep.*")|matches("^Tot_Ndep.*")|matches("^Tot_Sdep.*"))
    
    # selected_data_Sampling_Info <- data %>%
    # select(matches(".*\\_qual$")|matches(".*\\_censorcode$")|matches(".*\\_detectionlimit$")|matches(".*\\_labmethodname$")|matches(".*\\_methodinfo$"))
    ## YD revised, removed two groups "selected_data_N" and "selected_data_P" from "myList"
    myList <- list(selected_data_1=selected_data_1,selected_data_Lake=selected_data_Lake,selected_data_Land=selected_data_Land,selected_data_Weather=selected_data_Weather,
                   selected_data_Vegetation=selected_data_Vegetation,selected_data_Aerosol=selected_data_Aerosol,selected_data_N_Scaled=selected_data_N_Scaled,
                   selected_data_P_Scaled=selected_data_P_Scaled,selected_data_Deposition=selected_data_Deposition) # ,selected_data_Sampling_Info=selected_data_Sampling_Info
    return(myList)
  }
  
  ## to display the summary table and associated elements
  
  output$display_variable_groups <- renderUI({
    if (length(data) > 0 ) {
      radioButtons("variable_groups", "Variable group options", choices = c("Default Group" = "Selected by default","NLCD Land Use"="NLCD Land Use",
                                                                            "Weather related"="Weather related", "Vegetation related"="Vegetation related","Aerosol related"="Aerosol related",
                                                                            "N Inventory"="N Inventory","P Inventory"="P Inventory","Deposition"="Deposition"  ## YD fixed the typo in "Deposition" 
      ),#"Sampling Information"="Sampling Information"
      selected = "Selected by default") 
    }
  })
  
  observeEvent(input$displayTable, {
    
<<<<<<< HEAD
    # print("After rendering the map: ")
    # print(pryr::mem_used())
    shinyjs::show("summaryCP")
    
    
    output$display_table <- renderUI({
      if (length(data) > 0 ) {
        div(withSpinner(dataTableOutput("display_summary_table"),type=2),style="font-size:110%;font-style:bold")
      }
    })
    
    output$display_table_cat <- renderUI({
      if (length(data) > 0 ) {
        div(withSpinner(dataTableOutput("display_summary_table_cat"),type=2),style="font-size:110%;font-style:bold")
      }
    })
  }) ## observeEvent end, YD added
  
  ## YD added this observeEvent to make the table change with the radioButton choices
  observeEvent(input$variable_groups, {
    
    data <- loaded_data()
    req(input$variable_groups)
    
    ## generate group list and initialize group names
    
    myList <- generateGroupList(data)
    
    selected_data_1 <- myList$selected_data_1
    selected_data_Lake <- myList$selected_data_Lake
    group_names$Lake_Character = colnames(selected_data_Lake)
    selected_data_Land <- myList$selected_data_Land
    group_names$NLCD_Land_Use = colnames(selected_data_Land)
    selected_data_Weather <- myList$selected_data_Weather
    group_names$Weather_related = colnames(selected_data_Weather)
    selected_data_Vegetation <- myList$selected_data_Vegetation
    group_names$Vegetation = colnames(selected_data_Vegetation)
    selected_data_Aerosol <- myList$selected_data_Aerosol
    group_names$Aerosol_related = colnames(selected_data_Aerosol)
    ##selected_data_N <- myList$selected_data_N  ## YD commented out
    ##group_names$Aerosol_related = colnames(selected_data_Aerosol) ## YD commented out
    selected_data_N_Scaled <- myList$selected_data_N_Scaled
    group_names$N_Inventory_Scaled = colnames(selected_data_N_Scaled)
    # selected_data_P <- myList$selected_data_P    ## YD commented out
    # group_names$P_Inventory_Raw = colnames(selected_data_P)   ## YD commented out
    selected_data_P_Scaled <- myList$selected_data_P_Scaled
    group_names$P_Inventory_Scaled = colnames(selected_data_P_Scaled)
    selected_data_Deposition <- myList$selected_data_Deposition
    group_names$Deposition = colnames(selected_data_Deposition)
    #selected_data_Sampling_Info <- myList$selected_data_Sampling_Info
    #group_names$Sampling_Info = colnames(selected_data_Sampling_Info)
    #save(myList,file="./test_myList.RData")
    rm(myList)
    
    myChoice<- input$variable_groups
    
    print(myChoice)
    
    if (myChoice=="Selected by default") {
      selected_to_summary$selected_data <- selected_data_1 ## only use group 1 by default
    }else if(myChoice=="NLCD Land Use"){
      selected_to_summary$selected_data <- selected_data_Land
    }else if(myChoice=="Weather related"){
      selected_to_summary$selected_data <- selected_data_Weather 
    }else if(myChoice=="Vegetation related"){
      selected_to_summary$selected_data <- selected_data_Vegetation 
    }else if(myChoice=="Aerosol related"){
      selected_to_summary$selected_data <- selected_data_Aerosol 
    }else if(myChoice=="N Inventory"){
      selected_to_summary$selected_data <- selected_data_N_Scaled 
    }else if(myChoice=="P Inventory"){
      selected_to_summary$selected_data <- selected_data_P_Scaled
    }else if(myChoice=="Deposition"){
      selected_to_summary$selected_data <- selected_data_Deposition  #YD fixed typo in "Deposition"
    } #else if(myChoice=="Sampling Information"){
    #  selected_to_summary$selected_data <- selected_data_Sampling_Info
    #}
    ## YD removed two groups from this line below
    rm(selected_data_1,selected_data_Lake,selected_data_Land,selected_data_Aerosol,selected_data_N_Scaled,selected_data_P_Scaled,selected_data_Deposition) #,selected_data_Sampling_Info)
    
    print(length(selected_to_summary$selected_data))
    ## YD added this if else loop to check if we have any variables in the selected group
    ## if zero, no need to calculate summary
    if (length(selected_to_summary$selected_data)>0){
      ##YD added this loop because some numeric variables in this group were treated as categorical variables 
      ##when have very few unique values
      if (myChoice=="Aerosol related"){
        summary_statistics_calculated <- selected_to_summary$selected_data %>%
          tbl_summary(type = list(all_continuous()~"continuous",all_categorical()~"continuous"),
                      statistic=list(all_continuous()~"{min};{mean};{median};{max};{sd}"),
                      digits = all_continuous()~2,
                      missing_text="(Missing)") 
      }else{
        summary_statistics_calculated <- selected_to_summary$selected_data %>%
          tbl_summary(
            statistic=list(all_continuous()~"{min};{mean};{median};{max};{sd}",
                           all_categorical()~"{n};{N};{p}%"),
            digits = all_continuous()~2,
            missing_text="(Missing)") 
      }
=======
   # print("After rendering the map: ")
   # print(pryr::mem_used())
      shinyjs::show("summaryCP")
      
      
      output$display_table <- renderUI({
        if (length(data) > 0 ) {
          div(withSpinner(dataTableOutput("display_summary_table"),type=2),style="font-size:110%;font-style:bold")
        }
      })
      
      output$display_table_cat <- renderUI({
        if (length(data) > 0 ) {
          div(withSpinner(dataTableOutput("display_summary_table_cat"),type=2),style="font-size:110%;font-style:bold")
        }
      })
  }) ## observeEvent end, YD added
      
  ## YD added this observeEvent to make the table change with the radioButton choices
  observeEvent(input$variable_groups, {
         
      data <- loaded_data()
      req(input$variable_groups)
      
      ## generate group list and initialize group names
      
      myList <- generateGroupList(data)
      
      selected_data_1 <- myList$selected_data_1
      selected_data_Lake <- myList$selected_data_Lake
      group_names$Lake_Character = colnames(selected_data_Lake)
      selected_data_Land <- myList$selected_data_Land
      group_names$NLCD_Land_Use = colnames(selected_data_Land)
      selected_data_Weather <- myList$selected_data_Weather
      group_names$Weather_related = colnames(selected_data_Weather)
      selected_data_Vegetation <- myList$selected_data_Vegetation
      group_names$Vegetation = colnames(selected_data_Vegetation)
      selected_data_Aerosol <- myList$selected_data_Aerosol
      group_names$Aerosol_related = colnames(selected_data_Aerosol)
      ##selected_data_N <- myList$selected_data_N  ## YD commented out
      ##group_names$Aerosol_related = colnames(selected_data_Aerosol) ## YD commented out
      selected_data_N_Scaled <- myList$selected_data_N_Scaled
      group_names$N_Inventory_Scaled = colnames(selected_data_N_Scaled)
      # selected_data_P <- myList$selected_data_P    ## YD commented out
      # group_names$P_Inventory_Raw = colnames(selected_data_P)   ## YD commented out
      selected_data_P_Scaled <- myList$selected_data_P_Scaled
      group_names$P_Inventory_Scaled = colnames(selected_data_P_Scaled)
      selected_data_Deposition <- myList$selected_data_Deposition
      group_names$Deposition = colnames(selected_data_Deposition)
      #selected_data_Sampling_Info <- myList$selected_data_Sampling_Info
      #group_names$Sampling_Info = colnames(selected_data_Sampling_Info)
      #save(myList,file="./test_myList.RData")
      rm(myList)
      
      myChoice<- input$variable_groups
        
      print(myChoice)
      
      if (myChoice=="Selected by default") {
        selected_to_summary$selected_data <- selected_data_1 ## only use group 1 by default
      }else if(myChoice=="NLCD Land Use"){
        selected_to_summary$selected_data <- selected_data_Land
      }else if(myChoice=="Weather related"){
        selected_to_summary$selected_data <- selected_data_Weather 
      }else if(myChoice=="Vegetation related"){
        selected_to_summary$selected_data <- selected_data_Vegetation 
      }else if(myChoice=="Aerosol related"){
        selected_to_summary$selected_data <- selected_data_Aerosol 
      }else if(myChoice=="N Inventory"){
        selected_to_summary$selected_data <- selected_data_N_Scaled 
      }else if(myChoice=="P Inventory"){
        selected_to_summary$selected_data <- selected_data_P_Scaled
      }else if(myChoice=="Deposition"){
        selected_to_summary$selected_data <- selected_data_Deposition  #YD fixed typo in "Deposition"
        } #else if(myChoice=="Sampling Information"){
      #  selected_to_summary$selected_data <- selected_data_Sampling_Info
      #}
      ## YD removed two groups from this line below
      rm(selected_data_1,selected_data_Lake,selected_data_Land,selected_data_Aerosol,selected_data_N_Scaled,selected_data_P_Scaled,selected_data_Deposition) #,selected_data_Sampling_Info)
      
      print(length(selected_to_summary$selected_data))
      ## YD added this if else loop to check if we have any variables in the selected group
      ## if zero, no need to calculate summary
      if (length(selected_to_summary$selected_data)>0){
        ##YD added this loop because some numeric variables in this group were treated as categorical variables 
        ##when have very few unique values
        if (myChoice=="Aerosol related"){
            summary_statistics_calculated <- selected_to_summary$selected_data %>%
            tbl_summary(type = list(all_continuous()~"continuous",all_categorical()~"continuous"),
                        statistic=list(all_continuous()~"{min};{mean};{median};{max};{sd}"),
                        digits = all_continuous()~2,
                        missing_text="(Missing)") 
        }else{
             summary_statistics_calculated <- selected_to_summary$selected_data %>%
             tbl_summary(
                         statistic=list(all_continuous()~"{min};{mean};{median};{max};{sd}",
                         all_categorical()~"{n};{N};{p}%"),
                         digits = all_continuous()~2,
                         missing_text="(Missing)") 
        }
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
      summary_table <- summary_statistics_calculated[[1]] ## only carry on the statistics data frame
      #save(summary_table,file="./test_summary_table.RData")
      
      ## split the summary table into two tables, one for continuous variables and the other for categoricals
      summary_continuous <- summary_table[summary_table$var_type=="continuous",]
      summary_continuous_no_missing <- summary_continuous[summary_continuous$row_type=="label",c(1,6)]
      table_for_continuous <- summary_continuous_no_missing %>% separate(stat_0,c("minimum","mean","median","maximum","sd"),sep="\\;")
      
      summary_cat <- summary_table[summary_table$var_type=="categorical",]
      summary_cat_all <- summary_cat[summary_cat$row_type=="level",c(1,5,6)]
      table_for_cat <- summary_cat_all %>% separate(stat_0,c("number of data points","total number of data points","percentage of data points"),sep="\\;")
      table_for_cat <- table_for_cat[,c(1:3,5)]
      rm(summary_statistics_calculated) ## remove the unwanted list
      
      output$display_summary_table <- DT::renderDataTable({
        print("inside renderDT now")
        
        DT::datatable(
          data.frame(table_for_continuous),
          class = "display nowrap compact", # style
          #filter = "top", #location of column filters
          options = list(
            paging=FALSE,
            searching=FALSE,
            info=FALSE,
            columnDefs=list(list(className='dt-center',targets=c(1,2,3,4,5)))  ## YD added targets for newer version of R
          )
        ) # dataTable end
      }) #renderDT end
      
      
      output$display_summary_table_cat <- DT::renderDataTable({
        print("inside renderDT now")
        
        DT::datatable(
          data.frame(table_for_cat),
          class = "display nowrap compact", # style
          #filter = "top", #location of column filters
          colnames=c('variable','level','number of data points','percentage of the total data points'),
          options = list(
            scrollX = TRUE, #allow user to scroll wide tables horizontally
            stateSave = FALSE,
            info=FALSE,
            pageLength = 10,
            searchCols = default_search_columns,
            search = list(regex=FALSE,caseInsensitive = FALSE, search = default_search)
          )
        ) # dataTable end
      }) #renderDT end
      
      # output$variable_list_table <- DT::renderDataTable(
      #   data.frame(variable_names = colnames(selected_to_summary$selected_data)),
      #   options = list(
      #     paging = TRUE,
      #     pageLength = 10,
      #     fixedColumns = TRUE
      #   )
      # ) #renderDT end
      
      output$summary_table_overall_text <- renderText({
        Note1_text= paste0("Note 1: There are ", length(data), " variables in total for this dataset,","they are divided into 8 groups as listed under 'Variable group options'.")
        Note2_text = paste0("Note 2: You can use the radio buttons on the right to select a variable group to see their summary statistics.")
        paste(Note1_text,Note2_text,sep="\n")
      }) 
      
      
      output$summary_table_text <- renderText({
        Note3_text=paste0("Note 3: The above table displays all continuous variables (", nrow(table_for_continuous)," in total) from the selected group.") 
      }) 
      
      how_many = length(unique(table_for_cat$variable))
      
      output$summary_table_text_cat <- renderText({
        if (nrow(table_for_cat)>0){
          Note4_text=paste0("Note 4: The above table displays all categorical variables (", how_many," in total) from the selected group.") 
        } else{
          Note_text= paste0("Note 4: No categorical variables from the selected group.")
        }  ##inner if else loop end
      })  ## renderText end
    }else{  ## YD added code line 930-940 to reset the tables when no variable in the group
      output$display_summary_table <- DT::renderDataTable({
      })
      output$display_summary_table_cat <- DT::renderDataTable({
      })
      output$summary_table_text <- renderText({
        Note3_text=paste0("Note 3: No continuous variables from the selected group.") 
      }) 
      output$summary_table_text_cat <- renderText({
        Note_text= paste0("Note 4: No categorical variables from the selected group.")
      })
    }
    
  }) #observeEvent end
  
  
  
  #######################################################################################################################
  ######################################## server code end for tabPanel "Load Data"######################################
  #######################################################################################################################
  
  
  #####################################################################################################################
  ######################################## server code start for tabPanel "Explore Data"##################################
  #####################################################################################################################
  
  
  observeEvent(input[["tabset"]], {
    
    
    output$display_plot1 <- renderUI({
      withSpinner(plotOutput("plot1"),type=2)
    })
    
    output$end_points_option_in_summary <- renderUI({
      div(radioButtons("end_points_in_summary",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=FALSE),
          style= "margin-left:20px;font-style:bold")
    })
    
    output$display_plot2a <- renderUI({
      withSpinner(plotlyOutput("plot2a"),type=2)
    })
    
    output$display_plot2b <- renderUI({
      withSpinner(plotlyOutput("plot2b"),type=2)
    })
    
    output$end_points_option_in_series <- renderUI({
      div(radioButtons("end_points_in_series",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:50px;font-style:bold")
    })
    
    output$display_plot3 <- renderUI({
      withSpinner(plotOutput("plot3"),type=2)
    })
    
    output$end_points_option_in_HUC2 <- renderUI({
      div(radioButtons("end_points_in_HUC2",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:-200px;font-style:bold")
    })
    
    output$display_plot4 <- renderUI({
      withSpinner(plotOutput("plot4"),type=2)
    })
    
    output$end_points_option_in_HUC2_box <- renderUI({
      div(radioButtons("end_points_in_HUC2_box",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:50px;font-style:bold")
    })
    
    ## this part is added to provide a tab for predictor variables HUC2 summary
    output$select_variable <- renderUI({
      all_predictors = data_to_model$default_variable_names
      my_choices = all_predictors[!all_predictors %in% c("HU2","Year","Month")]
      selectizeInput("select_variable_to_summary",label ="Select one predictor variable",
                     choices= my_choices,
                     multiple = FALSE,
                     selected="P_Anthro_Inputs.1",
                     options = list(hideSelected = FALSE))
    })
    
    output$display_Y_axis_slider <- renderUI({
      sliderInput("Y_axis_scale","Adjust Y axis scale",min=0.05,max=1,value=1,step=0.05,sep="")
    })
    
    output$display_plot8 <- renderUI({
      withSpinner(plotOutput("plot8"),type=2)
    })
    
    output$end_points_option_in_Misc <- renderUI({
      div(radioButtons("end_points_in_Misc",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:50px;font-style:bold")
    })
    
    output$session1_title <- renderText({
      HTML("For plot on the left")
    })
    
    output$select_x1 <- renderUI({
      selectizeInput("select_bivariate_x1",label ="select one predictor variable(X1)",
                     choices=data_to_model$default_variable_names,
                     multiple = FALSE,
                     selected="Vegetation",
                     options = list(hideSelected = FALSE))
    })
    
    output$select_y1 <- renderUI({
      selectizeInput("select_bivariate_y1",label ="select one endpoint(Y1)",
                     choices=c("TN","TP","LogTN","LogTP"),
                     multiple = FALSE,
                     selected="LogTP",
                     options = list(hideSelected = FALSE))
    })
    
    output$session2_title <- renderText({
      HTML("For plot on the right")
    })
    
    output$select_x2 <- renderUI({
      selectizeInput("select_bivariate_x2",label ="select one predictor variable(X2)",
                     choices=data_to_model$default_variable_names,
                     multiple = FALSE,
                     selected="Tmax",
                     options = list(hideSelected = FALSE))
    })
    
    output$select_y2 <- renderUI({
      selectizeInput("select_bivariate_y2",label ="select one predictor variable(Y2)",
                     choices=data_to_model$default_variable_names,
                     multiple = FALSE,
                     selected="Tmin",
                     options = list(hideSelected = FALSE))
    })
    
    
    output$select_corr_end_points <- renderUI({
      selectizeInput("select_multivariate_end",label ="select endpoints",
                     choices=c("TN","TP","LogTN","LogTP"),
                     multiple = FALSE,
                     selected="LogTP",
                     options = list(hideSelected = FALSE))
    })
    
    output$select_corr_predictors <- renderUI({
      selectizeInput("select_multivariate_predictor",label ="select predictor variables",
                     choices=data_to_model$default_variable_names,
                     multiple = TRUE,
                     selected=c("Precip","MaxDepth","NLCD_pct_forest","NLCD_pct_farm"),
                     options = list(hideSelected = FALSE,plugins=list('remove_button'),maxItems=5))
    })
    
    output$reminder_note <- renderText({
      "Please limit the number of selected predictor variables no more than 5. \n\n"
    })
    
  }) #observe Event end
  
  
  observe({
    if(!is.null(input$select_bivariate_x2)){
      updateSelectInput(session,"select_bivariate_y2",choices=data_to_model$default_variable_names[!(data_to_model$default_variable_names %in% input$select_bivariate_x2 )],selected=isolate(input$select_bivariate_y2))
    }
  })
  
  
  observe({
    if(!is.null(input$select_bivariate_y2)){
      updateSelectInput(session,"select_bivariate_x2",choices=data_to_model$default_variable_names[!(data_to_model$default_variable_names %in% input$select_bivariate_y2 )],selected=isolate(input$select_bivariate_x2))
    }
  })
  
  ## for Tab 1:Summary Info
  
  output$plot1 <- renderPlot({
    myData <- loaded_data()
    req(input$end_points_in_summary)
    #save(myData,file="./test_data_before_subset.RData")
    myCol <- myData %>% select(input$end_points_in_summary) %>% na.omit() %>% summarise_all(list(mean=mean,median=median,min=min,max=max,var=var,sd=sd))
    Stats <- c("Mean","Median","Min","Max","Variance","SD")
    Value <- format(round(t(myCol),2),nsmall =2)
    Summ<-data.frame(Stats,Value)
    #print(Summ)
    
    p1 <- ggplot(myData, aes_string(x=input$end_points_in_summary)) + geom_histogram()+
      ylab("Frequency")+
      xlab(input$end_points_in_summary)+
      theme_classic()+ theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, 
                                                         linetype="solid"))+
      theme(text=element_text(size=15,face = "bold", color="blue"))
    
    mytheme <- gridExtra::ttheme_default(
      core = list(fg_params=list(cex = 1.0),padding=unit(c(12,12),'mm')),
      colhead = list(fg_params=list(cex = 1.5),padding=unit(c(8,8),'mm')),
      rowhead = list(fg_params=list(cex = 1.5)))
    
    tbl <- tableGrob(Summ,rows=NULL,theme = mytheme)
    grid.arrange(p1, tbl, ncol=2)
  })
  
  ## for Tab 2:Time Series
  
  output$plot2a <- renderPlotly({
    myData <- loaded_data()
    year_range <- max(myData$Year)-min(myData$Year)
    print(paste0("the year range is:",year_range))
    #meandf<-meandf[!duplicated(meandf[,c('programtype','Year')]),]
    if (year_range<5){
      break_month_num = 2
      x_lab_str = "Year-Month"
      x_date_label = "%Y-%m"
    }else if(year_range>=5&year_range<10){
      break_month_num = 6
      x_date_label = "%Y-%m"
      x_lab_str = "Year-Month"
    }else if(year_range>=10){
      break_month_num = 12
      x_lab_str ="Year"
      x_date_label = "%Y"
    }
    #print(paste0("the x lab in time series plot is:",x_lab_str))
    myBreaks = paste0(break_month_num," months")
    print(myBreaks)
    
    req(input$end_points_in_series)
    if (input$end_points_in_series=="LogTP"){
      myData_no_NA <- myData[,c('Year','Month','programtype','LogTP')] %>% na.omit()
      mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("LogTP",mean)
      meandf_LogTP<-mean[complete.cases(mean), ]
      p1<-ggplot(data =meandf_LogTP) +
        geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTP, color = programtype ))+
        geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTP, color = programtype ))+
        labs(x = x_lab_str,
             y = paste0("log","(TP, \u03bcg/L)"))+
        scale_x_date(date_labels=x_date_label,date_breaks=myBreaks)+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1) )
    }else if(input$end_points_in_series=="TP"){
      myData_no_NA <- myData[,c('Year','Month','programtype','TP')] %>% na.omit()
      mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("TP",mean)
      meandf_tp<-mean[complete.cases(mean), ]
      p1<-ggplot(data =meandf_tp) +
        geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TP, color = programtype ))+
        geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TP, color = programtype ))+
        labs(x = x_lab_str,
             y = paste0(input$end_points_in_series," (\u03bcg/L)"))+
        scale_x_date(date_labels=x_date_label,date_breaks=myBreaks)+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1) )
    }else if(input$end_points_in_series=="LogTN"){
      myData_no_NA <- myData[,c('Year','Month','programtype','LogTN')] %>% na.omit() 
      mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("LogTN",mean) 
      meandf_LogTN<-mean[complete.cases(mean), ]
      p1<-ggplot(data =meandf_LogTN) +
        geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTN, color = programtype ))+
        geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTN, color = programtype ))+
        labs(x = x_lab_str,
             y = paste0("log","(TN, \u03bcg/L)"))+
        scale_x_date(date_labels=x_date_label,date_breaks=myBreaks)+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))
    }else if(input$end_points_in_series=="TN"){
      myData_no_NA <- myData[,c('Year','Month','programtype','TN')] %>% na.omit()
      mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("TN",mean) 
      meandf_tn<-mean[complete.cases(mean), ]
      p1<-ggplot(data =meandf_tn) +
        geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TN, color = programtype ))+
        geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TN, color = programtype ))+
        labs(x = x_lab_str,
             y = paste0(input$end_points_in_series," (\u03bcg/L)"))+
        scale_x_date(date_labels=x_date_label,date_breaks=myBreaks)+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))
    }
    p1 <- ggplotly(p1)
  })
  
  output$plot2b <- renderPlotly({
    myData <- loaded_data()
    req(input$end_points_in_series)
    print("inside rendering plot2b now...")
    if (input$end_points_in_series=="LogTP"){
      p2<-ggplot(myData, aes(x=as.factor(Year), y=LogTP,fill=as.factor(Year))) + 
        geom_boxplot(fill=NA)+
        labs(x="Year", y = paste0("log","(TP, \u03bcg/L)"))+
        theme_classic()+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))#+
      #guides(fill=guide_legend(title="Year"))
    }else if(input$end_points_in_series=="TP"){
      p2<-ggplot(myData, aes(x=as.factor(Year), y=TP,fill=as.factor(Year))) + 
        geom_boxplot(fill=NA)+
        labs(x="Year", y = paste0(input$end_points_in_series," (\u03bcg/L)"))+
        theme_classic()+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))#+
      #guides(fill=guide_legend(title="Year"))
    }else if(input$end_points_in_series=="LogTN"){
      p2<-ggplot(myData, aes(x=as.factor(Year), y=LogTN,fill=as.factor(Year))) + 
        geom_boxplot(fill=NA)+
        labs(x="Year", y = paste0("log","(TN, \u03bcg/L)"))+
        theme_classic()+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))#+
      #guides(fill=guide_legend(title="Year"))
    }else if(input$end_points_in_series=="TN"){
      p2<-ggplot(myData, aes(x=as.factor(Year), y=TN,fill=as.factor(Year))) + 
        geom_boxplot(fill=NA)+
        labs(x="Year", y = paste0(input$end_points_in_series," (\u03bcg/L)"))+
        theme_classic()+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))#+
      #guides(fill=guide_legend(title="Year"))
    }
    
    p2 <- ggplotly(p2)
  })
  
  
  observe({
    if (req(input$tab3b)== "sb1"){
      ## for Tab 3:Endpoints HUC2 Summaries, sub-tab 1: HU2 frequency bar plot and pie chart
      output$plot3 <- renderPlot({
        myData <- loaded_data()
        req(input$end_points_in_HUC2)
        myData_1 <- myData %>% select(HU2,input$end_points_in_HUC2) %>% na.omit()
        p1 <- ggplot(myData_1,aes(x=HU2))+
          geom_bar(fill = "#0073C2FF")+
          ylab(paste0("Number of valid ",input$end_points_in_HUC2," data records"))+
          theme_classic()+
          theme(text=element_text(size=15,face = "bold",color="blue"),
                plot.title=element_text(hjust=0.5,size=14),
                axis.text.x=element_text(angle=0, hjust=1)) +
          scale_x_discrete("HUC 2 Zone ID", labels = as.character(myData$HU2), breaks = myData$HU2) + 
          theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, linetype="solid"))
        
        df <- data.frame(table(myData_1$HU2)) %>%
          mutate(Abs_Freq = format(round(as.numeric(Freq/sum(Freq)*100))/100, 2)) %>%
          rename(HU2=Var1)
        df$label <- scales::percent(as.numeric(df$Abs_Freq))
        df<-df[order(df$Freq),]
        p2<-ggplot(data=df)+
          ggtitle(paste0("Percentage of ",input$end_points_in_HUC2," data records"))+
          geom_col(aes(x="", y=as.numeric(Abs_Freq), fill=as.factor(HU2)), width = 1)+
          coord_polar("y", start=0)+
          theme_void()+
          geom_text(aes(x=1.6, y = as.numeric(Abs_Freq), label=label, group = as.factor(HU2)),
                    position = position_stack(vjust = 0.5))+ 
          guides(fill=guide_legend(title="HU2"))+
          theme(text=element_text(size=15,face = "bold", color="blue"),plot.title=element_text(hjust=0.5,size=15))+
          theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, linetype="solid"))
        grid.arrange(p1, p2, ncol=2)
      })
    }else if (req(input$tab3b)=="sb2"){
      ## for Tab 3:Endpoint HUC2 Summaries, sub-tab 2: HU2 frequency box plot
      output$plot4 <- renderPlot({
        
        myData<-loaded_data()
        req(input$end_points_in_HUC2_box)
        print("create the HU2 boxplot (plot4) now...")
        if (input$end_points_in_HUC2_box=="LogTP"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=LogTP,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+ theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0("log","(TP, \u03bcg/L)"))
        }else if (input$end_points_in_HUC2_box=="LogTN"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=LogTN,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+ theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0("log","(TN, \u03bcg/L)"))
        }else if (input$end_points_in_HUC2_box=="TP"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=TP,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+ theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0(input$end_points_in_HUC2_box," (\u03bcg/L)"))
        }else if (input$end_points_in_HUC2_box=="TN"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=TN,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+ theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0(input$end_points_in_HUC2_box," (\u03bcg/L)"))
        }
        p2 <- p2 + 
          theme_classic()+
          guides(fill=guide_legend(title="HUC2 Zone ID"))+ 
          theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5,linetype="solid"))+
          theme(text=element_text(size=15,face = "bold", color="blue"))
        print(p2)
      })
    }else if(req(input$tab3b)=="sb3"){
      
      ## for Tab 3:Endpoint HUC2 Summaries, sub-tab 3: HUC2 watershed map
      output$display_HUC2_watershed_map <- renderUI({
        imageOutput("HUC2_watershed_map")
      })
      
      output$HUC2_watershed_map <- renderImage({
        list(src="www/HUC2_location_map.png",
             contentType = 'image/png',
             width=800,
             height=600,
             alt='this is the HUC2 location map')
      },deleteFile = FALSE)
    } #if else loop end
  }) #observe end
  
  
  ## for tab3_add:Predictor variables HUC2 Summaries, predictor variables HU2 frequency box plot
  
  output$display_plot4_add <- renderUI({
    withSpinner(plotOutput("plot4_add"),type=2)
  })
  
  output$plot4_add <- renderPlot({
    myData<-loaded_data()
    req(input$select_variable_to_summary)
    print("create the HU2 boxplot (plot4 add) now...")
    
    variable_max = max(myData[,input$select_variable_to_summary],na.rm=TRUE)
    print(variable_max)
    
    y_axis_max = variable_max*input$Y_axis_scale
    
    p2 <- ggplot(myData, aes(x=as.factor(HU2), y=!!sym(input$select_variable_to_summary),fill=as.factor(HU2)))+
      geom_boxplot(fill=NA)+
      labs(x="HUC2 Zone ID", y = paste0(input$select_variable_to_summary))+
      theme_classic()+
      guides(fill=guide_legend(title="HUC2 Zone ID"))+ 
      theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5,linetype="solid"))+
      theme(text=element_text(size=15,face = "bold", color="blue"))+
      ylim(0,y_axis_max)
    print(p2)
    
  })
  
  ## for Tab 4: Correlations, sub-tab 1: bivariate
  observeEvent(input$display_bivariate_plots, {
    output$display_plot5 <- renderUI({
      withSpinner(plotOutput("plot5"),type=2)
    })
    myData <- loaded_data()
    output$plot5 <- renderPlot({
      input$display_bivariate_plots
      p1<-ggplot(myData, aes_string(x=isolate(input$select_bivariate_x1), y=isolate(input$select_bivariate_y1))) + 
        geom_point()+
        geom_smooth(formula = y ~ x, method=lm)+
        ylab(isolate(input$select_bivariate_y1))+
        # scale_x_continuous("HUC 2 Zone ID", labels = as.character(myData$HU2), breaks = myData$HU2)+ 
        theme_classic()+
        theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, linetype="solid"))+
        theme(text=element_text(size=15,face = "bold", color="blue"))
      
      p2<-ggplot(myData, aes_string(x=isolate(input$select_bivariate_x2), y=isolate(input$select_bivariate_y2))) + 
        geom_point()+
        geom_smooth(formula = y~x, method=lm)+
        ylab(isolate(input$select_bivariate_y2))+
        # scale_x_continuous("HUC 2 Zone ID", labels = as.character(myData$HU2), breaks = myData$HU2)+
        theme_classic()+
        theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5,linetype="solid"))+
        theme(text=element_text(size=15,face = "bold", color="blue"))
      
      grid.arrange(p1, p2, ncol=2)
    }) # renderPlot end
    
  }) #observeEvent end
  
  ## for Tab 4: Correlations, sub-tab 2: multivariate
  observeEvent(input$display_multivariate_plots, {
    
    start_time <- Sys.time()
    output$display_plot6 <- renderUI({
      withSpinner(plotOutput("plot6"),type=2)
    })
    
    output$display_plot7 <- renderUI({
      withSpinner(plotOutput("plot7"),type=2)
    })
    
    output$display_footnote_plot6 <- renderUI({
      div(style="overflow-x:hidden",verbatimTextOutput("plot6_correlation_footnote"))
    })
    
    output$display_save_multivariate_button <-  renderUI({
      div(br(),
          downloadButton(outputId="saveMultivariatePlot", label="Save plot",style="color:blue;background-color:black")
      )
    })
    
    myData <- loaded_data()
    selected_data <- myData %>% select(c(input$select_multivariate_end,input$select_multivariate_predictor))
    #print(colnames(selected_data))
    ## check if there is any predictor variable of the entire column is NA
    ## if so, remove those columns
    need_to_remove_names = colnames(selected_data[,colSums(is.na(selected_data))==nrow(selected_data)])
    selected_data <- selected_data[,colSums(is.na(selected_data))!=nrow(selected_data)]
    selected_data <- na.roughfix(selected_data)
    
    output$plot6 <- renderPlot({
      ## if we don't load "GGally" package, we can use the following code to create the multivariate correlation plot 
      ########### use "ggplot2" and "reshape2" packages, code start ####################
      # cormat <- round(cor(selected_data),2)
      # upper_cormat <- cormat
      # upper_cormat[upper.tri(cormat)] <- NA
      # melted_cormat <- reshape2::melt(upper_cormat,na.rm=TRUE)
      # 
      # p1 <- ggplot(data=melted_cormat, aes(Var1,Var2)) + 
      #   geom_tile(aes(fill=value),color="white")+
      #   scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,limit=c(-1,1),space="Lab",name="Pearson\nCorrelation")+
      #   theme_minimal()+
      #   theme(axis.text.x = element_text(angle=45,vjust=1,size=12,hjust=1),
      #         axis.title.x = element_blank(),
      #         axis.title.y = element_blank(),
      #         panel.grid.major = element_blank(),
      #         axis.ticks = element_blank())+
      #   coord_fixed()
      ########### use "ggplot2" and "reshape2" packages, code end ######################
      
      ## if we use "GGally" package, use the following code
      p1 <- ggcorr(selected_data,method=c("complete","pearson"),label=TRUE,label_round=3)
      to_download$corr_plot6 = p1
      print(p1)
    })
    
    output$plot7 <- renderPlot({
      glm_mapping <- function(data,mapping,method="glm",...){
        p <- ggplot(data=data,mapping = mapping)+
          geom_point()+
          geom_smooth(formula = y~x,method=method,...)
        p
      }
      p2 <- ggpairs(selected_data,lower=list(continuous=glm_mapping))
      to_download$corr_plot7 = p2
      print(p2)
    }) #renderPlot end
    
    
    output$plot6_correlation_footnote <- renderText({
      
      Note_text_line1= paste0("The correlation coefficients displayed in this page are computed by Pearson's method")
      
    })
    
    end_time <- Sys.time()
    
    print(paste0("run time for the multivariate plot is:",end_time-start_time))
    
  }) #observeEvent end
  
  output$saveMultivariatePlot <- downloadHandler(
    
    filename = function(){paste("multivariate_correlations_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$corr_plot6,dpi=300,width=12,height=10)
    }
  )
  
  ######### the following code is to render a "ggplot" static map on HUC2 and HUC8 #######################
  
  observe({
    if(req(input$tabset) == "tab5") {
      print("inside maps tabpanel now...") 
      
      output$display_explore_map_1 <- renderUI({
        withSpinner(plotOutput("map_on_HUC2",width=800, height=500),type=2)
      })
      
      output$end_points_option_in_maps <- renderUI({
        div(radioButtons("end_points_in_maps",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
            style= "margin-left:20px;font-style:bold")
      })
      
      output$display_explore_map_2 <- renderUI({
        withSpinner(plotOutput("map_on_HUC8",width=800, height=500),type=2)
      })
      
      output$map_on_HUC2 <- renderPlot({
        print("inside rendering the summary HUC2 map now...")
        
        myData <- loaded_data()
        req(input$end_points_in_maps)
        if(input$end_points_in_maps=="LogTP"){
          name_in_legend = "log(TP,\u03bcg/L)" 
          mySummary <- myData %>% select(HU2,input$end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(LogTP = mean(LogTP))
        }else if(input$end_points_in_maps=="LogTN"){
          name_in_legend = "log(TN,\u03bcg/L)"  
          mySummary <- myData %>% select(HU2,input$end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(LogTN = mean(LogTN)) 
        }else if(input$end_points_in_maps=="TN"){
          name_in_legend = paste0(input$end_points_in_maps,"(\u03bcg/L)")
          mySummary <- myData %>% select(HU2,input$end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(TN = mean(TN)) 
        }else if(input$end_points_in_maps=="TP"){
          name_in_legend = paste0(input$end_points_in_maps,"(\u03bcg/L)")
          mySummary <- myData %>% select(HU2,input$end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(TP = mean(TP)) 
        }
        mySummary$huc2 = formatC(as.numeric(as.character(mySummary$HU2)), width = 2, format = "d",flag = "0") # add zero in front of any
        NE_to_show <- states_in_US[states_in_US$name %in% NE_states_all,]
        myjoinedSummary = merge(all_joined_HU2,mySummary,by="huc2",duplicateGeoms=TRUE)
        myjoinedSummaryToMap = st_as_sf(myjoinedSummary)
        rm(mySummary,myjoinedSummary)
        
        myMap <- ggplot()+
          ggtitle(paste0(input$end_points_in_maps," Summarized by HUC2"))+
          geom_sf(data=NE_to_show,color="black",fill=NA)+
          geom_sf(myjoinedSummaryToMap,mapping=aes_string(fill=input$end_points_in_maps),colour=NA,alpha=0.8)+
          scale_fill_viridis(option="viridis",name=name_in_legend)+
          theme(panel.background = element_blank())+ # remove grey background
          theme(plot.title=element_text(hjust=0.5))+ # put the title at center
          theme(axis.text = element_blank())+ # remove axis text
          theme(axis.ticks = element_blank()) # remove axis text
        
        print(myMap)
      })
      
      output$map_on_HUC8 <- renderPlot({
        print("inside rendering the summary HUC8 map now...")
        myData <- loaded_data()
        req(input$end_points_in_maps)
        load("./Data/all_HU8_shapes.RData")
        all_joined_HU8 <- st_as_sf(joined_HU8)
        if(input$end_points_in_maps=="LogTP"){
          name_in_legend = "log(TP,\u03bcg/L)" 
          mySummary <- myData %>% select(HU8,input$end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(LogTP = mean(LogTP))
        }else if(input$end_points_in_maps=="LogTN"){
          name_in_legend = "log(TN,\u03bcg/L)"
          mySummary <- myData %>% select(HU8,input$end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(LogTN = mean(LogTN))
        }else if(input$end_points_in_maps=="TN"){
          name_in_legend = paste0(input$end_points_in_maps,"(\u03bcg/L)")
          mySummary <- myData %>% select(HU8,input$end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(TN = mean(TN))
        }else if(input$end_points_in_maps=="TP"){
          name_in_legend = paste0(input$end_points_in_maps,"(\u03bcg/L)")
          mySummary <- myData %>% select(HU8,input$end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(TP = mean(TP))
        }
        mySummary$huc8 = formatC(as.numeric(as.character(mySummary$HU8)), width = 8, format = "d",flag = "0") # add zero in front of any
        myjoinedSummary = merge(all_joined_HU8,mySummary,by="huc8",duplicateGeoms=TRUE)
        myjoinedSummaryToMap = st_as_sf(myjoinedSummary)
        rm(joined_HU8,all_joined_HU8,mySummary,myjoinedSummary)
        myMap <- ggplot()+
          ggtitle(paste0(input$end_points_in_maps," Summarized by HUC8"))+
          geom_sf(data=NE,color="black",fill=NA)+
          geom_sf(myjoinedSummaryToMap,mapping=aes_string(fill=input$end_points_in_maps),colour=NA,alpha=0.8)+
          scale_fill_viridis(option="viridis",name=name_in_legend)+
          theme(panel.background = element_blank())+ # remove grey background
          theme(plot.title=element_text(hjust=0.5))+ # put the title at center
          theme(axis.text = element_blank())+ # remove axis text
          theme(axis.ticks = element_blank()) # remove axis text
        print(myMap)
      })
    } #if loop end
    
  }) #observe end
  
  output$plot8 <- renderPlot({
    myData <- loaded_data()
    req(input$end_points_in_Misc)
    myData_1 <- myData %>% select(HU2,programtype,input$end_points_in_Misc) %>% na.omit()
    res <- myData_1 %>% group_by(HU2,programtype) %>% summarise(Frequency=n())
    p <- ggplot(res, aes(x = HU2, y = Frequency))+
      geom_col(aes(fill = programtype), width = 0.7)+
      scale_x_discrete( labels = as.character(res$HU2), breaks = res$HU2)+
      xlab("HUC2 Zone ID")+
      ylab(paste0("Number of valid ",input$end_points_in_Misc," data records"))+
      theme_classic()+
      theme(text=element_text(size=15,face = "bold", color="blue"))
    p
  })
  
  #######################################################################################################################
  ######################################## server code start for tabPanel "Create a subset"##############################
  #######################################################################################################################
  
  ## helper functions defined for this section
  ## this function is written to add state name to a data point with known long and lat
  lonlat_to_state <- function(pointsDF,states=us_states(),name_col="state_name"){
    pts <- st_as_sf(pointsDF,coords = 1:2,crs = 4326)
    states <- st_transform(states,crs = 3857)
    pts <- st_transform(pts,crs = 3857)
    state_names <- states.data[[name_col]]
    ii <- as.integer(st_intersects(pts,states))
    state_names[ii]
  }
  
  base_map_state <- reactive({
    data <- loaded_data()
    data_to_map <- data %>% select(Lat,Long)
    data_to_map <- data_to_map[!is.na(as.numeric(as.character(data_to_map$Lat)))&!is.na(as.numeric(as.character(data_to_map$Long))),]
    unique_lakes<- unique(data_to_map)
    print(paste0("the number of lakes is:",length(unique_lakes)))
    rm(data,data_to_map)
    
    leaflet() %>% addTiles() %>% addResetMapButton() %>%
      setView(lng=-94.8,lat=40,zoom=4) %>%
      addCircleMarkers(lat=unique_lakes$Lat,lng=unique_lakes$Long,radius=0.5,group="lakes") %>%
      addPolygons(data =states_in_US_sp,
                  fillColor = "green",  
                  fillOpacity = 0.5,
                  color="grey",
                  weight=2,
                  layerId = states_in_US_sp$name,
                  group="click_States_list") %>%
      addLegend(group="lakes",position="bottomright",labels="lakes",color="blue") %>%
      addLegend(group="click_States_list",position="bottomright",labels="States(clickable)",color="green")
    
  })
  
  base_map_HUC2 <- reactive({
    data <- loaded_data()
    data_to_map <- data %>% select(Lat,Long)
    data_to_map <- data_to_map[!is.na(as.numeric(as.character(data_to_map$Lat)))&!is.na(as.numeric(as.character(data_to_map$Long))),]
    unique_lakes<- unique(data_to_map)
    print(paste0("the number of lakes is:",length(unique_lakes)))
    rm(data,data_to_map)
    all_joined_HU2_sp = sf::as_Spatial(all_joined_HU2)
    myMap <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
      setView(lng=-94.8,lat=40,zoom=4) %>%
      addCircleMarkers(lat=unique_lakes$Lat,lng=unique_lakes$Long,radius=0.5,group="lakes")
    
    myMap <- myMap %>%
      addPolygons(data=all_joined_HU2_sp,
                  fillColor = "pink", 
                  fillOpacity = 0.5,
                  color="red",
                  smoothFactor = 0.2,
                  weight=2,
                  label=~huc2,
                  layerId =~huc2,
                  labelOptions = labelOptions(noHide=TRUE,textOnly =TRUE,opacity=0.5,textsize='18px',style =list("font-weight"="bold",padding="3px 8px") ),
                  group="click_HUC2_list") %>%
      addLegend(group="lakes",position="bottomright",labels="lakes",color="blue") %>%
      addLegend(group="click_HUC2_list",position="bottomright",labels="HUC2(clickable)",color="red")
    
  })
  
  observeEvent(input$iws_ha_range,{
    newvalue = input$iws_ha_range
    if (newvalue[2]<5000){
      updateSliderInput(session,"iws_ha_range",value=newvalue,min=0,max=6000,step=100)
    }else{
      sliders_value_list = data_to_model$sliders_value_list
      updateSliderInput(session,"iws_ha_range",min=sliders_value_list$iws_ha[1],max=sliders_value_list$iws_ha[2])   
    }
  })
  
  observeEvent(input$lake_area_range,{
    newvalue = input$lake_area_range
    if (newvalue[2]<2000){
      updateSliderInput(session,"lake_area_range",value=newvalue,min=0,max=2100,step=10)
    }else{
      sliders_value_list = data_to_model$sliders_value_list
      updateSliderInput(session,"lake_area_range",min=sliders_value_list$lake_area_ha[1],max=sliders_value_list$lake_area_ha[2])  
    }
  })
  
  # observeEvent(input$lake_perimeter_range,{
  #   newvalue = input$lake_perimeter_range
  #   if (newvalue[2]<5000){
  #     updateSliderInput(session,"lake_perimeter_range",value=newvalue,min=0,max=6000,step=100)
  #   }else{
  #     sliders_value_list = data_to_model$sliders_value_list
  #     updateSliderInput(session,"lake_perimeter_range",min=sliders_value_list$lake_perim_meters[1],max=sliders_value_list$lake_perim_meters[2])   
  #   }
  # })
  
  observeEvent(input$subsetSpatial, {
    
    # print("After rendering the summary table: ")
    # print(pryr::mem_used())
    
    
    
    output$ChooseStateButton <- renderUI({
      actionButton(inputId="ChooseState", label="Choose states",style="color:black;background-color:grey")
    })
    
    output$ChooseHUC2Button <- renderUI({
      actionButton(inputId="ChooseHUC2", label="Choose HUC2",style="color:black;background-color:grey")
    })
    
    
  })
  
  
  observeEvent(input$ChooseState,{
    
    shinyjs::hide("choose_HUC2_panel")
    shinyjs::show("choose_state_panel")
    
    click_HUC2_list$ids <- NULL
    click_HUC2_list$huc2_names <- NULL
    print("inside display clickable map now...")
    
    output$display_clickable_map <- renderUI({
      withSpinner(leafletOutput(outputId = "myClickableMap",width=800, height=600),type=2)
    })
    
    
    print(class(base_map_state()))
    
    output$myClickableMap <- renderLeaflet({
      
      base_map_state()
      
    })
    
    output$State_list_text_title <- renderText({
      "You selected these states: \n"
    })
    
    output$ClearStateButton <- renderUI({
      actionButton(inputId="ClearState", label="Clear all selected states",style="color:black;background-color:grey")
    })
    
  })
  
  
  observeEvent(input$myClickableMap_shape_click,{
    print("inside shape click now")
    click <- input$myClickableMap_shape_click
    print(click$id)
    click_States_list$ids <- c(click_States_list$ids,click$id)
    print(click_States_list$ids)
    lines_of_interest <- states_in_US_sp[which(states_in_US_sp$name %in% click_States_list$ids),]
    # print(lines_of_interest@data$id)
    # print(lines_of_interest$state_name)
    if (is.null(click$id)){
      req(click$id)
    }else if (!click$id %in% lines_of_interest@data$id){
      leafletProxy(mapId = "myClickableMap") %>%
        addPolylines(data=lines_of_interest,
                     layerId = lines_of_interest@data$id,
                     color ="#FFDF00",
                     weight =3,
                     opacity=1)
    } # end of if else statement
    
    output$State_list_text <- renderText({
      print(unlist(click_States_list$ids))
      paste0(unlist(click_States_list$ids),sep="\n")
    })
    
  })  # end of observeEvent
  
  
  
  observeEvent(input$ClearState,{
    
    output$myClickableMap <- renderLeaflet({
      
      click_States_list$ids <- NULL
      base_map_state()
      
    })  # end of re-rendering myClickableMap
    
  }) # end of ClearState observe Event
  
  
  
  observeEvent(input$ChooseHUC2,{
    
    shinyjs::show("choose_HUC2_panel")
    shinyjs::hide("choose_state_panel")
    click_States_list$ids <- NULL
    
    print("inside display clickable HUC2 map now...")
    
    output$display_clickable_HUC2_map <- renderUI({
      withSpinner(leafletOutput(outputId = "myClickableHUC2Map",width=800, height=600),type=2)
    })
    
    #print(class(base_map_HUC2()))
    
    output$myClickableHUC2Map <- renderLeaflet({
      
      base_map_HUC2()
      
    })
    
    output$HUC2_list_text_title <- renderText({
      "You selected these HUC2s: \n"
    })
    
    output$ClearHUC2Button <- renderUI({
      actionButton(inputId="ClearHUC2", label="Clear all selected HUC2s",style="color:black;background-color:grey")
    })
    
  })
  
  
  observeEvent(input$myClickableHUC2Map_shape_click,{
    print("inside HUC2 shape click now")
    all_joined_HU2_sp = sf::as_Spatial(all_joined_HU2)
    click <- input$myClickableHUC2Map_shape_click
    #print(click$id)
    click_HUC2_list$ids <- c(click_HUC2_list$ids,click$id)
    #print(click_HUC2_list$ids) 
    
    lines_of_interest <- all_joined_HU2_sp[which(all_joined_HU2_sp$huc2 %in% click_HUC2_list$ids),]
    #print(lines_of_interest@data$name)
    click_HUC2_list$ids <- lines_of_interest@data$huc2
    click_HUC2_list$huc2_names <- lines_of_interest@data$name
    if (is.null(click$id)){
      req(click$id)
    }else if (click$id %in% lines_of_interest@data$huc2){
      leafletProxy(mapId = "myClickableHUC2Map") %>%
        addPolylines(data=lines_of_interest,
                     layerId = lines_of_interest@data$huc2,
                     color ="#FFDF00",
                     weight =3,
                     opacity=1)
    } # end of if else statement
    
    
    output$HUC2_list_text <- renderText({
      if(!is.null(click_HUC2_list)){
        paste0(unlist(click_HUC2_list$ids),"(",unlist(click_HUC2_list$huc2_names),")",sep="\n")
      }
    })
    
  })  # end of observeEvent
  
  
  observeEvent(input$ClearHUC2,{
    
    output$myClickableHUC2Map <- renderLeaflet({
      
      click_HUC2_list$ids <- NULL
      click_HUC2_list$huc2_names <- NULL
      base_map_HUC2()
      
    })  # end of re-rendering myClickableMap
    
  }) # end of ClearState observe Event
  
  
  observeEvent(input$saveFile,{
    print("inside subseting by sliderInput now")
    
    ## check and gather the input from each slider
    elevation_range_slider_values = input$elevation_range
    #print(paste0("the selected elevation range is: ",elevation_range_slider_values[1], " and ",elevation_range_slider_values[2]))
    lake_area_range_slider_values = input$lake_area_range
    #print(paste0("the selected lake area range is: ",lake_area_range_slider_values[1], " and ",lake_area_range_slider_values[2]))
    #lake_perimeter_range_slider_values = input$lake_perimeter_range
    #print(paste0("the selected lake perimeter range is: ",lake_perimeter_range_slider_values[1], " and ",lake_perimeter_range_slider_values[2]))
    #lake_meanDepth_range_slider_values = input$lake_meanDepth_range  ##YD commented this line out
    #print(paste0("the selected lake meanDepth range is: ",lake_meanDepth_range_slider_values[1], " and ",lake_meanDepth_range_slider_values[2]))
    lake_maxDepth_range_slider_values = input$lake_maxDepth_range
    #print(paste0("the selected lake maxDepth range is: ",lake_maxDepth_range_slider_values[1], " and ",lake_maxDepth_range_slider_values[2]))
    iws_ha_range_slider_values = input$iws_ha_range
    #print(paste0("the selected iws area range is: ",iws_ha_range_slider_values[1], " and ",iws_ha_range_slider_values[2]))
    #iws_perimkm_range_slider_values = input$iws_perimkm_range
    #print(paste0("the selected iws perimeter range is: ",iws_perimkm_range_slider_values[1], " and ",iws_perimkm_range_slider_values[2]))
    year_range_slider_values = input$year_range
    #print(paste0("the selected year range is: ",year_range_slider_values[1], " and ",year_range_slider_values[2]))
    month_range_slider_values = input$month_range
    tn_range_slider_values = input$tn_range
    LogTN_range_slider_values = input$LogTN_range
    tp_range_slider_values = input$tp_range
    LogTP_range_slider_values = input$LogTP_range
    
    data <- loaded_data()
    ##YD added to check what variables are available for sliders
    all_slider_variables <- c("Year","Month","iws_ha","iws_perimkm","Elevation","lake_area_ha","lake_perim_meters","MeanDepth","MaxDepth",
                              "TN","LogTN","TP","LogTP")
    
    
    data_selected_after_sliders <- data %>%
<<<<<<< HEAD
      subset((Year>=year_range_slider_values[1]&Year<=year_range_slider_values[2])|Year=="NA") %>%
      subset(Month>=month_range_slider_values[1]&Month<=month_range_slider_values[2]) %>%
      subset(iws_ha>=iws_ha_range_slider_values[1]&iws_ha<=iws_ha_range_slider_values[2]) %>%
      #subset(iws_perimkm>=iws_perimkm_range_slider_values[1]&iws_perimkm<=iws_perimkm_range_slider_values[2]) %>%
      subset(Elevation>=elevation_range_slider_values[1]&Elevation<=elevation_range_slider_values[2]) %>%
      subset(lake_area_ha>=lake_area_range_slider_values[1]&lake_area_ha<=lake_area_range_slider_values[2]) %>%
      #subset(lake_perim_meters>=lake_perimeter_range_slider_values[1]&lake_perim_meters<=lake_perimeter_range_slider_values[2]) %>%
      #subset((MeanDepth>=lake_meanDepth_range_slider_values[1]&MeanDepth<=lake_meanDepth_range_slider_values[2])|is.na(MeanDepth)) %>%
      subset((MaxDepth>=lake_maxDepth_range_slider_values[1]&MaxDepth<=lake_maxDepth_range_slider_values[2])|is.na(MaxDepth)) %>%
      subset((TN>=tn_range_slider_values[1]&TN<=tn_range_slider_values[2])|is.na(TN)) %>%
      subset((LogTN>=LogTN_range_slider_values[1]&LogTN<=LogTN_range_slider_values[2])|is.na(LogTN)) %>%
      subset((TP>=tp_range_slider_values[1]&TP<=tp_range_slider_values[2])|is.na(TP)) %>%
      subset((LogTP>=LogTP_range_slider_values[1]&LogTP<=LogTP_range_slider_values[2])|is.na(LogTP))
=======
                                 subset((Year>=year_range_slider_values[1]&Year<=year_range_slider_values[2])|Year=="NA") %>%
                                 subset(Month>=month_range_slider_values[1]&Month<=month_range_slider_values[2]) %>%
                                 subset(iws_ha>=iws_ha_range_slider_values[1]&iws_ha<=iws_ha_range_slider_values[2]) %>%
                                 #subset(iws_perimkm>=iws_perimkm_range_slider_values[1]&iws_perimkm<=iws_perimkm_range_slider_values[2]) %>%
                                 subset(Elevation>=elevation_range_slider_values[1]&Elevation<=elevation_range_slider_values[2]) %>%
                                 subset(lake_area_ha>=lake_area_range_slider_values[1]&lake_area_ha<=lake_area_range_slider_values[2]) %>%
                                 #subset(lake_perim_meters>=lake_perimeter_range_slider_values[1]&lake_perim_meters<=lake_perimeter_range_slider_values[2]) %>%
                                 #subset((MeanDepth>=lake_meanDepth_range_slider_values[1]&MeanDepth<=lake_meanDepth_range_slider_values[2])|is.na(MeanDepth)) %>%
                                 subset((MaxDepth>=lake_maxDepth_range_slider_values[1]&MaxDepth<=lake_maxDepth_range_slider_values[2])|is.na(MaxDepth)) %>%
                                 subset((TN>=tn_range_slider_values[1]&TN<=tn_range_slider_values[2])|is.na(TN)) %>%
                                 subset((LogTN>=LogTN_range_slider_values[1]&LogTN<=LogTN_range_slider_values[2])|is.na(LogTN)) %>%
                                 subset((TP>=tp_range_slider_values[1]&TP<=tp_range_slider_values[2])|is.na(TP)) %>%
                                 subset((LogTP>=LogTP_range_slider_values[1]&LogTP<=LogTP_range_slider_values[2])|is.na(LogTP))
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
    
    print(paste0("the number of rows after all the sliders is:",nrow(data_selected_after_sliders)))
    
    if(length(input$selected_program_name)>0){
      data_selected_after_sliders <- data_selected_after_sliders %>%
        subset(programname %in% input$selected_program_name)
      print(nrow(data_selected_after_sliders))
    }
    
    if(length(input$selected_program_type)>0){
      data_selected_after_sliders <- data_selected_after_sliders %>%
        subset(programtype %in% input$selected_program_type)
      print(nrow(data_selected_after_sliders))
    }
    
    if(length(input$selected_HUC2)>0){
      data_selected_after_sliders <- data_selected_after_sliders %>%
        subset(HU2 %in% input$selected_HUC2)
      print(nrow(data_selected_after_sliders))
    }
    
    ## also need to check if user clicked any state from the clickable map
    
    if(length(click_States_list$ids)>0){
      data_selected_after_sliders$State_name <- lonlat_to_state(data.frame(x=data_selected_after_sliders$Long,y=data_selected_after_sliders$Lat))
      data_selected_after_sliders <- data_selected_after_sliders %>%
        subset(State_name %in% click_States_list$ids)
      print(nrow(data_selected_after_sliders))
      print(paste0("checking the clicked state list now:",unlist(click_States_list$ids)))
      
    }
    
    ## also need to check if user clicked any HUC2 zone from the clickable map
    
    if(length(click_HUC2_list$ids)>0){
      data_selected_after_sliders <- data_selected_after_sliders %>%
        subset(HU2 %in% as.integer(unlist(click_HUC2_list$ids)))
      print(nrow(data_selected_after_sliders))
      print(paste0("checking the clicked HUC2 list now:",unlist(click_HUC2_list$ids)))
      
    }
    
    
    data_to_model$data_subset <- data_selected_after_sliders
    
    print(length(data_to_model$data_subset))
    print(paste0("there are ", nrow(data_to_model$data_subset)," data records left in this subset"))
    number_of_samples = nrow(data_to_model$data_subset)
    alert_message_25 = paste0("Currently, there are ", nrow(data_to_model$data_subset)," data records left in this subset.",
                              "We recommend that you use at least 500 data records to fit models.", "Please go back to expand your subset selection.")
    alert_message_499 = paste0("Currently, there are ", nrow(data_to_model$data_subset)," data records left in this subset.",
                               "We recommend that you use at least 500 data records to fit models.")
    alert_message_500 = paste0("Subset with ",nrow(data_to_model$data_subset)," data records is created and saved, please continue.")
    
    if (number_of_samples<25) {
      shinyalert("Alert",alert_message_25,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert1")
      # shinyjs::delay(5000, shinyjs::runjs("swal.close();"))
    }else if (number_of_samples>=25&number_of_samples<=499){
      shinyalert("Alert",alert_message_499,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert2")
    }else{
      shinyalert(" ",alert_message_500,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info1")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
    }
    
    rm(data_selected_after_sliders)
    
  })
  
  observeEvent(input$alert1,{
    #print(input$alert1)
    shinyjs::runjs("swal.close();")
  })
  
  observeEvent(input$alert2,{
    #print(input$alert2)
    shinyjs::runjs("swal.close();")
  })
  
  shinyjs::hide("model_running_panel")
  
  #######################################################################################################################
  ######################################## server code end for tabPanel "Create a subset"################################
  #######################################################################################################################
  
  
  
  #####################################################################################################################
  ######################################## server code start for tabPanel "Explore Subset"#############################
  #####################################################################################################################
  
  
  observeEvent(input[["subset_tabset"]], {
    
    
    output$subset_display_plot1 <- renderUI({
      withSpinner(plotOutput("subset_plot1"),type=2)
    })
    
    output$subset_end_points_option_in_summary <- renderUI({
      div(radioButtons("subset_end_points_in_summary",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=FALSE),
          style= "margin-left:20px;font-style:bold")
    })
    
    output$subset_display_plot2a <- renderUI({
      withSpinner(plotlyOutput("subset_plot2a"),type=2)
    })
    
    output$subset_display_plot2b <- renderUI({
      withSpinner(plotlyOutput("subset_plot2b"),type=2)
    })
    
    output$subset_end_points_option_in_series <- renderUI({
      div(radioButtons("subset_end_points_in_series",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:50px;font-style:bold")
    })
    
    output$subset_display_plot3 <- renderUI({
      withSpinner(plotOutput("subset_plot3"),type=2)
    })
    
    output$subset_end_points_option_in_HUC2 <- renderUI({
      div(radioButtons("subset_end_points_in_HUC2",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:-200px;font-style:bold")
    })
    
    output$subset_display_plot4 <- renderUI({
      withSpinner(plotOutput("subset_plot4"),type=2)
    })
    
    output$subset_end_points_option_in_HUC2_box <- renderUI({
      div(radioButtons("subset_end_points_in_HUC2_box",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:50px;font-style:bold")
    })
    
    ## this part is added to provide a tab for subset predictor variables HUC2 summary
    
    
    output$subset_select_variable <- renderUI({
      selectizeInput("subset_select_variable_to_summary",label ="Select one predictor variable",
                     choices=data_to_model$default_variable_names,
                     multiple = FALSE,
                     selected="P_Anthro_Inputs.1",
                     options = list(hideSelected = FALSE))
    })
    
    output$subset_display_Y_axis_slider <- renderUI({
      sliderInput("subset_Y_axis_scale","Adjust Y axis scale",min=0.05,max=1,value=1,step=0.05,sep="")
    })
    
    
    output$subset_display_plot8 <- renderUI({
      withSpinner(plotOutput("subset_plot8"),type=2)
    })
    
    output$subset_end_points_option_in_Misc <- renderUI({
      div(radioButtons("subset_end_points_in_Misc",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:50px;font-style:bold")
    })
    
    output$subset_display_explore_map_1 <- renderUI({
      withSpinner(plotOutput("subset_map_on_HUC2",width=800, height=500),type=2)
    })
    
    output$subset_end_points_option_in_maps <- renderUI({
      div(radioButtons("subset_end_points_in_maps",label="Endpoint options",choices=c("LogTN","LogTP","TN","TP"),selected="LogTP",inline=TRUE),
          style= "margin-right:20px;font-style:bold")
    })
    
    output$subset_display_explore_map_2 <- renderUI({
      withSpinner(plotOutput("subset_map_on_HUC8",width=800, height=500),type=2)
    })
    
    
    
    output$subset_session1_title <- renderText({
      HTML("For plot on the left")
    })
    
    output$subset_select_x1 <- renderUI({
      selectizeInput("subset_select_bivariate_x1",label ="select one predictor variable(X1)",
                     choices=data_to_model$default_variable_names,
                     multiple = FALSE,
                     selected="Vegetation",
                     options = list(hideSelected = FALSE))
    })
    
    output$subset_select_y1 <- renderUI({
      selectizeInput("subset_select_bivariate_y1",label ="select one endpoint(Y1)",
                     choices=c("TN","TP","LogTN","LogTP"),
                     multiple = FALSE,
                     selected="LogTP",
                     options = list(hideSelected = FALSE))
    })
    
    output$subset_session2_title <- renderText({
      HTML("For plot on the right")
    })
    
    output$subset_select_x2 <- renderUI({
      selectizeInput("subset_select_bivariate_x2",label ="select one predictor variable(X2)",
                     choices=data_to_model$default_variable_names,
                     multiple = FALSE,
                     selected="Tmax",
                     options = list(hideSelected = FALSE))
    })
    
    output$subset_select_y2 <- renderUI({
      selectizeInput("subset_select_bivariate_y2",label ="select one predictor variable(Y2)",
                     choices=data_to_model$default_variable_names,
                     multiple = FALSE,
                     selected="Tmin",
                     options = list(hideSelected = FALSE))
    })
    
    
    output$subset_select_corr_end_points <- renderUI({
      selectizeInput("subset_select_multivariate_end",label ="select endpoints",
                     choices=c("TN","TP","LogTN","LogTP"),
                     multiple = FALSE,
                     selected="LogTP",
                     options = list(hideSelected = FALSE))
    })
    
    output$subset_select_corr_predictors <- renderUI({
      selectizeInput("subset_select_multivariate_predictor",label ="select predictor variables",
                     choices=data_to_model$default_variable_names,
                     multiple = TRUE,
                     selected=c("Precip","MaxDepth","NLCD_pct_forest","NLCD_pct_farm"),
                     options = list(hideSelected = FALSE,plugins=list('remove_button'),maxItems=5))
    })
    
    output$subset_reminder_note <- renderText({
      "Please limit the number of selected predictor variables no more than 5. \n\n"
    })
    
  }) #observe event end
  
  observe({
    if(!is.null(input$subset_select_bivariate_x2)){
      updateSelectInput(session,"subset_select_bivariate_y2",choices=data_to_model$default_variable_names[!(data_to_model$default_variable_names %in% input$subset_select_bivariate_x2 )],selected=isolate(input$subset_select_bivariate_y2))
    }
  })
  
  observe({
    if(!is.null(input$subset_select_bivariate_y2)){
      updateSelectInput(session,"subset_select_bivariate_x2",choices=data_to_model$default_variable_names[!(data_to_model$default_variable_names %in% input$subset_select_bivariate_y2 )],selected=isolate(input$subset_select_bivariate_x2))
    }
  })
  
  ## for Tab 1:Summary Info
  
  output$subset_plot1 <- renderPlot({
    
    print("inside rendering histogram plot now...")
    
    myData <- data_to_model$data_subset
    print(paste0("check HU2 type:",class(myData$HU2)))
    req(input$subset_end_points_in_summary)
    
    if (length(myData)==0){
      
      alert_message ="No subet was created, please go back to the previous tab to create your subset."
      shinyalert(" ",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info4")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
    }else{
      myCol <- myData %>% select(input$subset_end_points_in_summary) %>% na.omit() %>% summarise_all(list(mean=mean,median=median,min=min,max=max,var=var,sd=sd))
      Stats <- c("Mean","Median","Min","Max","Variance","SD")
      Value <- format(round(t(myCol),2),nsmall =2)
      Summ<-data.frame(Stats,Value)
      #print(Summ)
      
      p1 <- ggplot(myData, aes_string(x=input$subset_end_points_in_summary)) + geom_histogram()+
        ylab("Frequency")+
        xlab(input$subset_end_points_in_summary)+
        theme_classic()+ theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, 
                                                           linetype="solid"))+
        theme(text=element_text(size=15,face = "bold", color="blue"))
      
      mytheme <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 1.0),padding=unit(c(12,12),'mm')),
        colhead = list(fg_params=list(cex = 1.5),padding=unit(c(8,8),'mm')),
        rowhead = list(fg_params=list(cex = 1.5)))
      
      tbl <- tableGrob(Summ,rows=NULL,theme = mytheme)
      grid.arrange(p1, tbl, ncol=2)
    } # if else loop end
  })
  
  ## for Tab 2:Time Series
  
  output$subset_plot2a <- renderPlotly({
    
    myData <- data_to_model$data_subset
    year_range <- max(myData$Year)-min(myData$Year)
    print(paste0("the year range is:",year_range))
    #meandf<-meandf[!duplicated(meandf[,c('programtype','Year')]),]
    if (year_range<5){
      break_month_num = 2
      x_lab_str="Year-Month"
      x_date_label="%Y-%m"
    }else if(year_range>=5&year_range<10){
      break_month_num = 6
      x_lab_str="Year-Month"
      x_date_label="%Y-%m"
    }else if(year_range>=10){
      break_month_num = 12
      x_lab_str="Year"
      x_date_label="%Y"
    }
    print(paste0("the x lab is:",x_lab_str))
    myBreaks = paste0(break_month_num," months")
    print(myBreaks)
    req(input$subset_end_points_in_series)
    if (length(myData)==0){
      
      alert_message ="No subet was created, please go back to the previous tab to create your subset."
      shinyalert(" ",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info4")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
    }else{
      if (input$subset_end_points_in_series=="LogTP"){
        myData_no_NA <- myData[,c('programtype','Year','Month','LogTP')] %>% na.omit()
        mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("LogTP",mean)
        meandf<-mean[complete.cases(mean), ]
        p1<-ggplot(data =meandf) +
          geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTP, color = programtype ))+
          geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTP, color = programtype ))+
          labs(x = x_lab_str,y = paste0("ln","(TP, \u03bcg/L)"))
      }else if(input$subset_end_points_in_series=="TP"){
        myData_no_NA <- myData[,c('programtype','Year','Month','TP')] %>% na.omit()
        mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("TP",mean)
        meandf<-mean[complete.cases(mean), ]
        p1<-ggplot(data =meandf) +
          geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TP, color = programtype ))+
          geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TP, color = programtype ))+
          labs(x = x_lab_str,y = paste0(input$subset_end_points_in_series," (\u03bcg/L)"))
      }else if(input$subset_end_points_in_series=="LogTN"){
        myData_no_NA <- myData[,c('programtype','Year','Month','LogTN')] %>% na.omit() 
        mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("LogTN",mean) 
        meandf<-mean[complete.cases(mean), ]
        p1<-ggplot(data =meandf) +
          geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTN, color = programtype ))+
          geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = LogTN, color = programtype ))+
          labs(x = x_lab_str,y = paste0("ln","(TN, \u03bcg/L)"))
      }else if(input$subset_end_points_in_series=="TN"){  
        myData_no_NA <- myData[,c('programtype','Year','Month','TN')] %>% na.omit()
        mean<-myData_no_NA %>% group_by(Year,Month,programtype) %>% summarise_at("TN",mean) 
        meandf<-mean[complete.cases(mean), ]
        p1<-ggplot(data =meandf) +
          geom_point(mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TN, color = programtype ))+
          geom_line (mapping = aes(x = as.Date(paste(Year,Month,1,sep="-"),format = "%Y-%m-%d"), y = TN, color = programtype ))+
          labs(x = x_lab_str,y = paste0(input$subset_end_points_in_series," (\u03bcg/L)"))
      }
      p1 <- p1 +  
        scale_x_date(date_labels=x_date_label,date_breaks  =myBreaks)+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))
      
      p1 <- ggplotly(p1)
    } # if else loop end
    
  })
  
  output$subset_plot2b <- renderPlotly({
    
    myData <- data_to_model$data_subset
    req(input$subset_end_points_in_series)
    if (length(myData)==0){
      
      alert_message ="No subet was created, please go back to the previous tab to create your subset."
      shinyalert(" ",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info4")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
    }else{
      if (input$subset_end_points_in_series=="LogTP"){
        p2<-ggplot(myData, aes(x=as.factor(Year), y=LogTP,fill=as.factor(Year)))+
          geom_boxplot(fill=NA)+
          labs(x="Year", y = paste0("ln","(TP, \u03bcg/L)"))
      }else if(input$subset_end_points_in_series=="TP"){
        p2<-ggplot(myData, aes(x=as.factor(Year), y=TP,fill=as.factor(Year)))+
          geom_boxplot(fill=NA)+
          labs(x="Year", y = paste0(input$subset_end_points_in_series," (\u03bcg/L)"))
      }else if(input$subset_end_points_in_series=="LogTN"){
        p2<-ggplot(myData, aes(x=as.factor(Year), y=LogTN,fill=as.factor(Year)))+
          geom_boxplot(fill=NA)+
          labs(x="Year", y = paste0("ln","(TN, \u03bcg/L)"))
      }else if(input$subset_end_points_in_series=="TN"){  
        p2<-ggplot(myData, aes(x=as.factor(Year), y=TN,fill=as.factor(Year)))+
          geom_boxplot(fill=NA)+
          labs(x="Year", y = paste0(input$subset_end_points_in_series," (\u03bcg/L)"))
      }
      p2<- p2 +
        theme_classic()+
        theme(text=element_text(size=14,face = "bold", color="blue"),
              axis.text.x=element_text(angle=45, hjust=1))+
        guides(fill=guide_legend(title="Year"))
    } # if else loop end
    p2 <- ggplotly(p2)
  })
  
  
  observe({
    if (req(input$subset_tab3b)== "subset_sb1"){
      
      ## for Tab 3:HUC2 Summaries, sub-tab 1: HU2 frequency bar plot and pie chart
      output$subset_plot3 <- renderPlot({
        myData <- data_to_model$data_subset
        req(input$subset_end_points_in_HUC2)
        myData_1 <- myData %>% select(HU2,input$subset_end_points_in_HUC2) %>% na.omit()
        p1 <- ggplot(myData_1,aes(x=HU2))+
          geom_bar(fill = "#0073C2FF")+
          ylab(paste0("Number of valid ",input$subset_end_points_in_HUC2," data records"))+
          theme_classic()+
          theme(text=element_text(size=15,face = "bold",color="blue"),
                plot.title=element_text(hjust=0.5,size=14),
                axis.text.x=element_text(angle=0, hjust=1)) +
          scale_x_discrete("HUC 2 Zone ID", labels = as.character(myData$HU2), breaks = myData$HU2) + 
          theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, linetype="solid"))
        
        df <- data.frame(table(myData_1$HU2)) %>%
          mutate(Abs_Freq = format(round(as.numeric(Freq/sum(Freq)*100))/100, 2)) %>%
          rename(HU2=Var1)
        df$label <- scales::percent(as.numeric(df$Abs_Freq))
        df<-df[order(df$Freq),]
        p2<-ggplot(data=df)+
          ggtitle(paste0("Percentage of valid ",input$subset_end_points_in_HUC2," data records"))+
          geom_col(aes(x="", y=as.numeric(Abs_Freq), fill=as.factor(HU2)), width = 1)+
          coord_polar("y", start=0)+
          theme_void()+
          geom_text(aes(x=1.6, y = as.numeric(Abs_Freq), label=label, group = as.factor(HU2)),
                    position = position_stack(vjust = 0.5))+ 
          guides(fill=guide_legend(title="HU2"))+
          theme(text=element_text(size=15,face = "bold", color="blue"),plot.title=element_text(hjust=0.5,size=15))+
          theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, linetype="solid"))
        grid.arrange(p1, p2, ncol=2)
      }) #renderPlot close
      
    }else if (req(input$subset_tab3b)=="subset_sb2"){   
      
      ## for Tab 3:HUC2 Summaries, sub-tab 2: HU2 frequency box plot
      
      output$subset_plot4 <- renderPlot({
        
        myData <- data_to_model$data_subset
        req(input$subset_end_points_in_HUC2_box)
        #print("create the HU2 boxplot (plot4) now...")
        if (input$subset_end_points_in_HUC2_box=="LogTP"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=LogTP,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0("ln","(TP, \u03bcg/L)"))
        }else if (input$subset_end_points_in_HUC2_box=="LogTN"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=LogTN,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0("ln","(TN, \u03bcg/L)"))
        }else if (input$subset_end_points_in_HUC2_box=="TP"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=TP,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0(input$subset_end_points_in_HUC2_box," (\u03bcg/L)"))
        }else if (input$subset_end_points_in_HUC2_box=="TN"){
          p2 <- ggplot(myData, aes(x=as.factor(HU2), y=TN,fill=as.factor(HU2)))+
            geom_boxplot(fill=NA)+theme(legend.position="none")+
            labs(x="HUC2 Zone ID", y = paste0(input$subset_end_points_in_HUC2_box," (\u03bcg/L)"))
        }
        p2 <- p2 + 
          theme_classic()+
          guides(fill=guide_legend(title="HUC2 Zone ID"))+ 
          theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5,linetype="solid"))+
          theme(text=element_text(size=15,face = "bold", color="blue"))
        print(p2)
      }) # renderPlot close
      
    }else if(req(input$subset_tab3b)=="subset_sb3"){
      
      ## for Tab 3:Endpoint HUC2 Summaries, sub-tab 3: HUC2 watershed map
      output$subset_display_HUC2_watershed_map <- renderUI({
        imageOutput("subset_HUC2_watershed_map")
      })
      
      output$subset_HUC2_watershed_map <- renderImage({
        list(src="www/HUC2_location_map.png",
             contentType = 'image/png',
             width=800,
             height=600,
             alt='this is the HUC2 location map')
      },deleteFile = FALSE)
    } #if else loop end
  }) #observe end    
  
  
  ## for subset_tab3_add:Predictor variables HUC2 Summaries, predictor variables HU2 frequency box plot
  
  output$subset_display_plot4_add <- renderUI({
    withSpinner(plotOutput("subset_plot4_add"),type=2)
  })
  
  output$subset_plot4_add <- renderPlot({
    myData <- data_to_model$data_subset
    req(input$subset_select_variable_to_summary)
    print("create the HU2 boxplot (plot4 add) now...")
    
    variable_max = max(myData[,input$subset_select_variable_to_summary],na.rm=TRUE)
    print(variable_max)
    
    y_axis_max = variable_max*input$subset_Y_axis_scale
    
    p2 <- ggplot(myData, aes(x=as.factor(HU2), y=!!sym(input$subset_select_variable_to_summary),fill=as.factor(HU2)))+
      geom_boxplot(fill=NA)+
      labs(x="HUC2 Zone ID", y = paste0(input$subset_select_variable_to_summary))+
      theme_classic()+
      guides(fill=guide_legend(title="HUC2 Zone ID"))+ 
      theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5,linetype="solid"))+
      theme(text=element_text(size=15,face = "bold", color="blue"))+
      ylim(0,y_axis_max)
    print(p2)
    
  })
  
  
  
  ## for Tab 4: Correlations, sub-tab 1: bivariate
  observeEvent(input$subset_display_bivariate_plots, {
    
    output$subset_display_plot5 <- renderUI({
      withSpinner(plotOutput("subset_plot5"),type=2)
    })
    
    myData <- data_to_model$data_subset
    
    output$subset_plot5 <- renderPlot({
      
      input$subset_display_bivariate_plots
      
      p1<-ggplot(myData, aes_string(x=isolate(input$subset_select_bivariate_x1), y=isolate(input$subset_select_bivariate_y1))) + 
        geom_point()+
        geom_smooth(formula = y ~ x, method=lm)+
        ylab(isolate(input$subset_select_bivariate_y1))+
        # scale_x_continuous("HUC 2 Zone ID", labels = as.character(myData$HU2), breaks = myData$HU2)+ 
        theme_classic()+
        theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5, linetype="solid"))+
        theme(text=element_text(size=15,face = "bold", color="blue"))
      
      p2<-ggplot(myData, aes_string(x=isolate(input$subset_select_bivariate_x2), y=isolate(input$subset_select_bivariate_y2))) + 
        geom_point()+
        geom_smooth(formula = y~x, method=lm)+
        ylab(isolate(input$subset_select_bivariate_y2))+
        # scale_x_continuous("HUC 2 Zone ID", labels = as.character(myData$HU2), breaks = myData$HU2)+
        theme_classic()+
        theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5,linetype="solid"))+
        theme(text=element_text(size=15,face = "bold", color="blue"))
      
      grid.arrange(p1, p2, ncol=2)
    }) # renderPlot end
    
  }) #observeEvent end
  
  
  observeEvent(input$subset_display_multivariate_plots, {
    
    start_time <- Sys.time()
    output$subset_display_plot6 <- renderUI({
      withSpinner(plotOutput("subset_plot6"),type=2)
    })
    
    output$subset_display_plot7 <- renderUI({
      withSpinner(plotOutput("subset_plot7"),type=2)
    })
    
    output$display_footnote_subset_plot6 <- renderUI({
      div(style="overflow-x:hidden",verbatimTextOutput("subset_plot6_correlation_footnote"))
    })
    
    myData <- data_to_model$data_subset
    selected_data <- myData %>% select(c(input$subset_select_multivariate_end,input$subset_select_multivariate_predictor))
    
    ## check if there is any predictor variable of the entire column is NA
    ## if so, remove those columns
    need_to_remove_names = colnames(selected_data[,colSums(is.na(selected_data))==nrow(selected_data)])
    selected_data <- selected_data[,colSums(is.na(selected_data))!=nrow(selected_data)]
    selected_data <- na.roughfix(selected_data)
    
    output$subset_plot6 <- renderPlot({
      
      ## if we use "GGally" package, use the following code
      p1 <- ggcorr(selected_data,method=c("complete","pearson"),label=TRUE,label_round=3)
      print(p1)
    })
    
    
    output$subset_plot7 <- renderPlot({
      
      glm_mapping <- function(data,mapping,method="glm",...){
        p <- ggplot(data=data,mapping = mapping)+
          geom_point()+
          geom_smooth(formula = y~x,method=method,...)
        p
      }
      p2 <- ggpairs(selected_data,lower=list(continuous=glm_mapping))
      print(p2)
    }) #renderPlot end
    
    #end_time <- Sys.time()
    
    #print(paste0("run time for the multivariate plot is:",end_time-start_time))
    
    
    output$subset_plot6_correlation_footnote <- renderText({
      
      Note_text_line1= paste0("The correlation coefficients displayed in this page are computed by Pearson's method")
      
    })
    
  }) #observeEvent end
  
  output$subset_map_on_HUC2 <- renderPlot({
    print("inside rendering the summary HUC2 map for the subset now...")
    
    myData <- data_to_model$data_subset
    req(input$subset_end_points_in_maps)
    if(input$subset_end_points_in_maps=="LogTP"){
      mySummary <- myData %>% select(HU2,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(LogTP = mean(LogTP))
    }else if(input$subset_end_points_in_maps=="LogTN"){
      mySummary <- myData %>% select(HU2,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(LogTN = mean(LogTN)) 
    }else if(input$subset_end_points_in_maps=="TN"){
      mySummary <- myData %>% select(HU2,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(TN = mean(TN)) 
    }else if(input$subset_end_points_in_maps=="TP"){
      mySummary <- myData %>% select(HU2,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU2) %>% summarize(TP = mean(TP)) 
    }
    
    mySummary$huc2 = formatC(as.numeric(as.character(mySummary$HU2)), width = 2, format = "d",flag = "0") # add zero in front of any
    
    if (length(mySummary$huc2)>=8){
      NE_states_to_show_final = NE_states_all
    }else{
      NE_states_to_show_list=""
      if ("01" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c( 'Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Connecticut', 'Rhode Island'))
      }
      if ("02" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('New York' , 'Pennsylvania', 'New Jersey', 'Virginia', 'West Virginia'))
      }
      if ("03" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('North Carolina','South Carolina','Georgia','Alabama','Florida'))
      }
      if ("04" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('Wisconsin' , 'Michigan',  'Indiana', 'Ohio','New York' , 'Pennsylvania','Vermont','Maine','New Hampshire'))
      }
      if ("05" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('Ohio', 'Indiana', 'Illinois', 'Kentucky', 'West Virginia'))
      }
      if ("07" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('Minnesota', 'Iowa', 'Missouri', 'Wisconsin', 'Illinois'))
      }
      if ("08" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('Louisiana','Mississippi','Arkansas','Tennessee'))
      }
      if ("09" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('North Dakota','Minnesota'))
      }
      if ("10" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('North Dakota','South Dakota','Nebraska','Montana','Wyoming','Colorado','Kansas','Missouri'))
      }
      if ("11" %in% mySummary$huc2){
        NE_states_to_show_list <- append(NE_states_to_show_list, c('Oklahoma','New Mexico','Texas'))
      }
      NE_states_to_show_final = NE_states_to_show_list 
    }
    
    NE_states_list$names_in_subset <- NE_states_to_show_final 
    NE_to_show <- states_in_US[states_in_US$name %in% NE_states_to_show_final,]
    
    myjoinedSummary = merge(all_joined_HU2,mySummary,by="huc2",duplicateGeoms=TRUE)
    myjoinedSummaryToMap = st_as_sf(myjoinedSummary)
    rm(mySummary,myjoinedSummary)
    myMap <- ggplot()+
      ggtitle(paste0(input$subset_end_points_in_maps," Summarized by HUC2"))+
      geom_sf(data=NE_to_show,color="black",fill=NA)+
      geom_sf(myjoinedSummaryToMap,mapping=aes_string(fill=input$subset_end_points_in_maps),colour=NA,alpha=0.8)+
      scale_fill_viridis(option="viridis",name=input$subset_end_points_in_maps)+
      theme(panel.background = element_blank())+ # remove grey background
      theme(plot.title=element_text(hjust=0.5))+ # put the title at center
      theme(axis.text = element_blank())+ # remove axis text
      theme(axis.ticks = element_blank()) # remove axis text
    print(myMap)
  })
  
  output$subset_map_on_HUC8 <- renderPlot({
    print("inside rendering the summary HUC8 map for the subset now...")
    myData <- data_to_model$data_subset
    load("./Data/all_HU8_shapes.RData")
    req(input$subset_end_points_in_maps)
    all_joined_HU8 <- st_as_sf(joined_HU8)
    if(input$subset_end_points_in_maps=="LogTP"){
      mySummary <- myData %>% select(HU8,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(LogTP = mean(LogTP))
    }else if(input$subset_end_points_in_maps=="LogTN"){
      mySummary <- myData %>% select(HU8,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(LogTN = mean(LogTN))
    }else if(input$subset_end_points_in_maps=="TN"){
      mySummary <- myData %>% select(HU8,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(TN = mean(TN))
    }else if(input$subset_end_points_in_maps=="TP"){
      mySummary <- myData %>% select(HU8,input$subset_end_points_in_maps) %>% na.omit() %>% group_by(HU8) %>% summarize(TP = mean(TP))
    }
    mySummary$huc8 = formatC(as.numeric(as.character(mySummary$HU8)), width = 8, format = "d",flag = "0") # add zero in front of any
    myjoinedSummary = merge(all_joined_HU8,mySummary,by="huc8",duplicateGeoms=TRUE)
    myjoinedSummaryToMap = st_as_sf(myjoinedSummary)
    rm(joined_HU8,all_joined_HU8,mySummary,myjoinedSummary)
    
    NE_to_show_in_HUC8 <- states_in_US[states_in_US$name %in% NE_states_list$names_in_subset,]
    myMap <- ggplot()+
      ggtitle(paste0(input$subset_end_points_in_maps," Summarized by HUC8"))+
      geom_sf(data=NE_to_show_in_HUC8,color="black",fill=NA)+
      geom_sf(myjoinedSummaryToMap,mapping=aes_string(fill=input$subset_end_points_in_maps),colour=NA,alpha=0.8)+
      scale_fill_viridis(option="viridis",name=input$subset_end_points_in_maps)+
      theme(panel.background = element_blank())+ # remove grey background
      theme(plot.title=element_text(hjust=0.5))+ # put the title at center
      theme(axis.text = element_blank())+ # remove axis text
      theme(axis.ticks = element_blank()) # remove axis text
    print(myMap)
  })
  
  output$subset_plot8 <- renderPlot({
    myData <- data_to_model$data_subset
    req(input$subset_end_points_in_Misc)
    myData_1 <- myData %>% select(HU2,programtype,input$subset_end_points_in_Misc) %>% na.omit()
    res <- myData_1 %>% group_by(HU2,programtype) %>% summarise(Frequency=n())
    p <- ggplot(res, aes(x = HU2, y = Frequency))+
      geom_col(aes(fill = programtype), width = 0.7)+
      scale_x_discrete( labels = as.character(res$HU2), breaks = res$HU2)+
      xlab("HUC2 Zone ID")+
      ylab(paste0("Number of valid ",input$subset_end_points_in_Misc," data records"))+
      theme_classic()+
      theme(text=element_text(size=15,face = "bold", color="blue"))
    p
  })
  
  #######################################################################################################################
  ######################################## server code end for tabPanel "Explore Subset"################################
  #######################################################################################################################
  
  
  #######################################################################################################################
  ######################################## server code start for tabPanel "run models"###################################
  #######################################################################################################################
  
  #################################### UI logic flow for switching to different model types #############################
  selectizeTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = list(container="body")){
    options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
    options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
    bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      var opts = $.extend(", options, ", {html: true});
      var selectizeParent = document.getElementById('", id, "').parentElement;
      var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation){
          $(mutation.addedNodes).filter('div').filter(function(){return(this.getAttribute('data-value') == '", choice, "');}).each(function() {
            $(this).tooltip('destroy');
            $(this).tooltip(opts);
          });
        });
      });
      observer.observe(selectizeParent, { subtree: true, childList: true });
    });
  ")))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
  }
  
  js_remove_tooltips <- paste0("$('.tooltip').remove()")
  
  js2_group_1 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_1');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_1 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  js2_group_2 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_2');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_2 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  js2_group_3 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_3');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_3 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  js2_group_4 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_4');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_4 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  js2_group_5 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_5');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_5 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  js2_group_6 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_6');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_6 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  js2_group_7 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_7');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_7 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  js2_group_8 <- paste0(c(
    "var selectinput = document.getElementById('select_from_group_8');",
    "selectinput.selectize.setValue(-1, false);",
    "selectinput.selectize.selectall();",
    "$('#select_from_group_8 + .selectize-control .item').removeClass('active');"),
    collapse = "\n")
  
  
  observeEvent(input$ChooseModel, {
    data <- data_to_model$data_subset
    ###### the following code is added to bring in the variable name descriptions as tooltips ##########
    ###### the pre-processing started from "LAGOS_Full_Variables.xlsx" #################
    # name_desc_file <- read_excel("./data/LAGOS_Full_Variables.xlsx")
    # name_desc_file_sub <- name_desc_file[match(default_128,name_desc_file$`Variable Name`),]
    # name_description <- name_desc_file_sub[,1:2]
    # save(name_description,file="./data/LAGOS_name_description_subset.RData")
    load("./data/LAGOS_name_description_subset.RData")
    all_name_desc <- as.data.frame(name_description)
    
    ####################### this section is added to change the UI layout for user to select predictors by groups ################
    #######################             code revision start                        ###############################################
    
    output$userInput_actionB_1 <- renderUI({
      actionButton("selectall_1","Select all in Group 1",onclick = js2_group_1)
    })
    
    output$userInput_group_1 <- renderUI({
      selectizeInput("select_from_group_1",label ="Group 1:NLCD Land Use",
                     choices=group_names_for_default$NLCD_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button'))
      )
      
      
    })
    
    output$tooltips_for_group_1 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$NLCD_names),2]
      w <- lapply(seq_len(length(group_names_for_default$NLCD_names)),function(i){
        selectizeTooltip(id = "select_from_group_1", choice = group_names_for_default$NLCD_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    
    output$userInput_actionB_2 <- renderUI({
      
      actionButton("selectall_2","Select all in Group 2",onclick = js2_group_2)
    })
    
    output$userInput_group_2 <- renderUI({
      selectizeInput("select_from_group_2",label ="Group 2:P Inventory",
                     choices=group_names_for_default$P_inventory_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    output$tooltips_for_group_2 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$P_inventory_names),2]
      w <- lapply(seq_len(length(group_names_for_default$P_inventory_names)),function(i){
        selectizeTooltip(id = "select_from_group_2", choice = group_names_for_default$P_inventory_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    
    output$userInput_actionB_3 <- renderUI({
      
      actionButton("selectall_3","Select all in Group 3",onclick = js2_group_3)
    })
    
    output$userInput_group_3 <- renderUI({
      selectizeInput("select_from_group_3",label ="Group 3:N Inventory",
                     choices=group_names_for_default$N_inventory_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    output$tooltips_for_group_3 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$N_inventory_names),2]
      w <- lapply(seq_len(length(group_names_for_default$N_inventory_names)),function(i){
        selectizeTooltip(id = "select_from_group_3", choice = group_names_for_default$N_inventory_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    
    output$userInput_actionB_4 <- renderUI({
      
      actionButton("selectall_4","Select all in Group 4",onclick = js2_group_4)
    })
    
    output$userInput_group_4 <- renderUI({
      selectizeInput("select_from_group_4",label ="Group 4:Aerosol related",
                     choices=group_names_for_default$aerosol_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    output$tooltips_for_group_4 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$aerosol_names),2]
      w <- lapply(seq_len(length(group_names_for_default$aerosol_names)),function(i){
        selectizeTooltip(id = "select_from_group_4", choice = group_names_for_default$aerosol_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    
    output$userInput_actionB_5 <- renderUI({
      
      actionButton("selectall_5","Select all in Group 5",onclick = js2_group_5)
    })
    
    output$userInput_group_5 <- renderUI({
      selectizeInput("select_from_group_5",label ="Group 5:Weather related",
                     choices=group_names_for_default$weather_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    output$tooltips_for_group_5 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$weather_names),2]
      w <- lapply(seq_len(length(group_names_for_default$weather_names)),function(i){
        selectizeTooltip(id = "select_from_group_5", choice = group_names_for_default$weather_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    
    output$userInput_actionB_6 <- renderUI({
      
      actionButton("selectall_6","Select all in Group 6",onclick = js2_group_6)
    })
    
    output$userInput_group_6 <- renderUI({
      selectizeInput("select_from_group_6",label ="Group 6:Vegetation related",
                     choices=group_names_for_default$vegetation_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    output$tooltips_for_group_6 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$vegetation_names),2]
      w <- lapply(seq_len(length(group_names_for_default$vegetation_names)),function(i){
        selectizeTooltip(id = "select_from_group_6", choice = group_names_for_default$vegetation_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    output$userInput_actionB_7 <- renderUI({
      
      actionButton("selectall_7","Select all in Group 7",onclick = js2_group_7)
    })
    
    output$userInput_group_7 <- renderUI({
      selectizeInput("select_from_group_7",label ="Group 7:Deposition",
                     choices=group_names_for_default$deposition_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    output$tooltips_for_group_7 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$deposition_names),2]
      w <- lapply(seq_len(length(group_names_for_default$deposition_names)),function(i){
        selectizeTooltip(id = "select_from_group_7", choice = group_names_for_default$deposition_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    output$userInput_actionB_8 <- renderUI({
      
      actionButton("selectall_8","Select all in Group 8",onclick = js2_group_8)
    })
    
    output$userInput_group_8 <- renderUI({
      selectizeInput("select_from_group_8",label ="Group 8:Surface water, Watershed, & Misc.",
                     choices=group_names_for_default$lake_char_names,
                     multiple = TRUE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    output$tooltips_for_group_8 <- renderUI({
      name_desc <- all_name_desc[which(all_name_desc$`Variable Name` %in% group_names_for_default$lake_char_names),2]
      #print(name_desc)
      w <- lapply(seq_len(length(group_names_for_default$lake_char_names)),function(i){
        selectizeTooltip(id = "select_from_group_8", choice = group_names_for_default$lake_char_names[i], title = name_desc[i], placement = "right") 
      })
      
      do.call(fluidPage,w)
    })
    
    output$userInput_select_all <- renderUI({
      actionButton("select_all_default","Select all default variables",style="color:#0022FF;background-color:black")
    })
    
    output$userInput_clear_all <- renderUI({
      actionButton("clear_all_selected","Clear all",style="color:#0022FF;background-color:black",onclick=js_remove_tooltips)
    })
    
    #######################             code revision end                        ############################################### 
    if (input$model_type == "Multilinear regression (on data subset)") {
      shinyjs::hide("random_forest_panel")
      shinyjs::hide("random_forest_output_panel")
      shinyjs::show("regression_panel")
      shinyjs::show("regression_correlation_analysis_panel")
<<<<<<< HEAD
      
      data <- data_to_model$data_subset
      
      output$select_LR_end_points <- renderUI({
        selectizeInput("select_LR_end_points",label ="please select end points",
                       choices=c("TN","TP","LogTN","LogTP"),
                       multiple = FALSE,
                       selected="LogTP",
                       options = list(hideSelected = FALSE))
      })
      
      # output$userInput_LR_select_group <- renderUI({
      #   checkboxGroupInput("selectGroupLR","Select predictor variables by groups",c("NLCD Land Use","P Inventory","N Inventory","Aerosol related","Weather related","Vegetation related","Deposition","Date/Lake Characteristics"))
      # })
      
      output$userInput_link_models <- renderUI({
        actionButton(inputId="use_RF_variables_button", label="Link variables from random forest",style="color:blue;background-color:black")
      })
      
      topN_tooltip_text = paste0("the total number of the most important variables linked from the random forest model.","Default is 10.")
      output$userInput_top_n <- renderUI({
        tipify(numericInput("topN",label ="N important variables",10,min=1,max=100,step=1.0),topN_tooltip_text,placement="right",trigger="hover")
      })
      # output$userInput_LR_select <- renderUI({
      #   selectizeInput("select_variables_for_LR",label ="Selected predictor variables:",
      #                  choices=split(data_to_model$default_variable_names,data_to_model$group_for_names),
      #                  multiple = TRUE,
      #                  options = list(hideSelected = FALSE,plugins=list('remove_button')))
      #   
      # })
      
      # output$tooltips_space_holder <- renderUI({
      #   print(data_to_model$default_variable_names[1])
      #   name_desc <- name_description_subset$Description
      #   w <- lapply(seq_len(length(data_to_model$default_variable_names)),function(i){
      #     selectizeTooltip(id = "select_variables_for_LR", choice = data_to_model$default_variable_names[i], title = name_desc[i], placement = "right") 
      #   })
      #   
      #   do.call(fluidPage,w)
      # })
      
      output$userInput_correlation_criteria <- renderUI({
        numericInput("corrValue",label ="correlation criteria",0.9,min=0.5,max=0.95,step=0.05)
      })
      
      output$correlation_analysis <- renderUI({
        actionButton(inputId="RunCorrelationAnalysis", label="Step 1: Run Correlation Analysis",style="color:blue;background-color:black",onclick=js_remove_tooltips)
      })
      
      #nvmax_tooltip_text = paste0("nvmax is the maxinum number of variables used in 'regsubsets' function.","Default is 20.")
      #nbest_tooltip_text = paste0("nbest is the number of best models to keep in 'regsubsets' function.","Default is 1.")
      #nbest_models_tooltip_text = paste0("the number of best models to display for the model selection results.","Default is 5.")
      delta_BIC_tooltip_text = paste0("delta BIC value from the best model with the lowest BIC (Bayesian Information Criterion) for 
                                          the model selection results.","Default is -2.0. ")
      
      output$userInput_LR_nvmax <- renderUI({
        numericInput("nvmax",label ="nvmax",20,min=1,max=50)
      })
      
      output$userInput_LR_nbest <- renderUI({
        numericInput("nbest",label ="nbest",1,min=1,max=5)
      })
      
      output$userInput_nbest_to_display <- renderUI({
        numericInput("nbest_models_to_display",label ="number of best final models to display",5,min=2,max=15)
      })
      
      output$model_selection <- renderUI({
        actionButton(inputId="RunModelSelection", label="Step 2: Run Subset Selection",style="color:blue;background-color:black",onclick=js_remove_tooltips)
      })
      
      output$confirm_to_run_LR <- renderUI({
        actionButton(inputId="RunRegressionModel", label="Step 3: Finalize Regression Model",style="color:blue;background-color:black")
      })
      
      output$confirm_to_run_LR_prediction <- renderUI({
        actionButton(inputId="MakeRegressionModelPrediction", label="Step 4: Make Model Predictions",style="color:blue;background-color:black")
      })
      
=======
    
        data <- data_to_model$data_subset
          
          output$select_LR_end_points <- renderUI({
            selectizeInput("select_LR_end_points",label ="please select end points",
                           choices=c("TN","TP","LogTN","LogTP"),
                           multiple = FALSE,
                           selected="LogTP",
                           options = list(hideSelected = FALSE))
          })
          
          # output$userInput_LR_select_group <- renderUI({
          #   checkboxGroupInput("selectGroupLR","Select predictor variables by groups",c("NLCD Land Use","P Inventory","N Inventory","Aerosol related","Weather related","Vegetation related","Deposition","Date/Lake Characteristics"))
          # })
          
          output$userInput_link_models <- renderUI({
            actionButton(inputId="use_RF_variables_button", label="Link variables from random forest",style="color:blue;background-color:black")
          })
          
          topN_tooltip_text = paste0("the total number of the most important variables linked from the random forest model.","Default is 10.")
          output$userInput_top_n <- renderUI({
            tipify(numericInput("topN",label ="N important variables",10,min=1,max=100,step=1.0),topN_tooltip_text,placement="right",trigger="hover")
          })
          # output$userInput_LR_select <- renderUI({
          #   selectizeInput("select_variables_for_LR",label ="Selected predictor variables:",
          #                  choices=split(data_to_model$default_variable_names,data_to_model$group_for_names),
          #                  multiple = TRUE,
          #                  options = list(hideSelected = FALSE,plugins=list('remove_button')))
          #   
          # })
          
          # output$tooltips_space_holder <- renderUI({
          #   print(data_to_model$default_variable_names[1])
          #   name_desc <- name_description_subset$Description
          #   w <- lapply(seq_len(length(data_to_model$default_variable_names)),function(i){
          #     selectizeTooltip(id = "select_variables_for_LR", choice = data_to_model$default_variable_names[i], title = name_desc[i], placement = "right") 
          #   })
          #   
          #   do.call(fluidPage,w)
          # })
          
          output$userInput_correlation_criteria <- renderUI({
            numericInput("corrValue",label ="correlation criteria",0.9,min=0.5,max=0.95,step=0.05)
          })
          
          output$correlation_analysis <- renderUI({
            actionButton(inputId="RunCorrelationAnalysis", label="Step 1: Run Correlation Analysis",style="color:blue;background-color:black",onclick=js_remove_tooltips)
          })
          
          #nvmax_tooltip_text = paste0("nvmax is the maxinum number of variables used in 'regsubsets' function.","Default is 20.")
          #nbest_tooltip_text = paste0("nbest is the number of best models to keep in 'regsubsets' function.","Default is 1.")
          #nbest_models_tooltip_text = paste0("the number of best models to display for the model selection results.","Default is 5.")
          delta_BIC_tooltip_text = paste0("delta BIC value from the best model with the lowest BIC (Bayesian Information Criterion) for 
                                          the model selection results.","Default is -2.0. ")
          
          output$userInput_LR_nvmax <- renderUI({
            numericInput("nvmax",label ="nvmax",20,min=1,max=50)
          })
          
          output$userInput_LR_nbest <- renderUI({
            numericInput("nbest",label ="nbest",1,min=1,max=5)
          })
          
          output$userInput_nbest_to_display <- renderUI({
            numericInput("nbest_models_to_display",label ="number of best final models to display",5,min=2,max=15)
          })
          
          output$model_selection <- renderUI({
            actionButton(inputId="RunModelSelection", label="Step 2: Run Subset Selection",style="color:blue;background-color:black",onclick=js_remove_tooltips)
          })
          
          output$confirm_to_run_LR <- renderUI({
            actionButton(inputId="RunRegressionModel", label="Step 3: Finalize Regression Model",style="color:blue;background-color:black")
          })
          
          output$confirm_to_run_LR_prediction <- renderUI({
            actionButton(inputId="MakeRegressionModelPrediction", label="Step 4: Make Model Predictions",style="color:blue;background-color:black")
          })
    
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
    }else if (input$model_type == "Random forest (on data subset)"){
      shinyjs::hide("regression_panel")
      shinyjs::hide("regression_correlation_analysis_panel")
      shinyjs::hide("regression_model_selection_output_panel")
      shinyjs::hide("regression_model_prediction_panel")
      shinyjs::show("random_forest_panel")
      shinyjs::show("random_forest_output_panel")
      
      shinyjs::hide(id="topN")
      
      RF_information = paste0("The default random forest regression model uses 128 pre-selected predictor variables.",
                              " Please click on 'Select all default variables' button on top right then click on 'Run random forest' button on the left to run the default random forest regression model.",
                              " You can also select one or more variable groups by using the buttons above each dropdown menu to load the predictors as the model input.")
      
      shinyalert("Random Forest Model",RF_information,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "RFInfo")
      
      output$select_RF_end_points <- renderUI({
        selectizeInput("select_RF_end_points",label ="please select end points",
                       choices=c("LogTP","TP","TN","LogTN"),
                       multiple = FALSE,
                       selected="LogTP",
                       options = list(hideSelected = FALSE))
      })
      
      ntree_tooltip_text = paste0("The number of trees in the random forest.","Default is 500.")
      
      output$userInput_ntree <- renderUI({
        tipify(numericInput("ntree",label ="ntree",500,min=10,max=3000),ntree_tooltip_text,placement = "right",trigger ="hover")
      })
      
      mtry_tooltip_text = paste0("The number of randomly selected variables to search through to find the optimal tree splits.","Default is the (rounded down) square root of the total number of predictor variables.")
      
      output$userInput_mtry <- renderUI({
        tipify(numericInput("mtry",label ="mtry",0,min=0,max=300),mtry_tooltip_text,placement="right",trigger="hover")
      })
      
      testingsetperc_tooltip_text = paste0("The proportion of observations from the user-specified subset that is to be treated as a testing dataset.","Default is 0.1.")
      
      output$userInput_percentage <- renderUI({
        tipify(numericInput("TestingsetPerc",label ="testingset percentage",0.1,min=0.01,max=0.5),testingsetperc_tooltip_text,placement="right",trigger="hover")
      })
      
      minnode_tooltip_text = paste0("The minimum size of terminal nodes within each tree.","Decreasing this value will result in trees that are more complex.","Default is 5.")
      
      output$userInput_min_node <- renderUI({
        tipify(numericInput("minnode",label ="minimum node",5,min=1,max=50),minnode_tooltip_text,placement="right",trigger="hover")
      })
      
      seedvalue_tooltip_text = paste0("Setting the seed value for random forest model allows you to generate reproducible results.","Default is 10.")
      
      output$userInput_seed_value <- renderUI({
        tipify(numericInput("seedvalue",label ="set seed",10,min=1,max=50),seedvalue_tooltip_text,placement="right",trigger="hover")
      })
      
      # output$userInput_select_group <- renderUI({
      #   checkboxGroupInput("selectGroupRF","Select predictor variables by groups",c("NLCD Land Use","P Inventory","N Inventory","Aerosol related","Weather related","Vegetation related","Deposition","Date/Lake Characteristics"))
      # })
      
      # output$userInput_RF_select <- renderUI({
      #   selectizeInput("select_variables_for_RF",label ="Selected predictor variables:",
      #                  choices=split(data_to_model$default_variable_names,data_to_model$group_for_names),
      #                  multiple = TRUE,
      #                  options = list(hideSelected = FALSE,plugins=list('remove_button')))
      # })
      
      # output$RF_tooltips_space_holder <- renderUI({
      #   #print(data_to_model$default_variable_names[1])
      #   name_desc <- name_description_subset$Description
      #   w <- lapply(seq_len(length(data_to_model$default_variable_names)),function(i){
      #     selectizeTooltip(id = "select_variables_for_RF", choice = data_to_model$default_variable_names[i], title = name_desc[i], placement = "bottom") 
      #   })
      #   
      #   do.call(fluidPage,w)
      # })
      
      # output$userInput_load_RF_default <- renderUI({
      #   actionButton(inputId="LoadAllDefault", label="Load all default variables",style="color:blue;background-color:black")
      # })
      
      output$confirm_to_run_RF <- renderUI({
        actionButton(inputId="ConfirmRunRandomForest", label="Run random forest",style="color:blue;background-color:black",onclick=js_remove_tooltips)
      })
      
      output$userInput_link_models <- renderUI({
        actionButton(inputId="use_LR_variables_button", label="Link variables from multilinear regression model",style="color:blue;background-color:black")
      })
      
      # output$display_run_LR_variables_button <- renderUI({
      #   actionButton(inputId="run_LR_variables_button", label="Run with multilinear regression variables",style="color:blue;background-color:black")
      # })
      
    }
  })  # ChooseModel loop end
  
  output$modelrunMessage <- renderText({
    "Model is running ..."
  })
  
  observeEvent(input$RFInfo,{
    shinyjs::runjs("swal.close();")
  })
  
  observeEvent(input$select_all_default,{
    updateSelectizeInput(session,"select_from_group_1",choices = group_names_for_default$NLCD_names,selected=group_names_for_default$NLCD_names)
    updateSelectizeInput(session,"select_from_group_2",choices = group_names_for_default$P_inventory_names,selected=group_names_for_default$P_inventory_names)
    updateSelectizeInput(session,"select_from_group_3",choices = group_names_for_default$N_inventory_names,selected=group_names_for_default$N_inventory_names)
    updateSelectizeInput(session,"select_from_group_4",choices = group_names_for_default$aerosol_names,selected=group_names_for_default$aerosol_names)
    updateSelectizeInput(session,"select_from_group_5",choices = group_names_for_default$weather_names,selected=group_names_for_default$weather_names)
    updateSelectizeInput(session,"select_from_group_6",choices = group_names_for_default$vegetation_names,selected=group_names_for_default$vegetation_names)
    updateSelectizeInput(session,"select_from_group_7",choices = group_names_for_default$deposition_names,selected=group_names_for_default$deposition_names)
    updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=group_names_for_default$lake_char_names)
    
  })
  
  observeEvent(input$clear_all_selected,{
    updateSelectizeInput(session,"select_from_group_1",choices = group_names_for_default$NLCD_names,selected=NULL)
    updateSelectizeInput(session,"select_from_group_2",choices = group_names_for_default$P_inventory_names,selected=NULL)
    updateSelectizeInput(session,"select_from_group_3",choices = group_names_for_default$N_inventory_names,selected=NULL)
    updateSelectizeInput(session,"select_from_group_4",choices = group_names_for_default$aerosol_names,selected=NULL)
    updateSelectizeInput(session,"select_from_group_5",choices = group_names_for_default$weather_names,selected=NULL)
    updateSelectizeInput(session,"select_from_group_6",choices = group_names_for_default$vegetation_names,selected=NULL)
    updateSelectizeInput(session,"select_from_group_7",choices = group_names_for_default$deposition_names,selected=NULL)
    updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=NULL)
    
  })
  
  
  ##################################### model 1: run regression model #######################################
  
  ## helper self-defined functions to be used in this section
  
  get_model_formula <- function(id,object,endpoint){
    models <- summary(object)$which[id,-1]
    predictors <- names(which(models == TRUE))
    predictors <- paste(predictors, collapse = "+")
    as.formula(paste0(endpoint,"~",predictors))
  }
  
  
  get_model_predictors <- function(id,object){
    models <- summary(object)$which[id,-1]
    predictors <- names(which(models == TRUE))
    return(predictors)
  }
  
  merge.all <- function(x, ..., by = "row.names") {
    L <- list(...)
    for (i in seq_along(L)) {
      x <- merge(x, L[[i]], by = by)
      rownames(x) <- x$Row.names
      x$Row.names <- NULL
    }
    return(x)
  }
  
  
  plot.regsubsets2 <- 
    function (x, labels = obj$xnames, main = NULL, scale = c("bic", 
                                                             "Cp", "adjr2", "r2"), col = gray(seq(0, 0.9, length = 10)), 
              ...) 
    {
      obj <- x
      lsum <- summary(obj)
      par(mar = c(6, 5, 3, 3) + 0.1) # I modified this line
      nmodels <- length(lsum$rsq)
      np <- obj$np
      propscale <- FALSE
      sscale <- pmatch(scale[1], c("bic", "Cp", "adjr2", "r2"), 
                       nomatch = 0)
      if (sscale == 0) 
        stop(paste("Unrecognised scale=", scale))
      if (propscale) 
        stop(paste("Proportional scaling only for probabilities"))
      yscale <- switch(sscale, lsum$bic, lsum$cp, lsum$adjr2, lsum$rsq)
      up <- switch(sscale, -1, -1, 1, 1)
      index <- order(yscale * up)
      colorscale <- switch(sscale, yscale, yscale, -log(pmax(yscale, 
                                                             1e-04)), -log(pmax(yscale, 1e-04)))
      image(z = t(ifelse(lsum$which[index, ], colorscale[index], 
                         NA + max(colorscale) * 1.5)), xaxt = "n", yaxt = "n", 
            x = (1:np), y = 1:nmodels, xlab = "", ylab = scale[1], 
            col = col)
      laspar <- par("las")
      on.exit(par(las = laspar))
      par(las = 2)
      axis(1, at = 1:np, labels = FALSE) # I modified this line
      text(x=seq(1,np,by=1),par("usr")[3]-0.3,labels=labels,srt=25,pos=2,xpd=TRUE) # I added this line
      # "pos" to specify the text positions below (=1), to the left of (=2), above(=3),to the right of(=4) of the coordinates
      # "srt" to define the lable rotation angle
      # "xpd" to enable things to be drawn outside the plot region
      
      axis(2, at = 1:nmodels, labels = signif(yscale[index], 2))
      if (!is.null(main)) 
        title(main = main)
      box()
      invisible(NULL)
    }
  
  selected_variables_for_regression  <- reactive({
    
    all_selected <- c(input$select_from_group_1,input$select_from_group_2,input$select_from_group_3,input$select_from_group_4,
                      input$select_from_group_5,input$select_from_group_6,input$select_from_group_7,input$select_from_group_8)
    return(all_selected)
  })
  
  observeEvent(input$use_RF_variables_button,{
    if (length(rf_model_output$important_variables)>0){
      n_from_RF = input$topN
      RF_variables = rf_model_output$all_important_variables[1:n_from_RF]
      print(RF_variables)
      ## need to add R code to update the selectizeInput for each group
      if (any(RF_variables %in% group_names_for_default$NLCD_names)){
        selected_variables <- group_names_for_default$NLCD_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        print("updated group 1:")
        print(selected_variables)
        updateSelectizeInput(session,"select_from_group_1",choices = group_names_for_default$NLCD_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_1",choices = group_names_for_default$NLCD_names,selected=NULL)
      }
      
      if (any(RF_variables %in% group_names_for_default$P_inventory_names)){
        selected_variables <- group_names_for_default$P_inventory_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        print("updated group 2:")
        print(selected_variables)
        updateSelectizeInput(session,"select_from_group_2",choices = group_names_for_default$P_inventory_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_2",choices = group_names_for_default$P_inventory_names,selected=NULL)
      }
      
      if (any(RF_variables %in% group_names_for_default$N_inventory_names)){
        selected_variables <- group_names_for_default$N_inventory_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        updateSelectizeInput(session,"select_from_group_3",choices = group_names_for_default$N_inventory_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_3",choices = group_names_for_default$N_inventory_names,selected=NULL)
      }
      
      if (any(RF_variables %in% group_names_for_default$aerosol_names)){
        selected_variables <- group_names_for_default$aerosol_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        updateSelectizeInput(session,"select_from_group_4",choices = group_names_for_default$aerosol_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_4",choices = group_names_for_default$aerosol_names,selected=NULL)
      }
      
      if (any(RF_variables %in% group_names_for_default$weather_names)){
        selected_variables <- group_names_for_default$weather_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        updateSelectizeInput(session,"select_from_group_5",choices = group_names_for_default$weather_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_5",choices = group_names_for_default$weather_names,selected=NULL)
      }
      
      if (any(RF_variables %in% group_names_for_default$vegetation_names)){
        selected_variables <- group_names_for_default$vegetation_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        updateSelectizeInput(session,"select_from_group_6",choices = group_names_for_default$vegetation_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_6",choices = group_names_for_default$vegetation_names,selected=NULL)
      }
      
      if (any(RF_variables %in% group_names_for_default$deposition_names)){
        selected_variables <- group_names_for_default$deposition_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        updateSelectizeInput(session,"select_from_group_7",choices = group_names_for_default$deposition_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_7",choices = group_names_for_default$deposition_names,selected=NULL)
      }
      
      if (any(RF_variables %in% group_names_for_default$lake_char_names)){
        selected_variables <- group_names_for_default$lake_char_names
        selected_variables <- selected_variables[which(selected_variables %in% RF_variables)] 
        updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=NULL)
      }
      shinyjs::hide("regression_model_selection_output_panel")
      shinyjs::hide("regression_model_prediction_panel")
    }else{
      alert_message_no_RF = "No random forest model was created."
      shinyalert("Alert",alert_message_no_RF,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert_no_RF")
    }
  })
  
  observeEvent(input$alert_no_RF,{
    shinyjs::runjs("swal.close();")
  })
  
  
  observeEvent(input$RunCorrelationAnalysis, {
    shinyjs::hide("regression_model_selection_output_panel")
    shinyjs::hide("regression_model_prediction_panel")
    shinyjs::hide("regression_model_final_panel")
    shinyjs::show("regression_correlation_analysis_panel")
    
    data <- data_to_model$data_subset
    
<<<<<<< HEAD
    selected_variables = selected_variables_for_regression()
    
    if (length(selected_variables)==0){
      no_predictors_alert_message = paste0("None predictor variables are selected!")
      shinyalert("Alert",no_predictors_alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "noPredictorAlert")
    }else{
      selected_data_1 <- data %>% select(all_of(selected_variables))  
      
      ## categorical variables are excluded from the linear regression model
      if ("HU2" %in% colnames(selected_data_1)){
        #selected_data_1$HU2 <- as.numeric(as.character(selected_data_1$HU2))
        selected_variables <- selected_variables[-which(selected_variables %in% "HU2")] 
        updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=selected_variables)
        selected_data_1 <- data %>% select(all_of(selected_variables)) 
        remove_alert_message = paste0("Categorical variable HU2 is excluded from the linear regression model!")
        shinyalert(" ",remove_alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                   confirmButtonText="OK",inputId = "removeNote")
      }
      
      ## check if there is any predictor variable of the entire column is NA
      ## if so, remove those columns
      need_to_remove_names = colnames(selected_data_1[,colSums(is.na(selected_data_1))==nrow(selected_data_1)])
      
      if (length(need_to_remove_names)>0){
        print(need_to_remove_names)
        auto_remove_message = paste0("Within this subset, no valid data records for these following variables: ",paste(need_to_remove_names,collapse = ",")," \n they will be automaticallly removed from the selected predictor variables list.")
        shinyalert(" ",auto_remove_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                   confirmButtonText="OK",inputId = "autoRemoveInfo")
        ### find out those "need_to_remove_names" belong to what groups ####
        if (any(need_to_remove_names %in% group_names_for_default$NLCD_names)){
          selected_variables <- input$select_from_group_1
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_1",choices=group_names_for_default$NLCD_names,selected=selected_variables)
        }
        if (any(need_to_remove_names %in% group_names_for_default$P_inventory_names)){
          selected_variables <- input$select_from_group_2
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_2",choices=group_names_for_default$P_inventory_names,selected=selected_variables)
        }
        if (any(need_to_remove_names %in% group_names_for_default$N_inventory_names)){
          selected_variables <- input$select_from_group_3
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_3",choices = group_names_for_default$N_inventory_names,selected=selected_variables)
        }
        if (any(need_to_remove_names %in% group_names_for_default$aerosol_names)){
          selected_variables <- input$select_from_group_4
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_4",choices = group_names_for_default$aerosol_names,selected=selected_variables)
        }
        if (any(need_to_remove_names %in% group_names_for_default$weather_names)){
          selected_variables <- input$select_from_group_5
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_5",choices = group_names_for_default$weather_names,selected=selected_variables)
        }
        if (any(need_to_remove_names %in% group_names_for_default$vegetation_names)){
          selected_variables <- input$select_from_group_6
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_6",choices = group_names_for_default$vegetation_names,selected=selected_variables)
        }
        if (any(need_to_remove_names %in% group_names_for_default$deposition_names)){
          selected_variables <- input$select_from_group_7
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_7",choices = group_names_for_default$deposition_names,selected=selected_variables)
        }
        if (any(need_to_remove_names %in% group_names_for_default$lake_char_names)){
          selected_variables <- input$select_from_group_8
          selected_variables <- selected_variables[-which(selected_variables %in% need_to_remove_names)] 
          updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=selected_variables)
        }
      } # if else loop end
      
      selected_data_1 <- selected_data_1[,colSums(is.na(selected_data_1))!=nrow(selected_data_1)]
      
      selected_data_1 <- na.roughfix(selected_data_1)
      
      mcor <- round(cor(selected_data_1),2)
      
      upper<-mcor
      
      upper[upper.tri(mcor,diag=TRUE)]<-""
      
      lr_model_output$correlation_upper <- upper
      
      output$correlationAnalysis <- DT::renderDataTable({
        
        print("inside displaying correlation analysis table now...")
        Ncols_to_show <- ncol(lr_model_output$correlation_upper)
        if (Ncols_to_show < 9) {
          myTable <- DT::datatable(data.frame(lr_model_output$correlation_upper), extensions = 'Buttons',options = list(
            scrollX = FALSE, 
            stateSave = FALSE,
            pageLength = 60,
            autoWidth = TRUE,
            bSort = FALSE,
            dom = 'Bfrtip',
            buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
            searchCols = default_search_columns,
            search = list(regex=FALSE,caseInsensitive = FALSE, search = default_search),
            columnDefs = list(list(className="dt-center",targets="_all"))
          )) %>%
            formatStyle(names(data.frame(lr_model_output$correlation_upper)),color=JS(paste0("value > ",input$corrValue," & value <= 1 ? 'red' : ''"))) %>%
            formatStyle(names(data.frame(lr_model_output$correlation_upper)),backgroundColor=JS(paste0("value > ",input$corrValue," & value <= 1 ? 'black' : ''")))
          return(myTable)
        }
        else{
          myTable <- DT::datatable(data.frame(lr_model_output$correlation_upper), extensions = 'Buttons',options = list(
            scrollX = TRUE, #allow user to scroll wide tables horizontally
            stateSave = FALSE,
            pageLength = 60,
            autoWidth = TRUE,
            bSort = FALSE,
            dom = 'Bfrtip',
            buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
            searchCols = default_search_columns,
            search = list(regex=FALSE,caseInsensitive = FALSE, search = default_search),
            columnDefs = list(list(className="dt-center",targets="_all"))
          )) %>%
            formatStyle(names(data.frame(lr_model_output$correlation_upper)),color=JS(paste0("value > ",input$corrValue," & value <= 1 ? 'red' : ''"))) %>%
            formatStyle(names(data.frame(lr_model_output$correlation_upper)),backgroundColor=JS(paste0("value > ",input$corrValue," & value <= 1 ? 'black' : ''")))
          return(myTable)
        }
        print(myTable)
        
        
      }) #renderDataTable end
      
      
      output$showHighCells <- renderUI({
        actionButton(inputId="showhighcells", label="only show cells with high correlation",style="color:blue;background-color:black")
      })
      
      # print("After running correlation analysis: ")
      # print(pryr::mem_used())
      data_to_show <- data.frame(lr_model_output$correlation_upper)
    } # for else loop end
  }) #observeEvent end
  
  observeEvent(input$noPredictorAlert,{
    shinyjs::runjs("swal.close();")
  })
  
  observeEvent(input$autoRemoveInfo,{
    #print(input$autoRemoveInfo)
    shinyjs::runjs("swal.close();")
  })
  
  observeEvent(input$showhighcells,{
    
    print("inside displaying high correlation cells now...")
    
    myCorrelation <- data.frame(lr_model_output$correlation_upper)
    myCorrelation <- myCorrelation[,colSums(is.na(myCorrelation))==0]
    #print(myCorrelation)
    upper_cols <- myCorrelation[colSums(myCorrelation > input$corrValue & myCorrelation <= 1.0)>0]
    print(upper_cols)
    #save(upper_cols,file="./test_upper_cols.RData")
    if (ncol(upper_cols)==1){
      get_rowname <- rownames(upper_cols)[rowSums(upper_cols > 0.9 & upper_cols <= 1.0)>0]
      get_colname <- colnames(upper_cols)
      high_correlation_upper <- data.frame(upper_cols[rowSums(upper_cols > input$corrValue & upper_cols <= 1.0)>0,])
      colnames(high_correlation_upper) <- get_colname
      rownames(high_correlation_upper) <-get_rowname
    }else{
      high_correlation_upper <- upper_cols[rowSums(upper_cols > input$corrValue & upper_cols <=1.0)>0,]
    }
    print(high_correlation_upper)
    if (nrow(high_correlation_upper)==0) {
      alert_message_no_high = "There is no cell with high correlation to display.Please proceed to the next step."
      shinyalert(" ",alert_message_no_high,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert_no_high")
      
      output$filteredCorrelationAnalysis <- DT::renderDataTable({
        myTable <- DT::datatable(high_correlation_upper,options = list(
          scrollX = TRUE, #allow user to scroll wide tables horizontally
          stateSave = FALSE,
          pageLength = 60,
          autoWidth = FALSE,
          bSort = FALSE,
          searchCols = default_search_columns,
          search = list(regex=FALSE,caseInsensitive = FALSE, search = default_search),
          columnDefs = list(list(className="dt-center",targets="_all"))
        ))
        print(myTable)
      })
      
    }else{
      output$filteredCorrelationAnalysis <- DT::renderDataTable({
        
        myTable <- DT::datatable(high_correlation_upper,options = list(
          scrollX = TRUE, #allow user to scroll wide tables horizontally
          stateSave = FALSE,
          pageLength = 60,
          autoWidth = FALSE,
          bSort = FALSE,
          searchCols = default_search_columns,
          search = list(regex=FALSE,caseInsensitive = FALSE, search = default_search),
          columnDefs = list(list(className="dt-center",targets="_all"))
        )) %>%
          formatStyle(names(high_correlation_upper),color=JS(paste0("value > ",input$corrValue," & value < 1 ? 'red' : ''"))) %>%
          formatStyle(names(high_correlation_upper),backgroundColor=JS(paste0("value > ",input$corrValue," & value < 1 ? 'black' : ''")))
        return(myTable)
        
        print(myTable)
        
      }) #renderDataTable end
      
      output$display_footnote_text <- renderUI({
        div(style="overflow-x:hidden",verbatimTextOutput("correlation_table_footnote"))
      })
      
      output$correlation_table_footnote <- renderText({
        Note_text_line1= paste0("We recommend to remove the highly-correlated predictor variables (correlation > ",input$corrValue, " marked in red ) from the multi-linear regression model inputs.")
        Note_text_line2= paste0("Please use the above table as your reference, pick one variable from each highly-correlated variable pairs and remove them from the selected predictor variables list on the top.")
        Note_text_line3 = paste0("Then click on 'Step 2: Run Subset Selection' to proceed to the next step.")
        paste(Note_text_line1,Note_text_line2,Note_text_line3,sep="\n")
      })
      
    } #if else loop end
    
  }) #observeEvent end
  
  observeEvent(input$removeNote,{
    shinyjs::runjs("swal.close();")
  })
  
  observeEvent(input$alert_no_high,{
    shinyjs::runjs("swal.close();")
  })
  
  observeEvent(input$RunModelSelection, {
    
    shinyjs::show("regression_correlation_analysis_panel")
    shinyjs::hide("regression_model_final_panel")
    shinyjs::hide("regression_model_prediction_panel")
    shinyjs::show("regression_model_selection_output_panel")
    output$RegressionModelSelectionOutput1 <- renderUI({
      plotOutput("model_selection_BIC")
    })
    
    output$RegressionModelSelectionOutput2 <- renderUI({
      plotOutput("model_selection_r2")
    })
    
    output$RegressionModelSelectionOutput3 <- renderUI({
      plotOutput("model_selection_Cp")
    })
    
    output$RegressionModelSelectionOutput4 <- renderUI({
      plotOutput("model_selection_adj_r2")
    })
    
    output$RegressionModelSelectionOutput5 <- renderUI({
      DT::dataTableOutput("regsModelSummary")
    })
    
    output$display_model_selection_footnote_text <- renderUI({
      div(style="overflow-x:hidden",verbatimTextOutput("model_selection_footnote"))
    })
    
    output$model_selection_footnote <- renderText({
      Note_text_line1= paste0("This table displays the ", input$nbest_models_to_display, " best model subsets for multi-linear regression model.")
      Note_text_line2= paste0("They are sorted from lowest to highest according to BIC (Bayesian Information Criterion) values. ")
      Note_text_line3= paste0("If you want to see more/less models displayed, please increase/decrease the number under 'number of best final models to display' in the left sidebar layout.")
      Note_text_line4= paste0("Please click on one row in the above table to select the final model subset, we recommend to select the row with the lowest delta BIC('bic_delta').")
      Note_text_line5 = paste0("Then click on 'Step 3: Finalize Regression Model'to proceed to the next step.")
      paste(Note_text_line1,Note_text_line2,Note_text_line3,Note_text_line4,Note_text_line5,sep="\n")
    }) 
    
    data <- data_to_model$data_subset
    
    selected_variables = c(input$select_LR_end_points,selected_variables_for_regression())
    
    selected_data_1 <- data %>% select(selected_variables)
    selected_data_2 <- data %>% select(selected_variables_for_regression())
    column_idx= which(colnames(selected_data_1)==input$select_LR_end_points)
    selected_data_1 <- selected_data_1[!is.na(column_idx),]
    selected_data_1 <- na.roughfix(selected_data_1)
    
    if ("HU2" %in% colnames(selected_data_1)){
      selected_data_1$HU2 <- as.numeric(as.character(selected_data_1$HU2))
    }
    
    #print(nrow(selected_data_1))
    
    ## create a formula object
    
    myFormula = as.formula(
      paste(
        paste0('`',input$select_LR_end_points,'`'),
        paste(
          paste0('`',
                 colnames(selected_data_2),
                 '`'),
          collapse="+"),
        sep="~"))
    
    print(myFormula)
    #save(selected_data_1,file="./selected_data_for_LR.RData")
    
    lr_model_input$selected_data <- selected_data_1
    
    myModels <- regsubsets(myFormula, data=selected_data_1,nbest=input$nbest,nvmax=input$nvmax)
    # myModels <- regsubsets(LogTP~., data=selected_data_1,nbest=input$nbest,nvmax=input$nvmax)
    
    # save(myModels,file="./regsubsets_models.RData")
    
    #print(summary(myModels))
    
    output$model_selection_BIC <- renderPlot({
      print("inside displaying model selection bic plot now...")
      par(mar=c(1.0,0.5,0.5,0.5),cex.main=1.5,cex.lab=2.0)
      # plot(myModels,scale="bic")
      plot.regsubsets2(myModels,scale="bic")
    })
    
    output$model_selection_r2 <- renderPlot({
      print("inside displaying model selection r2 plot now...")
      par(mar=c(1.0,0.5,0.5,0.5),cex.main=1.5,cex.lab=2.0)
      # plot(myModels,scale="r2")
      plot.regsubsets2(myModels,scale="r2")
      
    })
    
    output$model_selection_Cp <- renderPlot({
      print("inside displaying model selection Cp plot now...")
      par(mar=c(1.0,0.5,0.5,0.5),cex.main=1.5,cex.lab=2.0)
      # plot(myModels,scale="Cp")
      plot.regsubsets2(myModels,scale="Cp")
      
    })
    
    output$model_selection_adj_r2 <- renderPlot({
      print("inside displaying model selection adjr2 plot now...")
      par(mar=c(1.0,0.5,0.5,0.5),cex.main=1.5,cex.lab=2.0)
      # plot(myModels,scale="adjr2")
      plot.regsubsets2(myModels,scale="adjr2")
    })
    
    regs_outmat <-summary(myModels)$outmat
    regs_bic <- summary(myModels)$bic
    regs_adjr2 <- summary(myModels)$adjr2
    regs_cp <- summary(myModels)$cp
    bic_delta <- regs_bic-min(regs_bic)
    
    regs_summary <- data.frame(regs_outmat,regs_adjr2,regs_cp,regs_bic,bic_delta)
    
    regs_summary_sorted <- regs_summary[order(regs_bic),]
    
    ## need to remove white space in the row names for 'regs_summary_sorted'
    
    names_removed_space <- gsub("\\s+","",row.names(regs_summary_sorted))
    
    row.names(regs_summary_sorted) <- names_removed_space
    
    output$regsModelSummary <- DT::renderDataTable({
      
      #print(input$nbest_models_to_display)
      n_cols = NCOL(regs_summary_sorted)
      myTable <- DT::datatable(regs_summary_sorted[1:input$nbest_models_to_display,],selection="single",options = list(
        scrollX = TRUE, #allow user to scroll wide tables horizontally
        dom ='t', # dom accepts character string "t" means only show table part
        stateSave = FALSE,
        pageLength = input$nvmax,
        autoWidth = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        columnDefs = list(list(className="dt-center",targets="_all"))
      )) %>%
        formatRound((n_cols-3):n_cols,digits=3)
      return(myTable)
      
    })
    
    lr_model_output$all_lr_models <- myModels
    lr_model_output$best_lr_models_idx <- row.names(regs_summary_sorted)
    
  }) #observeEvent end
  
  observeEvent(input$regsModelSummary_rows_selected,{
    print(str(input$regsModelSummary_rows_selected))
  })
  
  observeEvent(input$RunRegressionModel, {
    shinyjs::show("regression_model_final_panel")
    models_idx <-lr_model_output$best_lr_models_idx
    if (is.null(input$regsModelSummary_rows_selected)){
      alert_message ="Please click on the row to select your final regression model first"
      shinyalert(" ",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info1")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
      
    }else{
      print(input$regsModelSummary_rows_selected)
      selected_idx <- models_idx[input$regsModelSummary_rows_selected]
      #print(selected_idx)
      first_n = as.numeric(word(selected_idx,1,sep=fixed("(")))
      second_part = word(selected_idx,2,sep=fixed("("))
      n_in_2nd_part = as.numeric(word(second_part,1,sep=fixed(")")))
      #print(first_n)
      #print(n_in_2nd_part)
      selected_ID = first_n*n_in_2nd_part
      #print(selected_ID)
      
      my_LR_data <- lr_model_input$selected_data
      myModels <- lr_model_output$all_lr_models
      
      myFormula_final <- get_model_formula(selected_ID,myModels,input$select_LR_end_points)
      final_predictors <- get_model_predictors(selected_ID,myModels)
      
      lr_model_output$final_predictors <- final_predictors
      lr_model_output$final_formula <- myFormula_final
      
      the_final_LR_model <- lm(formula=myFormula_final,data=my_LR_data)
      
      #print(coef(the_final_LR_model))
      
      lr_model_output$final_LR_model <- the_final_LR_model
      
      #save(the_final_LR_model,file="./final_LR_model.RData")
      
      ## get predicted vs observed
      
      column_endpoints_idx_data= which(colnames(my_LR_data)==input$select_LR_end_points)
      LR_obs <- my_LR_data[,column_endpoints_idx_data]  
      LR_predictions = predict(the_final_LR_model, data =my_LR_data)
      
      lr_model_output$lr_predictions <- LR_predictions
      
      LR_tempDF = data.frame(yobs=LR_obs,ypred=LR_predictions)
      
      #print(colnames(LR_tempDF))
      
      output$RegressionModelFinalPlot <- renderUI({
        withSpinner(plotOutput("final_LR_fit_plot"),type=2)
      })
      
      output$RegressionModelBetaPlot <- renderUI({
        withSpinner(plotOutput("final_LR_beta_plot"),type=2)
      })
      
      output$RegressionModelSummaryTable <- renderUI({
        withSpinner(DT::dataTableOutput("LR_summary_table"),type=2)
      })
      
      output$final_LR_fit_plot <- renderPlot({
        print("inside displaying LR fit plot now...")
        myPlot <- ggplot(LR_tempDF,aes(yobs,ypred))+
          geom_point(size=0.5,color='blue') +
          geom_abline(slope=1,intercept=0,color="red") +
          scale_x_continuous(expand = c(0,0),limits = c(0,NA)) +
          scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
          xlab(paste0("Observed ",input$select_LR_end_points)) +
          ylab(paste0("Predicted ",input$select_LR_end_points)) +
          ggtitle("Observed vs Predicted") +
          theme_bw() +
          theme(text=element_text(size=15,face = "bold", color="blue"),
                plot.title=element_text(hjust=0.5,size=15))
        print(myPlot)
        
      })  ## renderPlot end
      
      mybeta <- lm.beta(the_final_LR_model)
      #print(mybeta)
      
      output$final_LR_beta_plot <- renderPlot({
        print("inside displaying LR beta plot now...")
        
        myGroup <- names(mybeta)
        tempDF <- data.frame(myGroup,myValue=as.numeric(mybeta))
        myPlot <- ggplot(tempDF,aes(x=myGroup,y=myValue))+
          geom_bar(stat="identity")+
          xlab("Predictor Variables ") +
          ylab("Standardized Coefficients") +
          ggtitle("Standardized Coefficients Plot") +
          theme_bw() +
          theme(text=element_text(size=15,face = "bold", color="blue"),
                plot.title=element_text(hjust=0.5,size=15),
                axis.text.x=element_text(angle=45, hjust=1),panel.grid.major.y=element_blank())
        print(myPlot)
        
      })  ##renderPlot end
      
      ## get the model coefficients table
      model_summary <- summary(the_final_LR_model)
      
      model_coef <- data.frame(model_summary$coefficients)
      vif_values <- data.frame(vif(the_final_LR_model))
      colnames(vif_values)[1] <- "VIF"
      beta_values <- data.frame(mybeta)
      colnames(beta_values)[1]<- "std.Beta"
      #tempDF <- data.frame(std.Beta=as.numeric(mybeta))
      #row.names(tempDF) <- names(mybeta)
      table_to_display <- merge.all(model_coef,beta_values,vif_values)
      #print(table_to_display)
      
      output$LR_summary_table <- DT::renderDataTable({
        
        
        myTable <- DT::datatable(table_to_display,options = list(
          scrollX = TRUE, #allow user to scroll wide tables horizontally
          dom ='t', # dom accepts character string "t" means only show table part
          stateSave = FALSE,
          pageLength = input$nvmax,
          autoWidth = FALSE,
          searching = FALSE,
          lengthChange = FALSE,
          columnDefs = list(list(className="dt-center",targets="_all"))
        )) %>%
          formatRound(c(1:3,5:6),digits=6) %>%
          formatStyle("VIF",color=JS(paste0("value > ",10,"? 'red' : ''"))) %>%
          formatStyle("VIF",backgroundColor=JS(paste0("value > ",10,"? 'black' : ''")))
        return(myTable)
        
      })  ##renderDataTable end multicollinearity 
      
      output$display_lr_summary_table_footnote <- renderUI({
        verbatimTextOutput("lr_summary_table_footnote")
      })
      
      output$lr_summary_table_footnote <- renderText({
        lr_footnote_text_line1= "The cells marked in red under the column 'VIF' indicate that multicollinearity was detected in the model,"
        lr_footnote_text_line2= "Please pick one or more associated predictor variables and remove them from the selected predictor variables list on the top."
        lr_footnote_text_line3= "Then rerun the processes from step 2 to 3."
        lr_footnote_text_line4= "If there is no cells marked in red, you may proceed to the next step by clicking on 'Step 4: Make Model Predictions' "
        paste(lr_footnote_text_line1,lr_footnote_text_line2,lr_footnote_text_line3,lr_footnote_text_line4,sep="\n")
      })
      
      ## gather model uncertainty metrics for the final regression model and display them in a table
      
      summary_metrics <- summary(the_final_LR_model)
      r2_report <- summary_metrics$r.squared  # Multiple R-squared
      adj_r2_report <- summary_metrics$adj.r.squared # Adjusted R-squared
      sigma_report <- summary_metrics$sigma # Residual standard error
      RMSE_report <- sqrt(mean(the_final_LR_model$residuals^2)) # Root mean square error
      mean_bias <-(sum(LR_predictions)-sum(LR_obs))/length(LR_obs) #mean bias
      SD <- sd(LR_predictions-LR_obs) #Standard deviation of the error
      nse <- vnse(LR_predictions, LR_obs,na.rm=T) # Nash-Sutcliffe efficiency coefficient (added by MJP)
      
      table_col_1 = c("Multiple R-squared","Adjusted R-squared","Residual standard error",
                      "Root Mean Squared Error","Mean Bias","Standard deviation of the error",
                      "Nash-Sutcliffe efficiency coefficient")
      table_col_2 = c(r2_report,adj_r2_report,sigma_report,RMSE_report,mean_bias,SD,nse)
      table_col_2 = formatC(table_col_2,digits=4,format="f")
      
      table_to_print <- data.frame(cbind(table_col_1,table_col_2))
      colnames(table_to_print) <- c("Model Uncertainty Metrics","Values")
      
      output$RegressionFinalModelPerformanceTable <- renderTable({
        table_to_print},type="html",bordered=TRUE,striped=TRUE,align="c")
      
    } ## if else loop end
    
  }) ## observeEvent end
  
  observeEvent(input$MakeRegressionModelPrediction, {
    
    shinyjs::hide("regression_correlation_analysis_panel")
    shinyjs::hide("regression_model_selection_output_panel")
    shinyjs::hide("regression_model_final_panel")
    shinyjs::show("regression_model_prediction_panel")
    
    # output$display_load_button <- renderUI({
    #   fileInput(inputId="loaded_newDataset", label="Load new dataset", multiple=FALSE,accept=c("text/csv",".csv",".RData"))
    # })
    
    output$look_back_step_1to3 <- renderUI({
      actionButton(inputId="look_back_steps", label="< Look back at the previous steps",style="color:blue;background-color:black")
    })
    
    output$LR_Break1 <- renderUI({
      div(
        h5("The following 2 sliders and toggle button are used for default datasets only",style="color:red;font-style:bold")
      )
    })
    
    output$select_LR_prediction_year_slider <- renderUI({
      sliderInput("LR_predict_year","Prediction year:",min=2002,max=2012,value=2007,step=5,round=0,sep="")
    })
    
    output$select_LR_prediction_month_slider <- renderUI({
      sliderInput("LR_predict_month","Prediction month:",min=2,max=12,value=7,step=5,round=0,sep="")
    })
    
    maxDepth_tooltip_text = paste0("Please select the lake maxDepth level at HUC8 scale, 'Shallow (3.6m)','Average (10.1m)'and 'Deep (24.4m)' represent 10%, 50% and 90% of LAGOS lakes maxDepth respectively.")
    output$select_LR_prediction_lake_maxDepth_button <- renderUI({
      tipify(radioButtons("LR_lake_maxDepth",label="Lake maxDepth",choices=c("Shallow","Average","Deep"),selected="Average",inline=FALSE),maxDepth_tooltip_text,
             placement="right",trigger="hover")
    })
    
    
    
    output$LR_Break2 <- renderUI({
      div(
        h5("!!!Make Sure to Click Correct Button Based on the Dataset Being Used!!!",style="color:red;font-style:bold")
      )
    })
    
    # MJP added
    output$LR_Break3 <- renderUI({
      div(
        h5("Default Map Buttons (!!!Only click if loaded default dataset in first section!!!):",style="color:red;font-style:bold")
      )
    })
    
    # original for LR (modified by MJP)
    output$display_map_button <- renderUI({
      #div(
      #h5("---------------------------------------",style="color:red;font-style:bold"),
      #h5("Make Sure You only click Correct Button Based on the Dataset Being Used",style="color:red;font-style:bold"),
      #h5("---------------------------------------",style="color:red;font-style:bold"),
      actionButton(inputId="showModelPredictionMap", label="Show LR prediction map for whole region (if using default dataset only!!!)",style="color:blue;background-color:black")
      #)
    })
    
    # original LR
    output$display_download_results_button <- renderUI({
      downloadButton(outputId="savePredictionResults", label="Download LR prediction results to csv (Default Data)",style="color:blue;background-color:black")
    })
    
    # original LR
    output$download_map_button <- renderUI({
      downloadButton(outputId="savePredictionMap", label="Save LR prediction map (Default Data)",style="color:blue;background-color:black")
    })
    
    
    output$LR_Break4a <- renderUI({
      div(id="LR_h5",
          h5("New Dataset Load Button:",style="color:red;font-style:bold")
      )
    })
    
    # MJP changes for LR
    output$display_lr_load_button <- renderUI({
      fileInput(inputId="uploaded_lr_newDataset", label="Load new dataset (if have new dataset only!!!)", multiple=FALSE,accept=c("text/csv",".csv",".RData"))
    })
    
    output$LR_Break4 <- renderUI({
      div(
        h5("New Dataset Map Buttons (!!!Only click if uploaded new dataset!!!):",style="color:red;font-style:bold")
      )
    })
    
    # mjp for LR user data
    output$display_map_button2 <- renderUI({
      actionButton(inputId="showModelPredictionMap2", label="Show LR prediction map for whole region (if using new dataset only!!!)",style="color:blue;background-color:black")
    })
    
    output$adjustRegressionMapColorLegend <- renderUI({
      sliderInput("LR_color_legend_range","Adjust map color legend here:",min=0,max=10,value=c(1,6),step=0.5,sep="")
    })
    
    # output$LR_Break5 <- renderUI({
    #   div(
    #     h5("Choose buttons for either default or new datasets:",style="color:red;font-style:bold")
    #   )
    # })
    
    
    # mjp LR
    output$display_download_results_button2 <- renderUI({
      downloadButton(outputId="savePredictionResults2", label="Download LR prediction results to csv (New User Data)",style="color:blue;background-color:black")
    })
    
    
    
    # mjp LR
    output$download_map_button2 <- renderUI({
      downloadButton(outputId="savePredictionMap2", label="Save LR prediction map (New User Data)",style="color:blue;background-color:black")
    })
    
    cutoff_tooltip_text = paste0("The endpoint concentration criteria is used to display the prediction map in two different colors (above or below this criteria) accordingly."," Default is 50.")
    output$display_userInput_LR_cutoff <- renderUI({
      tipify(numericInput("LR_cutoff_value",label ="Endpoint concentration criteria",50,min=1,max=300,step=1),cutoff_tooltip_text,placement="right",trigger="hover")
    })
    
    output$LR_Break6 <- renderUI({
      div(
        h5("!!!Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold")
      )
    })
    
    output$LR_Break7 <- renderUI({
      div(
        h5("Only use the next two buttons if using default dataset:",style="color:red;font-style:bold")
      )
    })
    
    output$display_LR_map_in_two_colors_button <- renderUI({
      #div(
      # h5("---------------------------------------",style="color:red;font-style:bold"),
      #h5("Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold"),
      #h5("---------------------------------------",style="color:red;font-style:bold"),
      #h5("Only use these two buttons if using default dataset",style="color:red;font-style:bold"),
      actionButton(inputId="showLRModelPredictionMapTwoColors", label="Show prediction map in bicolor (Default Data)",style="color:blue;background-color:black")
      #)
    })
=======
     myTable <- DT::datatable(table_to_display,options = list(
       scrollX = TRUE, #allow user to scroll wide tables horizontally
       dom ='t', # dom accepts character string "t" means only show table part
       stateSave = FALSE,
       pageLength = input$nvmax,
       autoWidth = FALSE,
       searching = FALSE,
       lengthChange = FALSE,
       columnDefs = list(list(className="dt-center",targets="_all"))
     )) %>%
     formatRound(c(1:3,5:6),digits=6) %>%
       formatStyle("VIF",color=JS(paste0("value > ",10,"? 'red' : ''"))) %>%
       formatStyle("VIF",backgroundColor=JS(paste0("value > ",10,"? 'black' : ''")))
     return(myTable)
     
   })  ##renderDataTable end multicollinearity 
   
   output$display_lr_summary_table_footnote <- renderUI({
     verbatimTextOutput("lr_summary_table_footnote")
   })
   
   output$lr_summary_table_footnote <- renderText({
     lr_footnote_text_line1= "The cells marked in red under the column 'VIF' indicate that multicollinearity was detected in the model,"
     lr_footnote_text_line2= "Please pick one or more associated predictor variables and remove them from the selected predictor variables list on the top."
     lr_footnote_text_line3= "Then rerun the processes from step 2 to 3."
     lr_footnote_text_line4= "If there is no cells marked in red, you may proceed to the next step by clicking on 'Step 4: Make Model Predictions' "
     paste(lr_footnote_text_line1,lr_footnote_text_line2,lr_footnote_text_line3,lr_footnote_text_line4,sep="\n")
   })
   
   ## gather model uncertainty metrics for the final regression model and display them in a table
   
   summary_metrics <- summary(the_final_LR_model)
   r2_report <- summary_metrics$r.squared  # Multiple R-squared
   adj_r2_report <- summary_metrics$adj.r.squared # Adjusted R-squared
   sigma_report <- summary_metrics$sigma # Residual standard error
   RMSE_report <- sqrt(mean(the_final_LR_model$residuals^2)) # Root mean square error
   mean_bias <-(sum(LR_predictions)-sum(LR_obs))/length(LR_obs) #mean bias
   SD <- sd(LR_predictions-LR_obs) #Standard deviation of the error
   nse <- vnse(LR_predictions, LR_obs,na.rm=T) # Nash-Sutcliffe efficiency coefficient (added by MJP)
   
   table_col_1 = c("Multiple R-squared","Adjusted R-squared","Residual standard error",
                    "Root Mean Squared Error","Mean Bias","Standard deviation of the error",
                   "Nash-Sutcliffe efficiency coefficient")
   table_col_2 = c(r2_report,adj_r2_report,sigma_report,RMSE_report,mean_bias,SD,nse)
   table_col_2 = formatC(table_col_2,digits=4,format="f")
   
   table_to_print <- data.frame(cbind(table_col_1,table_col_2))
   colnames(table_to_print) <- c("Model Uncertainty Metrics","Values")
   
   output$RegressionFinalModelPerformanceTable <- renderTable({
     table_to_print},type="html",bordered=TRUE,striped=TRUE,align="c")
   
   } ## if else loop end
   
 }) ## observeEvent end
 
 observeEvent(input$MakeRegressionModelPrediction, {
   
   shinyjs::hide("regression_correlation_analysis_panel")
   shinyjs::hide("regression_model_selection_output_panel")
   shinyjs::hide("regression_model_final_panel")
   shinyjs::show("regression_model_prediction_panel")
   
   # output$display_load_button <- renderUI({
   #   fileInput(inputId="loaded_newDataset", label="Load new dataset", multiple=FALSE,accept=c("text/csv",".csv",".RData"))
   # })
   
   output$look_back_step_1to3 <- renderUI({
     actionButton(inputId="look_back_steps", label="< Look back at the previous steps",style="color:blue;background-color:black")
   })
   
   output$LR_Break1 <- renderUI({
     div(
       h5("The following 2 sliders and toggle button are used for default datasets only",style="color:red;font-style:bold")
     )
   })
   
   output$select_LR_prediction_year_slider <- renderUI({
     sliderInput("LR_predict_year","Prediction year:",min=2002,max=2012,value=2007,step=5,round=0,sep="")
   })
   
   output$select_LR_prediction_month_slider <- renderUI({
     sliderInput("LR_predict_month","Prediction month:",min=2,max=12,value=7,step=5,round=0,sep="")
   })
   
   maxDepth_tooltip_text = paste0("Please select the lake maxDepth level at HUC8 scale, 'Shallow (3.6m)','Average (10.1m)'and 'Deep (24.4m)' represent 10%, 50% and 90% of LAGOS lakes maxDepth respectively.")
   output$select_LR_prediction_lake_maxDepth_button <- renderUI({
     tipify(radioButtons("LR_lake_maxDepth",label="Lake maxDepth",choices=c("Shallow","Average","Deep"),selected="Average",inline=FALSE),maxDepth_tooltip_text,
            placement="right",trigger="hover")
   })
   
 
   
   output$LR_Break2 <- renderUI({
     div(
       h5("!!!Make Sure to Click Correct Button Based on the Dataset Being Used!!!",style="color:red;font-style:bold")
     )
   })
   
   # MJP added
   output$LR_Break3 <- renderUI({
     div(
       h5("Default Map Buttons (!!!Only click if loaded default dataset in first section!!!):",style="color:red;font-style:bold")
     )
   })
   
   # original for LR (modified by MJP)
   output$display_map_button <- renderUI({
     #div(
       #h5("---------------------------------------",style="color:red;font-style:bold"),
       #h5("Make Sure You only click Correct Button Based on the Dataset Being Used",style="color:red;font-style:bold"),
       #h5("---------------------------------------",style="color:red;font-style:bold"),
       actionButton(inputId="showModelPredictionMap", label="Show LR prediction map for whole region (if using default dataset only!!!)",style="color:blue;background-color:black")
     #)
   })
   
   # original LR
   output$display_download_results_button <- renderUI({
     downloadButton(outputId="savePredictionResults", label="Download LR prediction results to csv (Default Data)",style="color:blue;background-color:black")
   })
   
   # original LR
   output$download_map_button <- renderUI({
     downloadButton(outputId="savePredictionMap", label="Save LR prediction map (Default Data)",style="color:blue;background-color:black")
   })
   
   
   output$LR_Break4a <- renderUI({
     div(id="LR_h5",
       h5("New Dataset Load Button:",style="color:red;font-style:bold")
     )
   })
   
   # MJP changes for LR
   output$display_lr_load_button <- renderUI({
     fileInput(inputId="uploaded_lr_newDataset", label="Load new dataset (if have new dataset only!!!)", multiple=FALSE,accept=c("text/csv",".csv",".RData"))
   })
   
   output$LR_Break4 <- renderUI({
     div(
       h5("New Dataset Map Buttons (!!!Only click if uploaded new dataset!!!):",style="color:red;font-style:bold")
     )
   })
   
   # mjp for LR user data
   output$display_map_button2 <- renderUI({
     actionButton(inputId="showModelPredictionMap2", label="Show LR prediction map for whole region (if using new dataset only!!!)",style="color:blue;background-color:black")
   })
   
   output$adjustRegressionMapColorLegend <- renderUI({
     sliderInput("LR_color_legend_range","Adjust map color legend here:",min=0,max=10,value=c(1,6),step=0.5,sep="")
   })
   
   # output$LR_Break5 <- renderUI({
   #   div(
   #     h5("Choose buttons for either default or new datasets:",style="color:red;font-style:bold")
   #   )
   # })
   
   
   # mjp LR
   output$display_download_results_button2 <- renderUI({
     downloadButton(outputId="savePredictionResults2", label="Download LR prediction results to csv (New User Data)",style="color:blue;background-color:black")
   })
   
   
   
   # mjp LR
   output$download_map_button2 <- renderUI({
     downloadButton(outputId="savePredictionMap2", label="Save LR prediction map (New User Data)",style="color:blue;background-color:black")
   })
   
   cutoff_tooltip_text = paste0("The endpoint concentration criteria is used to display the prediction map in two different colors (above or below this criteria) accordingly."," Default is 50.")
   output$display_userInput_LR_cutoff <- renderUI({
     tipify(numericInput("LR_cutoff_value",label ="Endpoint concentration criteria",50,min=1,max=300,step=1),cutoff_tooltip_text,placement="right",trigger="hover")
   })
   
   output$LR_Break6 <- renderUI({
     div(
       h5("!!!Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold")
     )
   })
   
   output$LR_Break7 <- renderUI({
     div(
       h5("Only use the next two buttons if using default dataset:",style="color:red;font-style:bold")
     )
   })
   
   output$display_LR_map_in_two_colors_button <- renderUI({
     #div(
      # h5("---------------------------------------",style="color:red;font-style:bold"),
       #h5("Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold"),
       #h5("---------------------------------------",style="color:red;font-style:bold"),
       #h5("Only use these two buttons if using default dataset",style="color:red;font-style:bold"),
     actionButton(inputId="showLRModelPredictionMapTwoColors", label="Show prediction map in bicolor (Default Data)",style="color:blue;background-color:black")
     #)
     })
   
   output$download_LR_map_in_two_colors_button <- renderUI({
     downloadButton(outputId="saveLRPredictionMapTwoColors", label="Save prediction map in bicolor (Default Data)",style="color:blue;background-color:black")
   })
  
   output$LR_Break8 <- renderUI({
     div(
       h5("Only use the next two buttons if uploaded new dataset above:",style="color:red;font-style:bold")
     )
   })
   
   # MJP
   output$display_LR_map_in_two_colors_button2 <- renderUI({
     #div(
       #h5("Only use these two buttons if uploaded new dataset above",style="color:red;font-style:bold"),
     actionButton(inputId="showLRModelPredictionMapTwoColors2", label="Show prediction map in bicolor (New Data)",style="color:blue;background-color:black")
     #)
     })
   
   # MJP
   output$download_LR_map_in_two_colors_button2 <- renderUI({
     downloadButton(outputId="saveLRPredictionMapTwoColors2", label="Save prediction map in bicolor (New Data)",style="color:blue;background-color:black")
   })
   
   ##YD added
   print(paste0("inside MakeRegressionModelPrediction now, check logic:",is.null(input$uploaded_dataset)))
   
   if (!is.null(input$uploaded_dataset)){
     delay(1000,shinyjs::hide("LR_predict_year"))
     delay(1000,shinyjs::hide("LR_predict_month"))
     delay(1000,shinyjs::hide("LR_lake_maxDepth"))
     delay(1200,shinyjs::hide("showModelPredictionMap"))
     delay(1200,shinyjs::hide("savePredictionResults"))
     delay(1200,shinyjs::hide("savePredictionMap"))
     delay(1500,shinyjs::hide("showLRModelPredictionMapTwoColors"))
     delay(1500,shinyjs::hide("saveLRPredictionMapTwoColors"))
   }else{
     delay(1000,shinyjs::hide("LR_h5"))
     delay(1000,shinyjs::hide("uploaded_lr_newDataset"))
     delay(1200,shinyjs::hide("showModelPredictionMap2"))
     delay(1200,shinyjs::hide("savePredictionResults2"))
     delay(1200,shinyjs::hide("savePredictionMap2"))
     delay(1500,shinyjs::hide("showLRModelPredictionMapTwoColors2"))
     delay(1500,shinyjs::hide("saveLRPredictionMapTwoColors2"))
   }
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
    
    output$download_LR_map_in_two_colors_button <- renderUI({
      downloadButton(outputId="saveLRPredictionMapTwoColors", label="Save prediction map in bicolor (Default Data)",style="color:blue;background-color:black")
    })
    
    output$LR_Break8 <- renderUI({
      div(
        h5("Only use the next two buttons if uploaded new dataset above:",style="color:red;font-style:bold")
      )
    })
    
<<<<<<< HEAD
    # MJP
    output$display_LR_map_in_two_colors_button2 <- renderUI({
      #div(
      #h5("Only use these two buttons if uploaded new dataset above",style="color:red;font-style:bold"),
      actionButton(inputId="showLRModelPredictionMapTwoColors2", label="Show prediction map in bicolor (New Data)",style="color:blue;background-color:black")
      #)
    })
    
    # MJP
    output$download_LR_map_in_two_colors_button2 <- renderUI({
      downloadButton(outputId="saveLRPredictionMapTwoColors2", label="Save prediction map in bicolor (New Data)",style="color:blue;background-color:black")
    })
    
    ##YD added
    print(paste0("inside MakeRegressionModelPrediction now, check logic:",is.null(input$uploaded_dataset)))
    
    if (!is.null(input$uploaded_dataset)){
      delay(1000,shinyjs::hide("LR_predict_year"))
      delay(1000,shinyjs::hide("LR_predict_month"))
      delay(1000,shinyjs::hide("LR_lake_maxDepth"))
      delay(1200,shinyjs::hide("showModelPredictionMap"))
      delay(1200,shinyjs::hide("savePredictionResults"))
      delay(1200,shinyjs::hide("savePredictionMap"))
      delay(1500,shinyjs::hide("showLRModelPredictionMapTwoColors"))
      delay(1500,shinyjs::hide("saveLRPredictionMapTwoColors"))
    }else{
      delay(1000,shinyjs::hide("LR_h5"))
      delay(1000,shinyjs::hide("uploaded_lr_newDataset"))
      delay(1200,shinyjs::hide("showModelPredictionMap2"))
      delay(1200,shinyjs::hide("savePredictionResults2"))
      delay(1200,shinyjs::hide("savePredictionMap2"))
      delay(1500,shinyjs::hide("showLRModelPredictionMapTwoColors2"))
      delay(1500,shinyjs::hide("saveLRPredictionMapTwoColors2"))
    }
    
  })
  
  observeEvent(input$look_back_steps, {
    
    shinyjs::show("regression_correlation_analysis_panel")
    shinyjs::show("regression_model_selection_output_panel")
    shinyjs::show("regression_model_final_panel")
    shinyjs::show("regression_model_prediction_panel")
  })
  
  #uploaded_new_data <- eventReactive(input$loaded_newDataset,{
  #myData<-import_raw_data(input$uploaded_dataset$datapath,"csv",has_header=TRUE)
  # e = new.env()
  # name <- load(input$loaded_newDataset$datapath,envir = e)
  # newData <- e[[name]]
  # #print(nrow(newData))
  # return(newData)
  #})
  
  #observe(uploaded_new_data())
  
  # LR Map Original
  observeEvent(input$showModelPredictionMap, { #observeEvent1 LR
    
    output$ShowRegressionModelPredictionMap <- renderUI({
      
      withSpinner(plotOutput("lr_prediction_map",width="800px",height="600px"),type=2)
    })
    
    load("./Data/all_HU8_shapes.RData")
    my_fileName <- paste("./Data/HUC8",isolate(input$LR_predict_year),isolate(input$LR_predict_month),isolate(input$LR_lake_maxDepth),"MaxDepth.RData",sep="_")
    print(my_fileName)
    if (file.exists(my_fileName)){
      load(my_fileName)
      confirm_message = paste0("a new dataset: ",my_fileName," is loaded to make predictions.")
      shinyalert("",confirm_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "confirm1")  
    }else{
      no_file_message = paste0("new dataset: ",my_fileName," does not exist in the data folder.")
      shinyalert("Alert",no_file_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "noFile1")  
    } 
    HUC8_newData = na.roughfix(HUC8_newData)
    
    LR_predictions <- predict(lr_model_output$final_LR_model, newdata=HUC8_newData)
    pred2 = as.data.frame(LR_predictions)
    names(pred2)[1]="predicted"
    ## convert LogTP and LogTN into TP and TN
    if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="LogTN"){
      pred2$converted=exp(pred2$predicted)
      names(pred2)[2]="HU8"
    }else{
      names(pred2)[1]="HU8"
    }
    pred2$huc8 = formatC(as.numeric(as.character(HUC8_newData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
    #print(length(pred2))
    to_download$LR_prediction =pred2
    
    if (input$select_LR_end_points=="LogTP"){
      names(to_download$LR_prediction)[1:2]=c("LogTP","TP")
      name_in_plot_title = "TP"
    }else if (input$select_LR_end_points=="LogTN"){
      names(to_download$LR_prediction)[1:2]=c("LogTN","TN")
      name_in_plot_title = "TN"
    }else{
      names(to_download$LR_prediction)[1]=input$select_LR_end_points
      name_in_plot_title = input$select_LR_end_points
    }
    myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
    
    #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) # YD commented this line out
    #print(length(myjoinedPred_to_map))
    myjoinedPred_to_map <- st_as_sf(myjoinedPred)
    max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8)))
    min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
    updateSliderInput(session,"LR_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
    #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8) ## domain=myjoinedPred_to_map$HU8 if you want color legend to change
    
    rm(joined_HU8,HUC8_newData,myjoinedPred)
    ##YD added
    myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
    # output$lr_prediction_map <- renderLeaflet ({
    #   print("inside display multi-linear regression prediction map now")
    #   my_color_breaks = seq(from=input$LR_color_legend_range[1],to=input$LR_color_legend_range[2],by=0.5)
    #   print(my_color_breaks)
    #   color_HUC8 = colorNumeric(palette=input$select_color,my_color_breaks)
    #   #print(color_HUC8)
    #   LR_map <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
    #     setView(lng=-80.8,lat=40,zoom=4) %>%
    #     addPolygons(data=myjoinedPred_to_map,
    #                 fillColor = ~color_HUC8(HU8),  
    #                 color="red",
    #                 smoothFactor = 0.5,
    #                 fillOpacity = 1,
    #                 weight=1,
    #                 popup=paste("HUC8 ID: ",myjoinedPred_to_map$huc8,"<br>",
    #                             paste0(input$select_LR_end_points,": "),formatC(myjoinedPred_to_map$HU8,format='f',digits=3))
    #     ) %>% # addPolygons end
    #     addLegend(pal=color_HUC8,values=my_color_breaks,position='topright',title=input$select_LR_end_points,opacity=1) #myjoinedPred_to_map$HU8
    # }) # renderLeaflet end
    
    output$lr_prediction_map <- renderPlot ({
      req(input$LR_color_legend_range)
      print("inside rendering the multi-linear regression prediction HUC8 map now...")
      
      to_download$LR_map <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
        scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$LR_color_legend_range[1],input$LR_color_legend_range[2]))+
        theme(panel.background = element_blank())+ # remove grey background
        theme(plot.title=element_text(hjust=0.5))+ # put the title at center
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$LR_map)
      
    })
    
    # rm(mjoinedPred_to_map,LR_map)
    
  }) #observeEvent1 end
  
  #---------------------------------------------------------------------------------
  uploaded_lr_new_data <- eventReactive(input$uploaded_lr_newDataset,{ # mjp2 should be loaded was load_rf_newDataset
    newUserData<-import_raw_data(input$uploaded_lr_newDataset$datapath,"csv",has_header=TRUE)
    
    #e = new.env()
    #name <- load(input$loaded_rf_newDataset$datapath,envir = e)
    #newUserData <- e[[name]]
    #print(nrow(newData))
    return(newUserData)
  })
  
  observe(uploaded_lr_new_data())
  
  observeEvent(input$dataInfo,{
    shinyjs::runjs("swal.close();")
  })
  #---------------------------------------------------------------------------------
  
  # LR Map New Dataset
  observeEvent(input$showModelPredictionMap2, { #observeEvent2
    
    output$ShowRegressionModelPredictionMap2 <- renderUI({
      
      withSpinner(plotOutput("lr_prediction_map2",width="800px",height="600px"),type=2)
    })
    
    load("./Data/all_HU8_shapes.RData")
    
    # my_fileName <- paste("./Data/HUC8",isolate(input$LR_predict_year),isolate(input$LR_predict_month),isolate(input$LR_lake_maxDepth),"MaxDepth.RData",sep="_")
    # print(my_fileName)
    # if (file.exists(my_fileName)){
    #   load(my_fileName)
    #   confirm_message = paste0("a new dataset: ",my_fileName," is loaded to make predictions.")
    #   shinyalert("",confirm_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
    #              confirmButtonText="OK",inputId = "confirm1")  
    # }else{
    #   no_file_message = paste0("new dataset: ",my_fileName," does not exist in the data folder.")
    #   shinyalert("Alert",no_file_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
    #              confirmButtonText="OK",inputId = "noFile1")  
    # } 
    
    newUserData <- uploaded_lr_new_data() 
    
    HUC8_newData = na.roughfix(newUserData)
    
    LR_predictions <- predict(lr_model_output$final_LR_model, newdata=HUC8_newData)
    pred2 = as.data.frame(LR_predictions)
    names(pred2)[1]="predicted"
    ## convert LogTP and LogTN into TP and TN
    if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="LogTN"){
      pred2$converted=exp(pred2$predicted)
      names(pred2)[2]="HU8"
    }else{
      names(pred2)[1]="HU8"
    }
    pred2$huc8 = formatC(as.numeric(as.character(HUC8_newData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
    #print(length(pred2))
    #save(pred2,file="./pred2_check.RData")
    
    to_download$LR_prediction2 =pred2
    
    if (input$select_LR_end_points=="LogTP"){
      names(to_download$LR_prediction2)[1:2]=c("LogTP","TP")
      name_in_plot_title = "TP"
    }else if (input$select_LR_end_points=="LogTN"){
      names(to_download$LR_prediction2)[1:2]=c("LogTN","TN")
      name_in_plot_title = "TN"
    }else{
      names(to_download$LR_prediction2)[1]=input$select_LR_end_points
      name_in_plot_title = input$select_LR_end_points
    }
    myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
    
    #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) #YD commented this line out
    #print(length(myjoinedPred_to_map))
    myjoinedPred_to_map <- st_as_sf(myjoinedPred)
    #YD changed
    max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8)))
    min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
    updateSliderInput(session,"LR_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
    #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8) ## domain=myjoinedPred_to_map$HU8 if you want color legend to change
    
    rm(joined_HU8,HUC8_newData,myjoinedPred)
    ##YD added
    myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
    
    output$lr_prediction_map2 <- renderPlot ({
      req(input$LR_color_legend_range)
      print("inside rendering the multi-linear regression prediction HUC8 map now...")
      
      to_download$LR_map2 <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
        scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$LR_color_legend_range[1],input$LR_color_legend_range[2]))+
        theme(panel.background = element_blank())+ # remove grey background
        theme(plot.title=element_text(hjust=0.5))+ # put the title at center
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$LR_map2)
      
    })
    
    # rm(mjoinedPred_to_map,LR_map)
    
  }) #observeEvent2 end
  
  # Original Default
  output$savePredictionResults <- downloadHandler(
    filename = function(){paste(input$select_LR_end_points,"_linear_regression_model_prediction_results_",Sys.Date(),".csv",sep="")},
    content = function(file){
      write.csv(to_download$LR_prediction,file,row.names=FALSE)
    }
  )
  output$savePredictionMap <- downloadHandler(
    filename = function(){paste("linear_regression_model_prediction_map_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$LR_map,dpi=300,width=12,height=10)
    }
  )
  
  # MJP added for New User Data
  output$savePredictionResults2 <- downloadHandler(
    filename = function(){paste(input$select_LR_end_points,"_linear_regression_model_prediction_results_",Sys.Date(),".csv",sep="")},
    content = function(file){
      write.csv(to_download$LR_prediction2,file,row.names=FALSE)
    }
  )
  
  #mjp added
  output$savePredictionMap2 <- downloadHandler(
    filename = function(){paste("linear_regression_model_prediction_map_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$LR_map2,dpi=300,width=12,height=10)
    }
  )
  
  #####################################  added to display LR prediction map in bicolor ######################
  
  # Default 
  observeEvent(input$showLRModelPredictionMapTwoColors, {
    
    output$ShowRegressionModelPredictionMapBiColor <- renderUI({
      withSpinner(plotOutput("regression_prediction_map_bicolor",width="800px",height="600px"),type=2)
    })
    
    load("./Data/all_HU8_shapes.RData")
    
    if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="TP"){
      myjoinedPred = merge(joined_HU8,to_download$LR_prediction,by="huc8",duplicateGeoms=TRUE)
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) #YD commented this line out
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      ##YD added
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
      #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
      name_in_plot_title = "TP"
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$LR_cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$LR_cutoff_value),]
    }else if (input$select_LR_end_points=="LogTN" | input$select_LR_end_points=="TN"){
      myjoinedPred = merge(joined_HU8,to_download$LR_prediction,by="huc8",duplicateGeoms=TRUE)
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) #YD commented this line out
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
      name_in_plot_title = "TN"
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$LR_cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$LR_cutoff_value),]
    }
    
    rm(joined_HU8,myjoinedPred)
    
    output$regression_prediction_map_bicolor <- renderPlot ({
      
      print("inside rendering the regression prediction HUC8 bicolor map now...")
      
      to_download$LR_bicolor_map <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        theme(panel.background = element_blank())+ # remove grey background
        labs(fill=name_in_plot_title)+
        scale_fill_manual(values=c("blue","red"))+
        theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
        theme(legend.title=element_text(size=14))+
        theme(legend.text=element_text(size=14))+
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$LR_bicolor_map)
    })
    
  }) # observeEvent end
  
  output$saveLRPredictionMapTwoColors <- downloadHandler(
    filename = function(){paste("regression_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$LR_bicolor_map,dpi=300,width=12,height=10)
    }
  )
  
  #-----------------------------------------------------------------------------------------
  # MJP New User Map
  observeEvent(input$showLRModelPredictionMapTwoColors2, {
    
    output$ShowRegressionModelPredictionMapBiColor2 <- renderUI({
      withSpinner(plotOutput("regression_prediction_map_bicolor2",width="800px",height="600px"),type=2)
    })
    
    load("./Data/all_HU8_shapes.RData")
    
    if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="TP"){
      testlr = to_download$LR_prediction2
      #save(testlr,file="./testlr_check.RData")
      myjoinedPred = merge(joined_HU8,to_download$LR_prediction2,by="huc8",duplicateGeoms=TRUE)
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) #YD commented this line out
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
      #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
      name_in_plot_title = "TP"
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$LR_cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$LR_cutoff_value),]
    }else if (input$select_LR_end_points=="LogTN" | input$select_LR_end_points=="TN"){
      myjoinedPred = merge(joined_HU8,to_download$LR_prediction2,by="huc8",duplicateGeoms=TRUE)
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) #YD commented this line out
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
      name_in_plot_title = "TN"
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$LR_cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$LR_cutoff_value),]
    }
    
    rm(joined_HU8,myjoinedPred)
    
    output$regression_prediction_map_bicolor2 <- renderPlot ({
      
      print("inside rendering the regression prediction HUC8 bicolor map now...")
      
      to_download$LR_bicolor_map2 <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        theme(panel.background = element_blank())+ # remove grey background
        labs(fill=name_in_plot_title)+
        scale_fill_manual(values=c("blue","red"))+
        theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
        theme(legend.title=element_text(size=14))+
        theme(legend.text=element_text(size=14))+
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$LR_bicolor_map2)
    })
    
  }) # observeEvent end
  
  output$saveLRPredictionMapTwoColors2 <- downloadHandler(
    filename = function(){paste("regression_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$LR_bicolor_map2,dpi=300,width=12,height=10)
    }
  )
  
  #####################################  model 2:  run random forest model  #######################################
  
  
  observeEvent(selected_variables_for_regression(),{
    best_mtry = floor(sqrt(length(selected_variables_for_regression())))
    updateNumericInput(session,"mtry",value=best_mtry)
  })
=======
   load("./Data/all_HU8_shapes.RData")
   my_fileName <- paste("./Data/HUC8",isolate(input$LR_predict_year),isolate(input$LR_predict_month),isolate(input$LR_lake_maxDepth),"MaxDepth.RData",sep="_")
   print(my_fileName)
   if (file.exists(my_fileName)){
     load(my_fileName)
     confirm_message = paste0("a new dataset: ",my_fileName," is loaded to make predictions.")
     shinyalert("",confirm_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                confirmButtonText="OK",inputId = "confirm1")  
   }else{
     no_file_message = paste0("new dataset: ",my_fileName," does not exist in the data folder.")
     shinyalert("Alert",no_file_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                confirmButtonText="OK",inputId = "noFile1")  
   } 
   HUC8_newData = na.roughfix(HUC8_newData)
   
   LR_predictions <- predict(lr_model_output$final_LR_model, newdata=HUC8_newData)
   pred2 = as.data.frame(LR_predictions)
   names(pred2)[1]="predicted"
   ## convert LogTP and LogTN into TP and TN
   if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="LogTN"){
     pred2$converted=exp(pred2$predicted)
     names(pred2)[2]="HU8"
   }else{
     names(pred2)[1]="HU8"
   }
   pred2$huc8 = formatC(as.numeric(as.character(HUC8_newData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
   #print(length(pred2))
   to_download$LR_prediction =pred2
   
   if (input$select_LR_end_points=="LogTP"){
     names(to_download$LR_prediction)[1:2]=c("LogTP","TP")
     name_in_plot_title = "TP"
   }else if (input$select_LR_end_points=="LogTN"){
     names(to_download$LR_prediction)[1:2]=c("LogTN","TN")
     name_in_plot_title = "TN"
   }else{
     names(to_download$LR_prediction)[1]=input$select_LR_end_points
     name_in_plot_title = input$select_LR_end_points
   }
   myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
   
   #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) # YD commented this line out
   #print(length(myjoinedPred_to_map))
   myjoinedPred_to_map <- st_as_sf(myjoinedPred)
   max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8)))
   min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
   updateSliderInput(session,"LR_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
   #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8) ## domain=myjoinedPred_to_map$HU8 if you want color legend to change
   
   rm(joined_HU8,HUC8_newData,myjoinedPred)
   ##YD added
   myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
   # output$lr_prediction_map <- renderLeaflet ({
   #   print("inside display multi-linear regression prediction map now")
   #   my_color_breaks = seq(from=input$LR_color_legend_range[1],to=input$LR_color_legend_range[2],by=0.5)
   #   print(my_color_breaks)
   #   color_HUC8 = colorNumeric(palette=input$select_color,my_color_breaks)
   #   #print(color_HUC8)
   #   LR_map <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
   #     setView(lng=-80.8,lat=40,zoom=4) %>%
   #     addPolygons(data=myjoinedPred_to_map,
   #                 fillColor = ~color_HUC8(HU8),  
   #                 color="red",
   #                 smoothFactor = 0.5,
   #                 fillOpacity = 1,
   #                 weight=1,
   #                 popup=paste("HUC8 ID: ",myjoinedPred_to_map$huc8,"<br>",
   #                             paste0(input$select_LR_end_points,": "),formatC(myjoinedPred_to_map$HU8,format='f',digits=3))
   #     ) %>% # addPolygons end
   #     addLegend(pal=color_HUC8,values=my_color_breaks,position='topright',title=input$select_LR_end_points,opacity=1) #myjoinedPred_to_map$HU8
   # }) # renderLeaflet end
   
   output$lr_prediction_map <- renderPlot ({
     req(input$LR_color_legend_range)
     print("inside rendering the multi-linear regression prediction HUC8 map now...")
     
     to_download$LR_map <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
       scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$LR_color_legend_range[1],input$LR_color_legend_range[2]))+
       theme(panel.background = element_blank())+ # remove grey background
       theme(plot.title=element_text(hjust=0.5))+ # put the title at center
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$LR_map)
     
   })
   
  # rm(mjoinedPred_to_map,LR_map)
  
 }) #observeEvent1 end
 
 #---------------------------------------------------------------------------------
 uploaded_lr_new_data <- eventReactive(input$uploaded_lr_newDataset,{ # mjp2 should be loaded was load_rf_newDataset
   newUserData<-import_raw_data(input$uploaded_lr_newDataset$datapath,"csv",has_header=TRUE)
   
   #e = new.env()
   #name <- load(input$loaded_rf_newDataset$datapath,envir = e)
   #newUserData <- e[[name]]
   #print(nrow(newData))
   return(newUserData)
 })
 
 observe(uploaded_lr_new_data())
 
 observeEvent(input$dataInfo,{
   shinyjs::runjs("swal.close();")
 })
 #---------------------------------------------------------------------------------
 
 # LR Map New Dataset
 observeEvent(input$showModelPredictionMap2, { #observeEvent2
   
   output$ShowRegressionModelPredictionMap2 <- renderUI({
     
     withSpinner(plotOutput("lr_prediction_map2",width="800px",height="600px"),type=2)
   })
   
   load("./Data/all_HU8_shapes.RData")
   
   # my_fileName <- paste("./Data/HUC8",isolate(input$LR_predict_year),isolate(input$LR_predict_month),isolate(input$LR_lake_maxDepth),"MaxDepth.RData",sep="_")
   # print(my_fileName)
   # if (file.exists(my_fileName)){
   #   load(my_fileName)
   #   confirm_message = paste0("a new dataset: ",my_fileName," is loaded to make predictions.")
   #   shinyalert("",confirm_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
   #              confirmButtonText="OK",inputId = "confirm1")  
   # }else{
   #   no_file_message = paste0("new dataset: ",my_fileName," does not exist in the data folder.")
   #   shinyalert("Alert",no_file_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
   #              confirmButtonText="OK",inputId = "noFile1")  
   # } 
   
   newUserData <- uploaded_lr_new_data() 
   
   HUC8_newData = na.roughfix(newUserData)
   
   LR_predictions <- predict(lr_model_output$final_LR_model, newdata=HUC8_newData)
   pred2 = as.data.frame(LR_predictions)
   names(pred2)[1]="predicted"
   ## convert LogTP and LogTN into TP and TN
   if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="LogTN"){
     pred2$converted=exp(pred2$predicted)
     names(pred2)[2]="HU8"
   }else{
     names(pred2)[1]="HU8"
   }
   pred2$huc8 = formatC(as.numeric(as.character(HUC8_newData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
   #print(length(pred2))
   #save(pred2,file="./pred2_check.RData")
   
   to_download$LR_prediction2 =pred2
   
   if (input$select_LR_end_points=="LogTP"){
     names(to_download$LR_prediction2)[1:2]=c("LogTP","TP")
     name_in_plot_title = "TP"
   }else if (input$select_LR_end_points=="LogTN"){
     names(to_download$LR_prediction2)[1:2]=c("LogTN","TN")
     name_in_plot_title = "TN"
   }else{
     names(to_download$LR_prediction2)[1]=input$select_LR_end_points
     name_in_plot_title = input$select_LR_end_points
   }
   myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
   
   #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) #YD commented this line out
   #print(length(myjoinedPred_to_map))
   myjoinedPred_to_map <- st_as_sf(myjoinedPred)
   #YD changed
   max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8)))
   min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
   updateSliderInput(session,"LR_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
   #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8) ## domain=myjoinedPred_to_map$HU8 if you want color legend to change
   
   rm(joined_HU8,HUC8_newData,myjoinedPred)
   ##YD added
   myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
   
   output$lr_prediction_map2 <- renderPlot ({
     req(input$LR_color_legend_range)
     print("inside rendering the multi-linear regression prediction HUC8 map now...")
     
     to_download$LR_map2 <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
       scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$LR_color_legend_range[1],input$LR_color_legend_range[2]))+
       theme(panel.background = element_blank())+ # remove grey background
       theme(plot.title=element_text(hjust=0.5))+ # put the title at center
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$LR_map2)
     
   })
   
   # rm(mjoinedPred_to_map,LR_map)
   
 }) #observeEvent2 end
 
 # Original Default
 output$savePredictionResults <- downloadHandler(
   filename = function(){paste(input$select_LR_end_points,"_linear_regression_model_prediction_results_",Sys.Date(),".csv",sep="")},
   content = function(file){
     write.csv(to_download$LR_prediction,file,row.names=FALSE)
   }
 )
 output$savePredictionMap <- downloadHandler(
   filename = function(){paste("linear_regression_model_prediction_map_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$LR_map,dpi=300,width=12,height=10)
   }
 )
 
 # MJP added for New User Data
 output$savePredictionResults2 <- downloadHandler(
   filename = function(){paste(input$select_LR_end_points,"_linear_regression_model_prediction_results_",Sys.Date(),".csv",sep="")},
   content = function(file){
     write.csv(to_download$LR_prediction2,file,row.names=FALSE)
   }
 )
 
 #mjp added
 output$savePredictionMap2 <- downloadHandler(
   filename = function(){paste("linear_regression_model_prediction_map_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$LR_map2,dpi=300,width=12,height=10)
   }
 )
 
 #####################################  added to display LR prediction map in bicolor ######################
 
 # Default 
 observeEvent(input$showLRModelPredictionMapTwoColors, {
   
   output$ShowRegressionModelPredictionMapBiColor <- renderUI({
     withSpinner(plotOutput("regression_prediction_map_bicolor",width="800px",height="600px"),type=2)
   })
   
   load("./Data/all_HU8_shapes.RData")
   
   if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="TP"){
     myjoinedPred = merge(joined_HU8,to_download$LR_prediction,by="huc8",duplicateGeoms=TRUE)
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) #YD commented this line out
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     ##YD added
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
     #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
     name_in_plot_title = "TP"
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$LR_cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$LR_cutoff_value),]
   }else if (input$select_LR_end_points=="LogTN" | input$select_LR_end_points=="TN"){
     myjoinedPred = merge(joined_HU8,to_download$LR_prediction,by="huc8",duplicateGeoms=TRUE)
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) #YD commented this line out
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
     name_in_plot_title = "TN"
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$LR_cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$LR_cutoff_value),]
   }
   
   rm(joined_HU8,myjoinedPred)
   
   output$regression_prediction_map_bicolor <- renderPlot ({
     
     print("inside rendering the regression prediction HUC8 bicolor map now...")
     
     to_download$LR_bicolor_map <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       theme(panel.background = element_blank())+ # remove grey background
       labs(fill=name_in_plot_title)+
       scale_fill_manual(values=c("blue","red"))+
       theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
       theme(legend.title=element_text(size=14))+
       theme(legend.text=element_text(size=14))+
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$LR_bicolor_map)
   })
   
 }) # observeEvent end
 
 output$saveLRPredictionMapTwoColors <- downloadHandler(
   filename = function(){paste("regression_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$LR_bicolor_map,dpi=300,width=12,height=10)
   }
 )
 
 #-----------------------------------------------------------------------------------------
 # MJP New User Map
 observeEvent(input$showLRModelPredictionMapTwoColors2, {
   
   output$ShowRegressionModelPredictionMapBiColor2 <- renderUI({
     withSpinner(plotOutput("regression_prediction_map_bicolor2",width="800px",height="600px"),type=2)
   })
   
   load("./Data/all_HU8_shapes.RData")
   
   if (input$select_LR_end_points=="LogTP" | input$select_LR_end_points=="TP"){
     testlr = to_download$LR_prediction2
     #save(testlr,file="./testlr_check.RData")
     myjoinedPred = merge(joined_HU8,to_download$LR_prediction2,by="huc8",duplicateGeoms=TRUE)
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) #YD commented this line out
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
     #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
     name_in_plot_title = "TP"
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$LR_cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$LR_cutoff_value),]
   }else if (input$select_LR_end_points=="LogTN" | input$select_LR_end_points=="TN"){
     myjoinedPred = merge(joined_HU8,to_download$LR_prediction2,by="huc8",duplicateGeoms=TRUE)
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) #YD commented this line out
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
     name_in_plot_title = "TN"
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$LR_cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$LR_cutoff_value),]
   }
   
   rm(joined_HU8,myjoinedPred)
   
   output$regression_prediction_map_bicolor2 <- renderPlot ({
     
     print("inside rendering the regression prediction HUC8 bicolor map now...")
     
     to_download$LR_bicolor_map2 <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$LR_cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       theme(panel.background = element_blank())+ # remove grey background
       labs(fill=name_in_plot_title)+
       scale_fill_manual(values=c("blue","red"))+
       theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
       theme(legend.title=element_text(size=14))+
       theme(legend.text=element_text(size=14))+
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$LR_bicolor_map2)
   })
   
 }) # observeEvent end
 
 output$saveLRPredictionMapTwoColors2 <- downloadHandler(
   filename = function(){paste("regression_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$LR_bicolor_map2,dpi=300,width=12,height=10)
   }
 )
 
 #####################################  model 2:  run random forest model  #######################################
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
  
  observeEvent(input$ConfirmRunRandomForest, {
    #print(input$ConfirmRunRandomForest)
    
    if(input$ConfirmRunRandomForest > 1)
    {
      shinyjs::hide("random_forest_output_panel")
    }
    
    data <- data_to_model$data_subset
    selected_variables = selected_variables_for_regression()
    print(paste0("the number of selected variables is:",length(selected_variables)))
    
    if (length(data)==0){
      
      alert_message ="Please go back to the previous tab to create your subset"
      shinyalert(" ",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info4")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
      
    }else if (input$mtry > length(selected_variables)) {
      alert_message ="mtry can not be more than the total number of predictor variables."
      shinyalert(" ",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "mtryAlert")
    }else{
      
      shinyjs::show("model_running_panel")
      
      
      selected_data_1 <- data %>% 
        select(c(input$select_RF_end_points, union("LAGOS_Lake_ID",selected_variables)))  
      
      print(paste0("check if two vectors are the same:",identical(sort(selected_variables),sort(data_to_model$default_variable_names))))
      
      
      data_HUC8_ID <- data %>%
        select(LAGOS_Lake_ID,HU8,HU2)
      
      rf_model_input$Lake_ID_with_HUC8 = data_HUC8_ID
      
      print(paste0("the selected data has ",length(selected_data_1), " columns"))
      
      ## removing observations with missing TP
      column_idx= which(colnames(selected_data_1)==input$select_RF_end_points)
      TP_data2 = selected_data_1[!is.na(selected_data_1[,column_idx]),]
      
      ## check if there is any predictor variable of the entire column is NA
      ## if so, remove those columns
      
      TP_data2 <- TP_data2[,colSums(is.na(TP_data2))!=nrow(TP_data2)]
      
      #save(TP_data2,file="./test_TP_data_for_RF.RData")
      TP_data2 <- na.roughfix(TP_data2)
      
      
      ## Imputing missing data with column medians
      #TP_data_rfr = na.roughfix(TP_data2[,])
      rf_model_input$data_rfr=subset(TP_data2,select=-c(LAGOS_Lake_ID))
      
      print(paste0("the number of oberservations is: ", nrow(rf_model_input$data_rfr)))
      
      ## Subsetting test set (10% of Dataset)
      set.seed(12)
      
      ind_test1 <- sample(nrow(TP_data2),round(nrow(TP_data2)*input$TestingsetPerc))
      tester1 = TP_data2[ind_test1,] # we need to keep the "LAGOS_Lake_ID" for later use
      trainer1 = TP_data2[-ind_test1,]
      trainer1= subset(trainer1,select=-c(LAGOS_Lake_ID)) # we don't need "LAGOS_Lake_ID" to run the ranger function
      
      ## get the testing dataset for later
      
      rf_model_input$testing_dataset = tester1
      print(paste0("the updated mtry is:",input$mtry))
      ## Fitting a random forest
      print(paste0("the ntree input is: ",input$ntree))
      start_time <- Sys.time()
      print(paste0("the seed value now is:",input$seedvalue))
      set.seed(input$seedvalue)
      ranger_formula = as.formula(paste0(input$select_RF_end_points,"~."))
      rfr_ranger = ranger(formula=ranger_formula,   # formula=F_Red,
                          data=trainer1,  # TP_Data_rfr <-use if not doing cross validation
                          num.trees=input$ntree,
                          mtry=input$mtry,
                          #sample.fraction=sample.frac,
                          #min.node.size=min.node,
                          importance='impurity')
      end_time <- Sys.time(); 
      
      print(paste0("the runtime for rfr_ranger is ",end_time - start_time))
      
      while (length(rfr_ranger)==0)
      {
        print("model is running")
      }
      
      
      rf_model_output$rfr_ranger = rfr_ranger
      ##save(rfr_ranger,file="./check_ranger_model_after_seed.RData")
      
      ## report model uncertainty & Bias
      
      #print(rfr_ranger$r.squared)
      #print(rfr_ranger$prediction.error)
      
      r2_report = rfr_ranger$r.squared
      error_report = rfr_ranger$prediction.error 
      
      #r2_to_print = formatC(rfr_ranger$r.squared,digits=4,format="f")
      #error_to_print = formatC(rfr_ranger$prediction.error,digits=4,format="f")
      
      r2 <- function(y,pred){
        1-var(y-pred)/var(y)
      }
      
      # R2 Testing Set
      column_endpoints_idx_tester= which(colnames(tester1)==input$select_RF_end_points)
      yobs_tester <- tester1[,column_endpoints_idx_tester]  
      ypred_tester = predict(rfr_ranger, data =tester1)
      r2_tester = r2(yobs_tester,ypred_tester$predictions)
      # r2_tester_to_print = formatC(r2(yobs_tester,ypred_tester$predictions),digits=4,format="f") 
      
      # R2 Training Set
      column_endpoints_idx_trainer= which(colnames(trainer1)==input$select_RF_end_points)
      yobs_trainer <- trainer1[,column_endpoints_idx_trainer]  
      ypred_trainer = predict(rfr_ranger, data =trainer1)
      r2_trainer = r2(yobs_trainer,ypred_trainer$predictions)
      #r2_trainer_to_print = formatC(r2(yobs_trainer,ypred_trainer$predictions),digits=4,format="f") 
      
      #Root Mean Squared Error (RMSE)
      rmse = sqrt(mean((ypred_tester$predictions-yobs_tester)^2))
      #rmse_to_print = formatC(rmse,digits=4,format="f")
      
      # Mean Bias
      bias = ( sum(ypred_tester$predictions) - sum(yobs_tester)) / length(yobs_tester) 
      #bias_to_print = formatC(bias,digits=4,format="f")
      
      #standard deviation of the error
      SD = sd(ypred_tester$predictions - yobs_tester)
      #SD_to_print = formatC(SD,digits=4,format="f")
      
      nse2 <- vnse(ypred_tester$predictions, yobs_tester,na.rm=T) # Nash-Sutcliffe efficiency coefficient (added by MJP)
      
      table_col_1 = c("r.squared (reported by Ranger function)","prediction.error (reported by Ranger function)","R squared (testing set)",
                      "R squared (training set)", "Root Mean Squared Error","Mean Bias","Standard deviation of the error",
                      "Nash-Sutcliffe efficiency coefficient")
      #table_col_2 = c(r2_to_print,error_to_print,r2_tester_to_print,r2_trainer_to_print,rmse_to_print,bias_to_print,SD_to_print)
      
      table_col_2 = c(r2_report,error_report,r2_tester,r2_trainer,rmse,bias,SD,nse2)
      table_col_2 = formatC(table_col_2,digits=4,format="f")
      
      table_to_print <- data.frame(cbind(table_col_1,table_col_2))
      colnames(table_to_print) <- c("Model Uncertainty Metrics","Values")
      
      output$RF_model_output_table <- renderTable({
        table_to_print},type="html",bordered=TRUE,striped=TRUE,align="c")
      
      ## create and display Observed vs Predicted plot
      
      tempDF = data.frame(yobs=yobs_tester,ypred=ypred_tester$predictions)
      
      output$observed_predicted_plot <- renderPlot({
        myPlot <- ggplot(tempDF,aes(yobs,ypred))+
          geom_point(size=0.5,color='blue') +
          geom_smooth(method='lm',color='grey') +
          geom_abline(slope=1,intercept=0,color="red") +
          scale_x_continuous(expand = c(0,0),limits = c(0,NA)) +
          scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
          xlab(paste0("Observed ",input$select_RF_end_points)) +
          ylab(paste0("Predicted ",input$select_RF_end_points)) +
          ggtitle("Random Forest Observed vs Predicted") +
          theme_bw() +
          theme(text=element_text(size=15,face = "bold", color="blue"),
                plot.title=element_text(hjust=0.5,size=15))
        print(myPlot)
      })
      
      ## create and display variable importance plot
      
      imprtnce = as.data.frame(importance(rfr_ranger))
      names(imprtnce)[1] = "PercIncMSE" 
      setDT(imprtnce, keep.rownames = TRUE)[]
      imprtnce = imprtnce[,1:2]
      names(imprtnce)[1] = "Variable.Name"
      imprtnce2 = imprtnce[order(-imprtnce$PercIncMSE),]
      all_imprtnce2 = imprtnce2
      nrow_important = min(nrow(imprtnce2),10)
      imprtnce2 = imprtnce2[1:nrow_important,] # Select top 10
      imprtnce2 = imprtnce2[order(imprtnce2$PercIncMSE),] # sort again
      
      print(imprtnce2)
      
      # assign different color based on the groups
      
      ## check if the group names were initialized inside the first tab
      ## if not, assign group names here
      start_initialize_time <- Sys.time()
      if (length(group_names$N_Inventory_Scaled)==0){
        
        data <- loaded_data()
        data$HU8=as.factor(data$HU8)
        data$HU2=as.factor(data$HU2) 
        myList <- generateGroupList(data)
        selected_data_1 <- myList$selected_data_1
        selected_data_Lake <- myList$selected_data_Lake
        group_names$Lake_Character = colnames(selected_data_Lake)
        selected_data_Land <- myList$selected_data_Land
        group_names$NLCD_Land_Use = colnames(selected_data_Land)
        selected_data_Weather <- myList$selected_data_Weather
        group_names$Weather_related = colnames(selected_data_Weather)
        selected_data_Vegetation <- myList$selected_data_Vegetation
        group_names$Vegetation = colnames(selected_data_Vegetation)
        selected_data_Aerosol <- myList$selected_data_Aerosol
        group_names$Aerosol_related = colnames(selected_data_Aerosol)
        selected_data_N <- myList$selected_data_N
        group_names$Aerosol_related = colnames(selected_data_Aerosol)
        selected_data_N_Scaled <- myList$selected_data_N_Scaled
        group_names$N_Inventory_Scaled = colnames(selected_data_N_Scaled)
        selected_data_P <- myList$selected_data_P
        group_names$P_Inventory_Raw = colnames(selected_data_P)
        selected_data_P_Scaled <- myList$selected_data_P_Scaled
        group_names$P_Inventory_Scaled = colnames(selected_data_P_Scaled)
        selected_data_Deposition <- myList$selected_data_Deposition
        group_names$Deposition = colnames(selected_data_Deposition)
        #selected_data_Sampling_Info <- myList$selected_data_Sampling_Info
        #group_names$Sampling_Info = colnames(selected_data_Sampling_Info)
        rm(data,myList)
        rm(selected_data_1,selected_data_Lake,selected_data_Land,selected_data_Weather,selected_data_Vegetation,selected_data_Aerosol,selected_data_N)
        rm(selected_data_N_Scaled,selected_data_P,selected_data_P_Scaled,selected_data_Deposition)#,selected_data_Sampling_Info)
      }
      
      end_initialize_time <- Sys.time()
      print(paste0("the runtime for the variable assignments is:",(end_initialize_time-start_initialize_time)))
      mypal = vector(mode="character",length=nrow(imprtnce2))
      mypal_group = vector(mode="character",length=nrow(imprtnce2))
      
      for (i in 1:nrow(imprtnce2)){
        if (any(grepl(imprtnce2$Variable.Name[i],group_names$N_Inventory_Scaled))==TRUE){
          mypal[i]="#00FF00"  # green for "N Inventory" group
          mypal_group[i]="N Inventory"
          # }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$N_Inventory_Raw))==TRUE){
          #    mypal[i]="#44FF00" # another green for "N Inventory Raw" group
          #    mypal_group[i]="N Inventory Raw"
        }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$Vegetation))==TRUE){
          mypal[i]="#006600" # dark green for "Vegetation" group
          mypal_group[i]="Vegetation related"
        }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$Lake_Character))==TRUE){
          mypal[i]="#0000FF" # lake blue for "Lake Characteristics" group
          mypal_group[i]="Date/Lake Characteristics"
        }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$P_Inventory_Scaled))==TRUE){
          mypal[i]="#00FFFF" # sky blue for "P Inventory" group
          mypal_group[i]="P Inventory"
          # }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$P_Inventory_Raw))==TRUE){
          #   mypal[i]="#66B2FF" # light blue for "P Inventory Raw" group
          #   mypal_group[i]="P Inventory Raw"
        }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$NLCD_Land_Use))==TRUE){
          mypal[i]="#FFFF66" # yellow for "NLCD Land Use" group
          mypal_group[i]="NLCD Land_Use"
        }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$Aerosol_related))==TRUE){
          mypal[i]="#CC6600" # brown for "Aerosol related" group
          mypal_group[i]="Aerosol related"
        }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$Deposition))==TRUE){
          mypal[i]="#606060" # grey for "Deposition" group
          mypal_group[i]="Deposition"
        }else if (any(grepl(imprtnce2$Variable.Name[i],group_names$Weather_related))==TRUE){
          mypal[i]="#9933FF" # purple for "Weather related" group 
          mypal_group[i]="Weather related"
          #}else if (any(grepl(imprtnce2$Variable.Name[i],group_names$Sampling_Info))==TRUE){
          #mypal[i]="#FF3333" # red for "Sampling information" group
          #mypal_group[i]="Sampling information"
        }else{
          mypal[i]="#000000"
          mypal_group[i]="Other"
        }    
      }
      # mypal = rev(mypal)
      
      # set order for labels 
      rf_model_output$important_variables <- imprtnce2$Variable.Name
      rf_model_output$all_important_variables <- all_imprtnce2$Variable.Name
      imprtnce2$Variable.Name <- 
        factor(imprtnce2$Variable.Name, 
               levels = imprtnce2$Variable.Name)
      
      #save(mypal,mypal_group,imprtnce2,file="./test_importance_plot.RData")
      
      ### the following code is added to create a color key for the importance plot
      unique_colors = unique(mypal)
      index_for_each_color = match(unique(mypal),mypal)
      unique_groups = mypal_group[index_for_each_color]
      color_key_data = data.frame(unique_colors,unique_groups,unique_values=rep(1,length(unique_colors)))
      
      colorKeyPlot <- ggplot(data=color_key_data, aes(x=unique_groups, y=unique_values,fill=unique_groups))+
        geom_bar(stat="identity") +
        scale_fill_manual(values = unique_colors,labels=unique_groups,name="color key for groups") +
        theme_bw() +
        theme(text=element_text(size=15,face = "bold", color="blue"),
              plot.title=element_text(hjust=0.5,size=15)) +
        theme(axis.line = element_line(colour = "black"))+ #+# add axis lines
        coord_flip()
      
      colorKeyLegend <- get_legend(colorKeyPlot)
      
      legend_plot <- as_ggplot(colorKeyLegend) + 
        theme(plot.margin = unit(c(0,0,5,5),"mm"))
      
      output$variable_importance_plot <- renderPlot ({
        
        myPlot <- ggplot(data=imprtnce2, aes(x=Variable.Name, y=PercIncMSE,fill=Variable.Name))+
          geom_bar(stat="identity") +
          labs(x=" ", y="% Increase in MSE") +
          scale_fill_manual(values = mypal,labels=mypal_group,name="variables in groups",guide="none") +
          ggtitle("Random Forest Variable Importance (Top 10)") +
          theme_bw() +
          theme(text=element_text(size=15,face = "bold", color="blue"),
                plot.title=element_text(hjust=0.5,size=15)) +
          #scale_y_continuous(breaks= seq(0, 25, by=5),limits = c(0,25))+
          theme(axis.line = element_line(colour = "black"))+ #+# add axis lines
          coord_flip()
        myPlot_updated <- grid.arrange(myPlot,legend_plot,ncol=2,widths=c(2.5,1))
        print(myPlot_updated)
      })
      
      
      ### to see more model output visualization
      output$seeMoreButton <- renderUI({
        actionButton(inputId="seeMore", label="See additional plots and maps...",style="color:blue;background-color:black;")
      }) 
      
      shinyjs::hide("model_running_panel")
      shinyjs::show("random_forest_output_panel")
      
      rm(TP_data2,rfr_ranger)
      
    } # if,else loop end
    
    shinyjs::hide("partial_dependence_plot")
    shinyjs::hide("more_partial_dependence_plot")
    
  }) #observeEvent end
  
  observeEvent(input$mtryAlert,{
    shinyjs::runjs("swal.close();")
  }) 
  
  
  #### this part is added to link the variables from multilinear regression model to random forest model  ######
  
<<<<<<< HEAD
  observeEvent(input$use_LR_variables_button, {
    
    data <- data_to_model$data_subset
    
    if (length(data)==0){
      
      alert_message ="Please go back to the previous tab to create your subset"
      shinyalert(" ",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info4")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
      
    }else if(length(lr_model_output$final_predictors)==0){
      alert_message ="No multilinear regression model was created!"
      shinyalert("Alert",alert_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "info5")
      shinyjs::delay(3000, shinyjs::runjs("swal.close();"))
    }else{
      LR_variables = lr_model_output$final_predictors
      
      ## update the predictor variables from the multilinear regression modelin each selectizeInput group
      if (any(LR_variables %in% group_names_for_default$NLCD_names)){
        selected_variables <- group_names_for_default$NLCD_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_1",choices = group_names_for_default$NLCD_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_1",choices = group_names_for_default$NLCD_names,selected=NULL)
      }
      
      if (any(LR_variables %in% group_names_for_default$P_inventory_names)){
        selected_variables <- group_names_for_default$P_inventory_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_2",choices = group_names_for_default$P_inventory_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_2",choices = group_names_for_default$P_inventory_names,selected=NULL)
      }
      
      if (any(LR_variables %in% group_names_for_default$N_inventory_names)){
        selected_variables <- group_names_for_default$N_inventory_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_3",choices = group_names_for_default$N_inventory_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_3",choices = group_names_for_default$N_inventory_names,selected=NULL)
      }
      
      if (any(LR_variables %in% group_names_for_default$aerosol_names)){
        selected_variables <- group_names_for_default$aerosol_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_4",choices = group_names_for_default$aerosol_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_4",choices = group_names_for_default$aerosol_names,selected=NULL)
      }
      
      if (any(LR_variables %in% group_names_for_default$weather_names)){
        selected_variables <- group_names_for_default$weather_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_5",choices = group_names_for_default$weather_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_5",choices = group_names_for_default$weather_names,selected=NULL)
      }
      
      if (any(LR_variables %in% group_names_for_default$vegetation_names)){
        selected_variables <- group_names_for_default$vegetation_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_6",choices = group_names_for_default$vegetation_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_6",choices = group_names_for_default$vegetation_names,selected=NULL)
      }
      
      if (any(LR_variables %in% group_names_for_default$deposition_names)){
        selected_variables <- group_names_for_default$deposition_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_7",choices = group_names_for_default$deposition_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_7",choices = group_names_for_default$deposition_names,selected=NULL)
      }
      
      if (any(LR_variables %in% group_names_for_default$lake_char_names)){
        selected_variables <- group_names_for_default$lake_char_names
        selected_variables <- selected_variables[which(selected_variables %in% LR_variables)] 
        updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=selected_variables)
      }else{
        updateSelectizeInput(session,"select_from_group_8",choices = group_names_for_default$lake_char_names,selected=NULL)
      }
      
    } # if,else loop end
    shinyjs::hide("random_forest_output_panel")
    shinyjs::hide("partial_dependence_plot")
    shinyjs::hide("more_partial_dependence_plot")
    
  }) #observeEvent end
  
  #### link the variables from multilinear regression model to random forest model code end  ######
  
  
  observeEvent(input$seeMore, {
    
    # output$display_partial_dependence_plot <- renderUI({
    #   withSpinner(plotOutput("partial_dependence_plot",width="600px",height="500px"),type=2)
    # })
    # shinyjs::show("partial_dependence_plot")
    # 
    # #### create and display the partial dependence plot for four preselected variables
    # ylabel = paste0("Predicted  ", input$select_RF_end_points)
    # partial_start_time <- Sys.time()
    # ## sometimes the total number of predictor variables might be smaller than 10
    # idx_1 <- min(length(rf_model_output$important_variables),10)
    # idx_2 <- min(length(rf_model_output$important_variables)-1,9)
    # idx_3 <- min(length(rf_model_output$important_variables)-2,8)
    # idx_4 <- min(length(rf_model_output$important_variables)-3,7)
    # par.p1 <- partial(rf_model_output$rfr_ranger, train = rf_model_input$data_rfr, which.class=2,
    #                   pred.var=rf_model_output$important_variables[idx_1], chull= T)
    # if (idx_2>0){
    # par.p2 <- partial(rf_model_output$rfr_ranger, train = rf_model_input$data_rfr, which.class=2,
    #                   pred.var=rf_model_output$important_variables[idx_2], chull= T)
    # }
    # if (idx_3>0){
    # par.p3 <- partial(rf_model_output$rfr_ranger, train = rf_model_input$data_rfr, which.class=2,
    #                   pred.var=rf_model_output$important_variables[idx_3], chull= T)
    # }
    # if (idx_4>0){
    # par.p4 <- partial(rf_model_output$rfr_ranger, train = rf_model_input$data_rfr, which.class=2,
    #            pred.var=rf_model_output$important_variables[idx_4], chull= T)
    # }
    # partial_end_time <- Sys.time()
    # print(paste0("the runtime for partial dependence calculation is ",partial_end_time - partial_start_time))
    
    output$morePartialDependence <- renderUI({
      selectizeInput("select_variables_PD",label ="Please select a variable to see partial dependence plot",
                     choices=rev(rf_model_output$important_variables),
                     multiple = FALSE,
                     options = list(hideSelected = FALSE,plugins=list('remove_button')))
    })
    
    PD_slider_text = paste0("This slider allows you to reduce the X axis limit by applying a ratio to the maximum X value.","Default is 1.")
    output$displayPDSlider <- renderUI({
      tipify(sliderInput("PD_X_axis_scale","Adjust partical dependence X axis scale",min=0.05,max=1,value=1,step=0.05,sep=""),PD_slider_text,placement="right",trigger = "hover")
    })
    
    # output$partial_dependence_plot <- renderPlot ({
    #   
    #   ## gather the variables already shown (maximum number is 4) in partial dependence plot
    #   PD_already_shown <- rf_model_output$important_variables[idx_1]
    #   
    #   p1 <- autoplot(par.p1, contour = TRUE, 
    #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_1])+theme_bw()
    #   if (idx_2>0){
    #   p2 <- autoplot(par.p2, contour = TRUE, 
    #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_2])+theme_bw()
    #   PD_already_shown <- c(PD_already_shown,rf_model_output$important_variables[idx_2])
    #   }else{
    #   p2 <- ggplot()+theme_void() ## create an empty plot
    #   }
    #   if (idx_3>0){
    #   p3 <- autoplot(par.p3, contour = TRUE, 
    #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_3])+theme_bw()
    #   PD_already_shown <- c(PD_already_shown,rf_model_output$important_variables[idx_3])
    #   }else{
    #   p3 <- ggplot()+theme_void() ## create an empty plot
    #   }
    #   if (idx_4>0){
    #   p4 <- autoplot(par.p4, contour = TRUE, 
    #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_4])+theme_bw()
    #   PD_already_shown <- c(PD_already_shown,rf_model_output$important_variables[idx_4])
    #   }else{
    #   p4 <- ggplot()+theme_void() ## create an empty plot
    #   }
    #   print(PD_already_shown)
    #   updated_choices <- rf_model_output$important_variables[!(rf_model_output$important_variables %in% PD_already_shown)]
    #   print(updated_choices)
    #   updateSelectizeInput(session,"select_variables_PD",choices = updated_choices,selected=NULL)
    #   # Puts all the plots together
    #   myPlot <- grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)
    #   print(myPlot)
    # }) #renderPlot end
    
    output$displayMoreButton <- renderUI({
      actionButton(inputId="displayMore", label="Display partial dependence plot",style="color:blue;background-color:black")
    })
    
    ## remove this feature (requested in the final round of revision in September 2022)
    # output$showMapButton <- renderUI({
    #   actionButton(inputId="showPredictionMap", label="Show Prediction Maps for testing dataset",style="color:blue;background-color:black")
    # })
    
    output$RF_Break1 <- renderUI({
      div(
        h5("The following 2 sliders and toggle button are used for default datasets only",style="color:red;font-style:bold")
      )
    })
    
    output$select_prediction_year_slider <- renderUI({
      sliderInput("predict_year","Prediction year:",min=2002,max=2012,value=2007,step=5,round=0,sep="")
    })
    
    output$select_prediction_month_slider <- renderUI({
      sliderInput("predict_month","Prediction month:",min=2,max=12,value=7,step=5,round=0,sep="")
    })
    
    maxDepth_tooltip_text = paste0("Please select the lake maxDepth level at HUC8 scale, 'Shallow (3.6m)','Average (10.1m)'and 'Deep (24.4m)' represent 10%, 50% and 90% of LAGOS lakes maxDepth respectively.")
    output$select_prediction_lake_maxDepth_button <- renderUI({
      tipify(radioButtons("lake_maxDepth",label="Lake maxDepth",choices=c("Shallow","Average","Deep"),selected="Average",inline=FALSE),maxDepth_tooltip_text,
             placement="right",trigger="hover")
    })
    
    # MJP added
    # output$RF_Break2 <- renderUI({
    #   div(
    #     h5("!!!Make Sure to Click Correct Button Based on the Dataset Being Used!!!",style="color:red;font-style:bold")
    #   )
    # })
    
    # MJP changes
    output$display_rf_map_button <- renderUI({
      #div(
      #h5("---------------------------------------",style="color:red;font-style:bold"),
      #h5("Make Sure You only click Correct Button Based on the Dataset Being Used",style="color:red;font-style:bold"),
      #h5("---------------------------------------",style="color:red;font-style:bold"),
      actionButton(inputId="showModelPredictionMapNew", label="Show prediction map for whole region (if using default dataset only!!!)",style="color:blue;background-color:black")
      #)
    })
    
    # MJP added
    # output$RF_Break3 <- renderUI({
    #   div(
    #     h5("Default Map Buttons (!!!Only click if loaded default dataset in first section!!!):",style="color:red;font-style:bold")
    #   )
    # })
    
    # Original
    output$display_download_rf_results_button <- renderUI({
      downloadButton(outputId="saveRFPredictionResults", label="Download prediction results to csv (Default Dataset)",style="color:blue;background-color:black")
    })
    
    # Original
    output$download_rf_map_button <- renderUI({
      downloadButton(outputId="saveRFPredictionMap", label="Save prediction map (Default Dataset)",style="color:blue;background-color:black")
    })
    
    output$RF_Break4a <- renderUI({
      div(id="RF_h5",
          h5("New Dataset Load Button:",style="color:red;font-style:bold")
      )
    })
    
    # MJP changes
    output$display_rf_load_button <- renderUI({
      fileInput(inputId="uploaded_rf_newDataset", label="Load new dataset (if have new variables)", multiple=FALSE,accept=c("text/csv",".csv",".RData"))
    })
    
    
    output$RF_Break4 <- renderUI({
      div(
        h5("New Dataset Map Buttons (!!!Only click if uploaded new dataset!!!):",style="color:red;font-style:bold")
      )
    })
    
    # MJP changes
    output$display_rf_map_button2 <- renderUI({
      actionButton(inputId="showModelPredictionMapNew2", label="Show prediction map for whole region (if using new dataset only!!!)",style="color:blue;background-color:black")
    })
    
    # output$RF_Break5 <- renderUI({
    #   div(
    #     h5("These buttons work for either default or new datasets:",style="color:red;font-style:bold")
    #   )
    # })
    
    
    #MJP addition
    output$display_download_rf_results_button2 <- renderUI({
      downloadButton(outputId="saveRFPredictionResults2", label="Download prediction results to csv (New Dataset)",style="color:blue;background-color:black")
    })
    
    # MJP Addition
    output$download_rf_map_button2 <- renderUI({
      downloadButton(outputId="saveRFPredictionMap2", label="Save prediction map (New Dataset)",style="color:blue;background-color:black")
    })
    
    
    output$adjustRFMapColorLegend <- renderUI({
      sliderInput("RF_color_legend_range","Adjust map color legend here:",min=0,max=10,value=c(1,6),step=0.5,sep="")
    })
    
    cutoff_tooltip_text = paste0("The endpoint concentration criteria is used to display the prediction map in two different colors (above or below this criteria) accordingly."," Default is 50.")
    output$display_userInput_cutoff <- renderUI({
      tipify(numericInput("cutoff_value",label ="Endpoint concentration criteria",50,min=1,max=300,step=1),cutoff_tooltip_text,placement="right",trigger="hover")
    })
    
    output$RF_Break6 <- renderUI({
      div(
        h5("!!!Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold")
      )
    })
    
    output$RF_Break7 <- renderUI({
      div(
        h5("Only use the next two buttons if using default dataset:",style="color:red;font-style:bold")
      )
    })
    output$display_rf_map_in_two_colors_button <- renderUI({
      #div(
      #   h5("---------------------------------------------------",style="color:red;font-style:bold"),
      #  h5("Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold"),
      #h5("---------------------------------------------------",style="color:red;font-style:bold"),
      #h5("Only use the next two buttons if using default dataset",style="color:red;font-style:bold"),
      actionButton(inputId="showModelPredictionMapTwoColors", label="Show prediction map in bicolor (Default Dataset)",style="color:blue;background-color:black")
      #)
    })
    
    output$download_rf_map_in_two_colors_button <- renderUI({
      downloadButton(outputId="saveRFPredictionMapTwoColors", label="Save prediction map in bicolor (Default Dataset)",style="color:blue;background-color:black")
    })
    
    output$RF_Break8 <- renderUI({
      div(
        h5("Only use the next two buttons if uploaded new dataset above:",style="color:red;font-style:bold")
      )
    })
    
    # MJP added
    output$display_rf_map_in_two_colors_button2 <- renderUI({
      actionButton(inputId="showModelPredictionMapTwoColors2", label="Show prediction map in bicolor (New User Dataset)",style="color:blue;background-color:black")
    })
    
    # MJP added
    output$download_rf_map_in_two_colors_button2 <- renderUI({
      downloadButton(outputId="saveRFPredictionMapTwoColors2", label="Save prediction map in bicolor (New User Dataset)",style="color:blue;background-color:black")
    })
    ##YD added
    print(paste0("inside seeMore now, check logic:",is.null(input$uploaded_dataset)))
    
    if (!is.null(input$uploaded_dataset)){
      delay(1000,shinyjs::hide("predict_year"))
      delay(1000,shinyjs::hide("predict_month"))
      delay(1000,shinyjs::hide("lake_maxDepth"))
      delay(1200,shinyjs::hide("showModelPredictionMapNew"))
      delay(1200,shinyjs::hide("saveRFPredictionResults"))
      delay(1200,shinyjs::hide("saveRFPredictionMap"))
      delay(1500,shinyjs::hide("showModelPredictionMapTwoColors"))
      delay(1500,shinyjs::hide("saveRFPredictionMapTwoColors"))
    }else{
      delay(1000,shinyjs::hide("RF_h5"))
      delay(1000,shinyjs::hide("uploaded_rf_newDataset"))
      delay(1200,shinyjs::hide("showModelPredictionMapNew2"))
      delay(1200,shinyjs::hide("saveRFPredictionResults2"))
      delay(1200,shinyjs::hide("saveRFPredictionMap2"))
      delay(1500,shinyjs::hide("showModelPredictionMapTwoColors2"))
      delay(1500,shinyjs::hide("saveRFPredictionMapTwoColors2"))
    }
    
  }) #seeMore observeEvent end
  
  
  observeEvent(input$displayMore, {
    
    output$display_more_partial_dependence_plot <- renderUI({
      withSpinner(plotOutput("more_partial_dependence_plot",width="600px",height="500px"),type=2)
    })
    
    shinyjs::show("more_partial_dependence_plot")
    
    print(input$select_variables_PD)
    
=======
   # output$partial_dependence_plot <- renderPlot ({
   #   
   #   ## gather the variables already shown (maximum number is 4) in partial dependence plot
   #   PD_already_shown <- rf_model_output$important_variables[idx_1]
   #   
   #   p1 <- autoplot(par.p1, contour = TRUE, 
   #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_1])+theme_bw()
   #   if (idx_2>0){
   #   p2 <- autoplot(par.p2, contour = TRUE, 
   #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_2])+theme_bw()
   #   PD_already_shown <- c(PD_already_shown,rf_model_output$important_variables[idx_2])
   #   }else{
   #   p2 <- ggplot()+theme_void() ## create an empty plot
   #   }
   #   if (idx_3>0){
   #   p3 <- autoplot(par.p3, contour = TRUE, 
   #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_3])+theme_bw()
   #   PD_already_shown <- c(PD_already_shown,rf_model_output$important_variables[idx_3])
   #   }else{
   #   p3 <- ggplot()+theme_void() ## create an empty plot
   #   }
   #   if (idx_4>0){
   #   p4 <- autoplot(par.p4, contour = TRUE, 
   #                  ylab=ylabel, xlab=rf_model_output$important_variables[idx_4])+theme_bw()
   #   PD_already_shown <- c(PD_already_shown,rf_model_output$important_variables[idx_4])
   #   }else{
   #   p4 <- ggplot()+theme_void() ## create an empty plot
   #   }
   #   print(PD_already_shown)
   #   updated_choices <- rf_model_output$important_variables[!(rf_model_output$important_variables %in% PD_already_shown)]
   #   print(updated_choices)
   #   updateSelectizeInput(session,"select_variables_PD",choices = updated_choices,selected=NULL)
   #   # Puts all the plots together
   #   myPlot <- grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)
   #   print(myPlot)
   # }) #renderPlot end
 
 output$displayMoreButton <- renderUI({
   actionButton(inputId="displayMore", label="Display partial dependence plot",style="color:blue;background-color:black")
 })
 
 ## remove this feature (requested in the final round of revision in September 2022)
 # output$showMapButton <- renderUI({
 #   actionButton(inputId="showPredictionMap", label="Show Prediction Maps for testing dataset",style="color:blue;background-color:black")
 # })
 
 output$RF_Break1 <- renderUI({
   div(
     h5("The following 2 sliders and toggle button are used for default datasets only",style="color:red;font-style:bold")
   )
 })
 
 output$select_prediction_year_slider <- renderUI({
   sliderInput("predict_year","Prediction year:",min=2002,max=2012,value=2007,step=5,round=0,sep="")
 })
 
 output$select_prediction_month_slider <- renderUI({
   sliderInput("predict_month","Prediction month:",min=2,max=12,value=7,step=5,round=0,sep="")
 })
 
 maxDepth_tooltip_text = paste0("Please select the lake maxDepth level at HUC8 scale, 'Shallow (3.6m)','Average (10.1m)'and 'Deep (24.4m)' represent 10%, 50% and 90% of LAGOS lakes maxDepth respectively.")
 output$select_prediction_lake_maxDepth_button <- renderUI({
 tipify(radioButtons("lake_maxDepth",label="Lake maxDepth",choices=c("Shallow","Average","Deep"),selected="Average",inline=FALSE),maxDepth_tooltip_text,
        placement="right",trigger="hover")
 })
 
 # MJP added
 # output$RF_Break2 <- renderUI({
 #   div(
 #     h5("!!!Make Sure to Click Correct Button Based on the Dataset Being Used!!!",style="color:red;font-style:bold")
 #   )
 # })
 
 # MJP changes
 output$display_rf_map_button <- renderUI({
   #div(
   #h5("---------------------------------------",style="color:red;font-style:bold"),
   #h5("Make Sure You only click Correct Button Based on the Dataset Being Used",style="color:red;font-style:bold"),
   #h5("---------------------------------------",style="color:red;font-style:bold"),
   actionButton(inputId="showModelPredictionMapNew", label="Show prediction map for whole region (if using default dataset only!!!)",style="color:blue;background-color:black")
   #)
   })
 
 # MJP added
 # output$RF_Break3 <- renderUI({
 #   div(
 #     h5("Default Map Buttons (!!!Only click if loaded default dataset in first section!!!):",style="color:red;font-style:bold")
 #   )
 # })
 
 # Original
 output$display_download_rf_results_button <- renderUI({
   downloadButton(outputId="saveRFPredictionResults", label="Download prediction results to csv (Default Dataset)",style="color:blue;background-color:black")
 })
 
 # Original
 output$download_rf_map_button <- renderUI({
   downloadButton(outputId="saveRFPredictionMap", label="Save prediction map (Default Dataset)",style="color:blue;background-color:black")
 })
 
 output$RF_Break4a <- renderUI({
   div(id="RF_h5",
     h5("New Dataset Load Button:",style="color:red;font-style:bold")
   )
 })
 
 # MJP changes
 output$display_rf_load_button <- renderUI({
   fileInput(inputId="uploaded_rf_newDataset", label="Load new dataset (if have new variables)", multiple=FALSE,accept=c("text/csv",".csv",".RData"))
 })
 
 
 output$RF_Break4 <- renderUI({
   div(
     h5("New Dataset Map Buttons (!!!Only click if uploaded new dataset!!!):",style="color:red;font-style:bold")
   )
 })
 
 # MJP changes
 output$display_rf_map_button2 <- renderUI({
   actionButton(inputId="showModelPredictionMapNew2", label="Show prediction map for whole region (if using new dataset only!!!)",style="color:blue;background-color:black")
 })
 
 # output$RF_Break5 <- renderUI({
 #   div(
 #     h5("These buttons work for either default or new datasets:",style="color:red;font-style:bold")
 #   )
 # })

 
 #MJP addition
 output$display_download_rf_results_button2 <- renderUI({
  downloadButton(outputId="saveRFPredictionResults2", label="Download prediction results to csv (New Dataset)",style="color:blue;background-color:black")
 })
 
 # MJP Addition
 output$download_rf_map_button2 <- renderUI({
   downloadButton(outputId="saveRFPredictionMap2", label="Save prediction map (New Dataset)",style="color:blue;background-color:black")
 })
 
 
 output$adjustRFMapColorLegend <- renderUI({
   sliderInput("RF_color_legend_range","Adjust map color legend here:",min=0,max=10,value=c(1,6),step=0.5,sep="")
 })
 
 cutoff_tooltip_text = paste0("The endpoint concentration criteria is used to display the prediction map in two different colors (above or below this criteria) accordingly."," Default is 50.")
 output$display_userInput_cutoff <- renderUI({
   tipify(numericInput("cutoff_value",label ="Endpoint concentration criteria",50,min=1,max=300,step=1),cutoff_tooltip_text,placement="right",trigger="hover")
 })
 
 output$RF_Break6 <- renderUI({
   div(
     h5("!!!Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold")
   )
 })
 
 output$RF_Break7 <- renderUI({
   div(
     h5("Only use the next two buttons if using default dataset:",style="color:red;font-style:bold")
   )
 })
 output$display_rf_map_in_two_colors_button <- renderUI({
   #div(
  #   h5("---------------------------------------------------",style="color:red;font-style:bold"),
   #  h5("Only use the following buttons after creating the color map above!!!",style="color:red;font-style:bold"),
   #h5("---------------------------------------------------",style="color:red;font-style:bold"),
   #h5("Only use the next two buttons if using default dataset",style="color:red;font-style:bold"),
   actionButton(inputId="showModelPredictionMapTwoColors", label="Show prediction map in bicolor (Default Dataset)",style="color:blue;background-color:black")
   #)
   })
 
 output$download_rf_map_in_two_colors_button <- renderUI({
   downloadButton(outputId="saveRFPredictionMapTwoColors", label="Save prediction map in bicolor (Default Dataset)",style="color:blue;background-color:black")
 })
 
 output$RF_Break8 <- renderUI({
   div(
     h5("Only use the next two buttons if uploaded new dataset above:",style="color:red;font-style:bold")
   )
 })
 
 # MJP added
 output$display_rf_map_in_two_colors_button2 <- renderUI({
   actionButton(inputId="showModelPredictionMapTwoColors2", label="Show prediction map in bicolor (New User Dataset)",style="color:blue;background-color:black")
   })
 
 # MJP added
 output$download_rf_map_in_two_colors_button2 <- renderUI({
   downloadButton(outputId="saveRFPredictionMapTwoColors2", label="Save prediction map in bicolor (New User Dataset)",style="color:blue;background-color:black")
 })
 ##YD added
 print(paste0("inside seeMore now, check logic:",is.null(input$uploaded_dataset)))
 
 if (!is.null(input$uploaded_dataset)){
   delay(1000,shinyjs::hide("predict_year"))
   delay(1000,shinyjs::hide("predict_month"))
   delay(1000,shinyjs::hide("lake_maxDepth"))
   delay(1200,shinyjs::hide("showModelPredictionMapNew"))
   delay(1200,shinyjs::hide("saveRFPredictionResults"))
   delay(1200,shinyjs::hide("saveRFPredictionMap"))
   delay(1500,shinyjs::hide("showModelPredictionMapTwoColors"))
   delay(1500,shinyjs::hide("saveRFPredictionMapTwoColors"))
 }else{
   delay(1000,shinyjs::hide("RF_h5"))
   delay(1000,shinyjs::hide("uploaded_rf_newDataset"))
   delay(1200,shinyjs::hide("showModelPredictionMapNew2"))
   delay(1200,shinyjs::hide("saveRFPredictionResults2"))
   delay(1200,shinyjs::hide("saveRFPredictionMap2"))
   delay(1500,shinyjs::hide("showModelPredictionMapTwoColors2"))
   delay(1500,shinyjs::hide("saveRFPredictionMapTwoColors2"))
 }
 
 }) #seeMore observeEvent end
  
  
 observeEvent(input$displayMore, {
   
   output$display_more_partial_dependence_plot <- renderUI({
     withSpinner(plotOutput("more_partial_dependence_plot",width="600px",height="500px"),type=2)
   })
   
   shinyjs::show("more_partial_dependence_plot")
   
   print(input$select_variables_PD)
   
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
    if (length(input$select_variables_PD)>0){
      output$more_partial_dependence_plot <- renderPlot ({
        input$displayMore
        print("inside display partial dependence plot now")
        ylabel = paste0("Predicted  ", isolate(input$select_RF_end_points))
        plot_list <- list()
        data_to_plot <- rf_model_input$data_rfr
        variable_max <- max(data_to_plot[,isolate(input$select_variables_PD)],na.rm=TRUE)
        x_axis_max <- variable_max*isolate(input$PD_X_axis_scale)
        par.p <- partial(rf_model_output$rfr_ranger, train = rf_model_input$data_rfr, which.class=2,
                         pred.var=isolate(input$select_variables_PD), chull= T)
        one_plot <- autoplot(par.p, contour = TRUE, size=1,
                             ylab=ylabel, xlab=isolate(input$select_variables_PD))+
          theme_bw()+
          theme(text=element_text(size=15,face = "bold", color="blue"))+
          xlim(c(0,x_axis_max))
        print(one_plot)
        
      }) # renderPlot end
    } # if loop end
    
  })
  
  ############### R code for display random forest prediction map for the testing dataset start #########################
  
  ## removed this feature (requested in the final round of revision in September-2022)
<<<<<<< HEAD
=======
 
 # observeEvent(input$showPredictionMap, {
 #   
 #             output$display_rf_testing_prediction_map <- renderUI({
 #                                                 withSpinner(leafletOutput("rf_testing_prediction_map",width="500px",height="400px"),type=2)
 #                                                 })
 #             output$display_rf_testing_observation_map <- renderUI({
 #               withSpinner(leafletOutput("rf_testing_observation_map",width="500px",height="400px"),type=2)
 #             })
 #             
 #             output$display_rf_testing_difference_map <- renderUI({
 #               withSpinner(leafletOutput("rf_testing_difference_map",width="500px",height="400px"),type=2)
 #             })
 #      
 #      load("./Data/all_HU8_shapes.RData")
 #      print(nrow(rf_model_input$testing_dataset))
 #      obs1 = subset(rf_model_input$testing_dataset, select = input$select_RF_end_points)
 #      new_dataset = rf_model_input$testing_dataset[,!(names(rf_model_input$testing_dataset) %in% c(input$select_RF_end_points))] 
 #      
 #     # save(new_dataset,file="./Random_Forest_testing_dataset.RData")
 #      pred_testing = predict(rf_model_output$rfr_ranger, data = new_dataset)
 #      pred1 = as.data.frame(pred_testing$predictions)
 #      prediction_name = paste0(input$select_RF_end_points,".Predictions")
 #      names(pred1)[1]= prediction_name
 #      pred1$LAGOS_Lake_ID = new_dataset$LAGOS_Lake_ID
 #      # find the corresponding HU8 code for each Lake ID
 #      HUC8_data = rf_model_input$Lake_ID_with_HUC8
 #      print(colnames(HUC8_data))
 #      HU8_idx = match(pred1$LAGOS_Lake_ID,HUC8_data$LAGOS_Lake_ID)
 #      print(length(HU8_idx))
 #      pred1$HU8 = HUC8_data$HU8[HU8_idx]
 #      obs1$HU8 = HUC8_data$HU8[HU8_idx]
 #      
 #     # save(pred1,file="./Random_Forest_pred1.RData")
 #      
 #      #Mean concentration by HUC8
 #      pred2 = aggregate(pred1[,1],list(pred1$HU8),mean)
 #      obs2 = aggregate(obs1[,1],list(obs1$HU8),mean)
 #      names(pred2)[1]= "HU8"
 #      names(obs2)[1]="HU8"
 #      # Put leading zeros back on TP dataset
 #      pred2$huc8 = formatC(as.numeric(as.character(pred2$HU8)), width = 8, format = "d",flag = "0") # add zero in front of any
 #      obs2$huc8 = pred2$huc8
 #      print(length(pred2))
 #      myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
 #      myjoinedObs = merge(joined_HU8,obs2,by="huc8",duplicateGeoms=TRUE)
 #      rm(joined_HU8)
 #      print(length(myjoinedPred))
 #      #save(myjoinedPred,file="./joinedPred.RData")
 #      ## remove those polygons with NA predictions
 #      myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="x",margin=1)
 #      myjoinedObs_to_map <- sp.na.omit(myjoinedObs,col.name="x",margin=1)
 #      print(length(myjoinedPred_to_map))
 #      myjoinedDiff_to_map <- myjoinedObs_to_map
 #      myjoinedDiff_to_map$x <-myjoinedPred_to_map$x - myjoinedObs_to_map$x
 #      color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$x)
 #      color_HUC8_obs = colorNumeric(palette=input$select_color,domain=myjoinedObs_to_map$x)
 #      color_HUC8_diff = colorNumeric(palette="RdBu",domain=myjoinedDiff_to_map$x)
 #      #print(color_HUC8)
 #      
 #      tag.map.title <- tags$style(HTML("
 #                       .leaflet-control.map-title { 
 #                        transform: translate(-50%,20%);
 #                        position: fixed !important;
 #                        left: 50%;
 #                        text-align: center;
 #                        padding-left: 10px; 
 #                        padding-right: 10px; 
 #                        background: rgba(255,255,255,0.75);
 #                        font-weight: bold;
 #                        font-size: 28px;
 #                         }
 #                         "))
 #      
 #      #title_obs <- tags$div(tag.map.title, HTML("Observed"))  
 #      
 #      #title_pred <- tags$div(tag.map.title, HTML("Predicted"))  
 #      
 #      #title_diff <- tags$div(tag.map.title, HTML("Differences (Predicted-Observed)"))
 #      
 #        output$pred_title <- renderText({
 #          HTML("Predicted")
 #        })
 #      
 #        output$rf_testing_prediction_map <- renderLeaflet ({
 #        print("inside display random forest prediction map now")
 #       myMap <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
 #                setView(lng=-78.8,lat=43,zoom=5) %>%
 #                addPolygons(data=myjoinedPred_to_map,
 #                     fillColor = ~color_HUC8(x),  
 #                     color="red",
 #                     smoothFactor = 0.5,
 #                     fillOpacity = 1,
 #                     weight=1,
 #                     popup=~huc8
 #                ) %>% # addPolygons end
 #                addLegend(pal=color_HUC8,values=myjoinedPred_to_map$x,position='bottomright',title=input$select_RF_end_points,opacity=1)
 #                #addControl(title_pred,position = "center")
 #        
 #      }) # renderLeaflet end
 #      
 #        output$obs_title <- renderText({
 #          HTML("Observed")
 #        })
 #      
 #      output$rf_testing_observation_map <- renderLeaflet ({
 #        print("inside display random forest observation map now")
 #        myMap <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
 #                 setView(lng=-78.8,lat=43,zoom=5) %>%
 #          addPolygons(data=myjoinedObs_to_map,
 #                      fillColor = ~color_HUC8(x),  
 #                      color="red",
 #                      smoothFactor = 0.5,
 #                      fillOpacity = 1,
 #                      weight=1,
 #                      popup=~huc8
 #          ) %>% # addPolygons end
 #          addLegend(pal=color_HUC8,values=myjoinedPred_to_map$x,position='bottomright',title=input$select_RF_end_points,opacity=1) 
 #          #addControl(title_obs,position = "center")
 #        
 #      }) # renderLeaflet end
 #      
 #      output$diff_title <- renderText({
 #        HTML("Differences (Predicted-Observed)")
 #      })
 #      
 #      output$rf_testing_difference_map <- renderLeaflet ({
 #        print("inside display random forest difference map now")
 #        myMap <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
 #                 setView(lng=-78.8,lat=43,zoom=5) %>%
 #          addPolygons(data=myjoinedDiff_to_map,
 #                      fillColor = ~color_HUC8_diff(x),  
 #                      color="red",
 #                      smoothFactor = 0.5,
 #                      fillOpacity = 1,
 #                      weight=1,
 #                      popup=~huc8
 #          ) %>% # addPolygons end
 #          addLegend(pal=color_HUC8_diff,values=myjoinedDiff_to_map$x,position='bottomright',title=input$select_RF_end_points,opacity=1) 
 #          #addControl(title_diff,position = "center")
 #      }) # renderLeaflet end
 #   
 # })
 
 ############### R code for display random forest prediction map for the testing dataset end #########################
 
 
 ############### R code for display random forest prediction map for the uploaded new dataset start ##################
 
 
 # Make Default RF Prediction Map - observeEvent1
 observeEvent(input$showModelPredictionMapNew, {
   
   output$ShowRFModelPredictionMapNew <- renderUI({
     withSpinner(plotOutput("rf_prediction_map_new",width="800px",height="600px"),type=2)
   })
   load("./Data/all_HU8_shapes.RData")
   my_fileName <- paste("./Data/HUC8",isolate(input$predict_year),isolate(input$predict_month),isolate(input$lake_maxDepth),"MaxDepth.RData",sep="_")
   print(my_fileName)
   if (file.exists(my_fileName)){
     load(my_fileName)
     confirm_message = paste0("a new dataset: ",my_fileName," is loaded to make predictions.")
     shinyalert("",confirm_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                confirmButtonText="OK",inputId = "confirm1")  
   }else{
     no_file_message = paste0("new dataset: ",my_fileName," does not exist in the data folder.")
     shinyalert("Alert",no_file_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                confirmButtonText="OK",inputId = "noFile1")  
   }
   
   HUC8_newData = na.roughfix(HUC8_newData)
   pred_new = predict(rf_model_output$rfr_ranger, data = HUC8_newData)
   pred2 = as.data.frame(pred_new$predictions)
   names(pred2)[1]="predicted"
   #save(pred2,file="./test_RF_pred2.RData")
   ## convert LogTP and LogTN into TP and TN
   if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="LogTN"){
     pred2$converted=exp(pred2$predicted)
     names(pred2)[2]="HU8"
   }else{
     names(pred2)[1]="HU8"
   }
   
   pred2$huc8 = formatC(as.numeric(as.character(HUC8_newData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
   to_download$RF_prediction = pred2
   if (input$select_RF_end_points=="LogTP"){
     names(to_download$RF_prediction)[1:2]=c("LogTP","TP")
     name_in_plot_title = "TP"
   }else if (input$select_RF_end_points=="LogTN"){
     names(to_download$RF_prediction)[1:2]=c("LogTN","TN")
     name_in_plot_title = "TN"
   }else{
     names(to_download$RF_prediction)[1]=input$select_RF_end_points
     name_in_plot_title = input$select_RF_end_points
   }
   #print(length(pred2))
   myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
   save(myjoinedPred,file="./test_myjoinedPred.RData")
   #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) #YD commented this line out because Funtion is deprecated
   myjoinedPred_to_map <- st_as_sf(myjoinedPred)
   #print(length(myjoinedPred_to_map))
   max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8)))
   min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
   updateSliderInput(session,"RF_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
   #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8)
   
   #print(color_HUC8)
   rm(joined_HU8,HUC8_newData,myjoinedPred)
   ##YD added
   myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
   output$rf_prediction_map_new <- renderPlot ({
     
     print("inside rendering the random forest prediction HUC8 map now...")
     
     to_download$RF_map <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
       scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$RF_color_legend_range[1],input$RF_color_legend_range[2]))+
       theme(panel.background = element_blank())+ # remove grey background
       theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
       theme(legend.title=element_text(size=14))+
       theme(legend.text=element_text(size=14))+
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$RF_map)
     
   })
   
 }) #observeEvent1 end
 
 
 #-------------------------MJP Uncommented out this------------------------------------------
 # This was originally located directly above observeEvent1
 uploaded_rf_new_data <- eventReactive(input$uploaded_rf_newDataset,{ # mjp2 should be loaded was load_rf_newDataset
   newUserData<-import_raw_data(input$uploaded_rf_newDataset$datapath,"csv",has_header=TRUE)
   
   #e = new.env()
   #name <- load(input$loaded_rf_newDataset$datapath,envir = e)
   #newUserData <- e[[name]]
   #print(nrow(newData))
   return(newUserData)
 })
 
 observe(uploaded_rf_new_data())
 
 observeEvent(input$dataInfo,{
   shinyjs::runjs("swal.close();")
 })
 
 # -------------------------------------------------------------------------------------------
 
 
 # Make RF Prediction Map with new Variables - observeEvent2
 observeEvent(input$showModelPredictionMapNew2, {
   
   output$ShowRFModelPredictionMapNew2 <- renderUI({
     withSpinner(plotOutput("rf_prediction_map_new2",width="800px",height="600px"),type=2)
   })
   load("./Data/all_HU8_shapes.RData")
   
   
   #download_dir = "C:/Users/mpennino/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/"
   #test = read.csv(paste0(download_dir,"HUC8_2007_7_Average_MaxDepth2.csv"))
   #test2 = na.omit(test)
   #test2 = na.roughfix(test)
   #newUserData = read.csv(paste0(download_dir,"HUC8_2007_7_Average_MaxDepth3.csv"))
   newUserData <- uploaded_rf_new_data()  
   
   # Select out specific variables
   #selected_variables = c("NLCD_Dummy", "NLCD_pct_forest2","NLCD_pct_farm2")
   
   #method 1
   #selected_variables = selected_variables_for_regression()
   #HUC8_newUserData = newUserData[,c('HUC8',selected_variables)]
   
   # method 2
   # imprtnce = as.data.frame(importance(rf_model_output$rfr_ranger))
   # names(imprtnce)[1] = "PercIncMSE"
   # setDT(imprtnce, keep.rownames = TRUE)[]
   # imprtnce = imprtnce[,1:2]
   # names(imprtnce)[1] = "Variable.Name"
   # HUC8_newUserData = newUserData[,c('HUC8',imprtnce$Variable.Name)]
   
   # select for variables in the model
   #HUC8_newUserData = na.omit(HUC8_newUserData)
   
   #HUC8_newUserData = subset(HUC8_newUserData,select=-c())
   HUC8_newUserData = na.roughfix(newUserData)
   pred_new = predict(rf_model_output$rfr_ranger, data = HUC8_newUserData)
   #pred_new = predict(rfr_ranger, data = HUC8_newUserData) # this version of code used for testing (MJP)
   
   pred2 = data.frame(predicted = pred_new$predictions)
   #save(pred2,file="./test_RF_pred2.RData")
   ## convert LogTP and LogTN into TP and TN, MJP --> I think in this section predicted value is converted to HU8
   if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="LogTN"){
     pred2$converted=exp(pred2$predicted)
     names(pred2)[2]="HU8"
   }else{
     names(pred2)[1]="HU8"
   }
   
   #names(pred2)[1]="HU8" # use if running this as test
   
   pred2$huc8 = formatC(as.numeric(as.character(HUC8_newUserData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
   to_download$RF_prediction2 = pred2
   
   if (input$select_RF_end_points=="LogTP"){
     names(to_download$RF_prediction2)[1:2]=c("LogTP","TP")
     name_in_plot_title = "TP"
   }else if (input$select_RF_end_points=="LogTN"){
     names(to_download$RF_prediction2)[1:2]=c("LogTN","TN")
     name_in_plot_title = "TN"
   }else{
     names(to_download$RF_prediction2)[1]=input$select_RF_end_points
     name_in_plot_title = input$select_RF_end_points
   }
   
   #print(length(pred2))
   myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE) # joined_HU8 comes from all_HU8_shapes.RData
   #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) # original, YD commented this line out because function deprecated
   myjoinedPred_to_map <- st_as_sf(myjoinedPred) # original
   
   #joined_HU8_ = st_as_sf(joined_HU8) # mjp change
   #myjoinedPred = merge(joined_HU8_[,c('huc8','areasqkm','geometry')],pred2,by="huc8",duplicateGeoms=TRUE) # mjp version
   #myjoinedPred_to_map = na.roughfix(myjoinedPred) # mjp version instead of sp.na.omit
   #myjoinedPred_to_map = myjoinedPred %>% drop_na(HU8) # mjp version
   
   #print(length(myjoinedPred_to_map))
   max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8))) # original
   min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
   #max_HU8 = round(1.2*max(myjoinedPred_to_map$predicted)) # mjp
   ##YD changed
   updateSliderInput(session,"RF_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
   #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8)
   
   #print(color_HUC8)
   rm(joined_HU8,HUC8_newUserData,myjoinedPred)
   ##YD added
   myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
   output$rf_prediction_map_new2 <- renderPlot ({
     
     print("inside rendering the random forest prediction HUC8 map now...")
     
     to_download$RF_map2 <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
       scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$RF_color_legend_range[1],input$RF_color_legend_range[2]))+
       theme(panel.background = element_blank())+ # remove grey background
       theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
       theme(legend.title=element_text(size=14))+
       theme(legend.text=element_text(size=14))+
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$RF_map2)
     
   })
   
 }) #observeEvent2 end
 
 # Original
 output$saveRFPredictionResults <- downloadHandler(
   filename = function(){paste(input$select_RF_end_points,"_random_forest_model_prediction_results_",Sys.Date(),".csv",sep="")},
   content = function(file){
     write.csv(to_download$RF_prediction,file,row.names=FALSE)
   }
 )
 
 # MJP added this for the RF map for new user dataset
 output$saveRFPredictionResults2 <- downloadHandler(
   filename = function(){paste(input$select_RF_end_points,"_random_forest_model_prediction_results_",Sys.Date(),".csv",sep="")},
   content = function(file){
     write.csv(to_download$RF_prediction2,file,row.names=FALSE)
   }
 )
 
 # Original
 output$saveRFPredictionMap <- downloadHandler(
   filename = function(){paste("random_forest_model_prediction_map_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$RF_map,dpi=300,width=12,height=10)
   }
   #saveWidget(to_download$LR_map,my_filename,selfcontained=FALSE)
 )
 
 # MJP added 
 output$saveRFPredictionMap2 <- downloadHandler(
   filename = function(){paste("random_forest_model_prediction_map_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$RF_map2,dpi=300,width=12,height=10)
   }
   #saveWidget(to_download$LR_map,my_filename,selfcontained=FALSE)
 )
 
 #-----------------------------------------------------------------------------------------
 ## this part is added to display the prediction map after setting up a concentration (cutoff) criteria
 
 ## FOR DEFAULT DATASET
 
 observeEvent(input$showModelPredictionMapTwoColors, {
   
   output$ShowRFModelPredictionMapBiColor <- renderUI({
     withSpinner(plotOutput("rf_prediction_map_bicolor",width="800px",height="600px"),type=2)
   })
   
   load("./Data/all_HU8_shapes.RData")
   
   if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="TP"){
     myjoinedPred = merge(joined_HU8,to_download$RF_prediction,by="huc8",duplicateGeoms=TRUE)
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) # original,YD commented this line out because function deprecated
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) # mjp version
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     ##YD added
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
    #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
     name_in_plot_title = "TP"
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$cutoff_value),]
   }else if (input$select_RF_end_points=="LogTN" | input$select_RF_end_points=="TN"){
     myjoinedPred = merge(joined_HU8,to_download$RF_prediction,by="huc8",duplicateGeoms=TRUE)
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) #YD commented this line out because function deprecated
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     ##YD added
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
     name_in_plot_title = "TN"
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$cutoff_value),]
   }
   
   rm(joined_HU8,myjoinedPred)
   
   output$rf_prediction_map_bicolor <- renderPlot ({
     
     print("inside rendering the random forest prediction HUC8 bicolor map now...")
     
     to_download$RF_bicolor_map <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       theme(panel.background = element_blank())+ # remove grey background
       labs(fill=name_in_plot_title)+
       scale_fill_manual(values=c("blue","red"))+
       theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
       theme(legend.title=element_text(size=14))+
       theme(legend.text=element_text(size=14))+
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$RF_bicolor_map)
   })
   
 })
 
 
 output$saveRFPredictionMapTwoColors <- downloadHandler(
   filename = function(){paste("random_forest_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$RF_bicolor_map,dpi=300,width=12,height=10)
   }
 )
 

 
 observeEvent(input$confirm1,{
   shinyjs::runjs("swal.close();")
 }) 
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
  
  # observeEvent(input$showPredictionMap, {
  #   
  #             output$display_rf_testing_prediction_map <- renderUI({
  #                                                 withSpinner(leafletOutput("rf_testing_prediction_map",width="500px",height="400px"),type=2)
  #                                                 })
  #             output$display_rf_testing_observation_map <- renderUI({
  #               withSpinner(leafletOutput("rf_testing_observation_map",width="500px",height="400px"),type=2)
  #             })
  #             
  #             output$display_rf_testing_difference_map <- renderUI({
  #               withSpinner(leafletOutput("rf_testing_difference_map",width="500px",height="400px"),type=2)
  #             })
  #      
  #      load("./Data/all_HU8_shapes.RData")
  #      print(nrow(rf_model_input$testing_dataset))
  #      obs1 = subset(rf_model_input$testing_dataset, select = input$select_RF_end_points)
  #      new_dataset = rf_model_input$testing_dataset[,!(names(rf_model_input$testing_dataset) %in% c(input$select_RF_end_points))] 
  #      
  #     # save(new_dataset,file="./Random_Forest_testing_dataset.RData")
  #      pred_testing = predict(rf_model_output$rfr_ranger, data = new_dataset)
  #      pred1 = as.data.frame(pred_testing$predictions)
  #      prediction_name = paste0(input$select_RF_end_points,".Predictions")
  #      names(pred1)[1]= prediction_name
  #      pred1$LAGOS_Lake_ID = new_dataset$LAGOS_Lake_ID
  #      # find the corresponding HU8 code for each Lake ID
  #      HUC8_data = rf_model_input$Lake_ID_with_HUC8
  #      print(colnames(HUC8_data))
  #      HU8_idx = match(pred1$LAGOS_Lake_ID,HUC8_data$LAGOS_Lake_ID)
  #      print(length(HU8_idx))
  #      pred1$HU8 = HUC8_data$HU8[HU8_idx]
  #      obs1$HU8 = HUC8_data$HU8[HU8_idx]
  #      
  #     # save(pred1,file="./Random_Forest_pred1.RData")
  #      
  #      #Mean concentration by HUC8
  #      pred2 = aggregate(pred1[,1],list(pred1$HU8),mean)
  #      obs2 = aggregate(obs1[,1],list(obs1$HU8),mean)
  #      names(pred2)[1]= "HU8"
  #      names(obs2)[1]="HU8"
  #      # Put leading zeros back on TP dataset
  #      pred2$huc8 = formatC(as.numeric(as.character(pred2$HU8)), width = 8, format = "d",flag = "0") # add zero in front of any
  #      obs2$huc8 = pred2$huc8
  #      print(length(pred2))
  #      myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
  #      myjoinedObs = merge(joined_HU8,obs2,by="huc8",duplicateGeoms=TRUE)
  #      rm(joined_HU8)
  #      print(length(myjoinedPred))
  #      #save(myjoinedPred,file="./joinedPred.RData")
  #      ## remove those polygons with NA predictions
  #      myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="x",margin=1)
  #      myjoinedObs_to_map <- sp.na.omit(myjoinedObs,col.name="x",margin=1)
  #      print(length(myjoinedPred_to_map))
  #      myjoinedDiff_to_map <- myjoinedObs_to_map
  #      myjoinedDiff_to_map$x <-myjoinedPred_to_map$x - myjoinedObs_to_map$x
  #      color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$x)
  #      color_HUC8_obs = colorNumeric(palette=input$select_color,domain=myjoinedObs_to_map$x)
  #      color_HUC8_diff = colorNumeric(palette="RdBu",domain=myjoinedDiff_to_map$x)
  #      #print(color_HUC8)
  #      
  #      tag.map.title <- tags$style(HTML("
  #                       .leaflet-control.map-title { 
  #                        transform: translate(-50%,20%);
  #                        position: fixed !important;
  #                        left: 50%;
  #                        text-align: center;
  #                        padding-left: 10px; 
  #                        padding-right: 10px; 
  #                        background: rgba(255,255,255,0.75);
  #                        font-weight: bold;
  #                        font-size: 28px;
  #                         }
  #                         "))
  #      
  #      #title_obs <- tags$div(tag.map.title, HTML("Observed"))  
  #      
  #      #title_pred <- tags$div(tag.map.title, HTML("Predicted"))  
  #      
  #      #title_diff <- tags$div(tag.map.title, HTML("Differences (Predicted-Observed)"))
  #      
  #        output$pred_title <- renderText({
  #          HTML("Predicted")
  #        })
  #      
  #        output$rf_testing_prediction_map <- renderLeaflet ({
  #        print("inside display random forest prediction map now")
  #       myMap <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
  #                setView(lng=-78.8,lat=43,zoom=5) %>%
  #                addPolygons(data=myjoinedPred_to_map,
  #                     fillColor = ~color_HUC8(x),  
  #                     color="red",
  #                     smoothFactor = 0.5,
  #                     fillOpacity = 1,
  #                     weight=1,
  #                     popup=~huc8
  #                ) %>% # addPolygons end
  #                addLegend(pal=color_HUC8,values=myjoinedPred_to_map$x,position='bottomright',title=input$select_RF_end_points,opacity=1)
  #                #addControl(title_pred,position = "center")
  #        
  #      }) # renderLeaflet end
  #      
  #        output$obs_title <- renderText({
  #          HTML("Observed")
  #        })
  #      
  #      output$rf_testing_observation_map <- renderLeaflet ({
  #        print("inside display random forest observation map now")
  #        myMap <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
  #                 setView(lng=-78.8,lat=43,zoom=5) %>%
  #          addPolygons(data=myjoinedObs_to_map,
  #                      fillColor = ~color_HUC8(x),  
  #                      color="red",
  #                      smoothFactor = 0.5,
  #                      fillOpacity = 1,
  #                      weight=1,
  #                      popup=~huc8
  #          ) %>% # addPolygons end
  #          addLegend(pal=color_HUC8,values=myjoinedPred_to_map$x,position='bottomright',title=input$select_RF_end_points,opacity=1) 
  #          #addControl(title_obs,position = "center")
  #        
  #      }) # renderLeaflet end
  #      
  #      output$diff_title <- renderText({
  #        HTML("Differences (Predicted-Observed)")
  #      })
  #      
  #      output$rf_testing_difference_map <- renderLeaflet ({
  #        print("inside display random forest difference map now")
  #        myMap <- leaflet() %>% addTiles() %>% addResetMapButton() %>%
  #                 setView(lng=-78.8,lat=43,zoom=5) %>%
  #          addPolygons(data=myjoinedDiff_to_map,
  #                      fillColor = ~color_HUC8_diff(x),  
  #                      color="red",
  #                      smoothFactor = 0.5,
  #                      fillOpacity = 1,
  #                      weight=1,
  #                      popup=~huc8
  #          ) %>% # addPolygons end
  #          addLegend(pal=color_HUC8_diff,values=myjoinedDiff_to_map$x,position='bottomright',title=input$select_RF_end_points,opacity=1) 
  #          #addControl(title_diff,position = "center")
  #      }) # renderLeaflet end
  #   
  # })
  
  ############### R code for display random forest prediction map for the testing dataset end #########################
  
<<<<<<< HEAD
  
  ############### R code for display random forest prediction map for the uploaded new dataset start ##################
  
  
  # Make Default RF Prediction Map - observeEvent1
  observeEvent(input$showModelPredictionMapNew, {
    
    output$ShowRFModelPredictionMapNew <- renderUI({
      withSpinner(plotOutput("rf_prediction_map_new",width="800px",height="600px"),type=2)
    })
    load("./Data/all_HU8_shapes.RData")
    my_fileName <- paste("./Data/HUC8",isolate(input$predict_year),isolate(input$predict_month),isolate(input$lake_maxDepth),"MaxDepth.RData",sep="_")
    print(my_fileName)
    if (file.exists(my_fileName)){
      load(my_fileName)
      confirm_message = paste0("a new dataset: ",my_fileName," is loaded to make predictions.")
      shinyalert("",confirm_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "confirm1")  
    }else{
      no_file_message = paste0("new dataset: ",my_fileName," does not exist in the data folder.")
      shinyalert("Alert",no_file_message,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "noFile1")  
    }
    
    HUC8_newData = na.roughfix(HUC8_newData)
    pred_new = predict(rf_model_output$rfr_ranger, data = HUC8_newData)
    pred2 = as.data.frame(pred_new$predictions)
    names(pred2)[1]="predicted"
    #save(pred2,file="./test_RF_pred2.RData")
    ## convert LogTP and LogTN into TP and TN
    if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="LogTN"){
      pred2$converted=exp(pred2$predicted)
      names(pred2)[2]="HU8"
    }else{
      names(pred2)[1]="HU8"
    }
    
    pred2$huc8 = formatC(as.numeric(as.character(HUC8_newData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
    to_download$RF_prediction = pred2
    if (input$select_RF_end_points=="LogTP"){
      names(to_download$RF_prediction)[1:2]=c("LogTP","TP")
      name_in_plot_title = "TP"
    }else if (input$select_RF_end_points=="LogTN"){
      names(to_download$RF_prediction)[1:2]=c("LogTN","TN")
      name_in_plot_title = "TN"
    }else{
      names(to_download$RF_prediction)[1]=input$select_RF_end_points
      name_in_plot_title = input$select_RF_end_points
    }
    #print(length(pred2))
    myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE)
    save(myjoinedPred,file="./test_myjoinedPred.RData")
    #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) #YD commented this line out because Funtion is deprecated
    myjoinedPred_to_map <- st_as_sf(myjoinedPred)
    #print(length(myjoinedPred_to_map))
    max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8)))
    min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
    updateSliderInput(session,"RF_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
    #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8)
    
    #print(color_HUC8)
    rm(joined_HU8,HUC8_newData,myjoinedPred)
    ##YD added
    myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
    output$rf_prediction_map_new <- renderPlot ({
      
      print("inside rendering the random forest prediction HUC8 map now...")
      
      to_download$RF_map <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
        scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$RF_color_legend_range[1],input$RF_color_legend_range[2]))+
        theme(panel.background = element_blank())+ # remove grey background
        theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
        theme(legend.title=element_text(size=14))+
        theme(legend.text=element_text(size=14))+
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$RF_map)
      
    })
    
  }) #observeEvent1 end
  
  
  #-------------------------MJP Uncommented out this------------------------------------------
  # This was originally located directly above observeEvent1
  uploaded_rf_new_data <- eventReactive(input$uploaded_rf_newDataset,{ # mjp2 should be loaded was load_rf_newDataset
    newUserData<-import_raw_data(input$uploaded_rf_newDataset$datapath,"csv",has_header=TRUE)
    
    #e = new.env()
    #name <- load(input$loaded_rf_newDataset$datapath,envir = e)
    #newUserData <- e[[name]]
    #print(nrow(newData))
    return(newUserData)
  })
  
  observe(uploaded_rf_new_data())
  
  observeEvent(input$dataInfo,{
    shinyjs::runjs("swal.close();")
  })
  
  # -------------------------------------------------------------------------------------------
  
  
  # Make RF Prediction Map with new Variables - observeEvent2
  observeEvent(input$showModelPredictionMapNew2, {
    
    output$ShowRFModelPredictionMapNew2 <- renderUI({
      withSpinner(plotOutput("rf_prediction_map_new2",width="800px",height="600px"),type=2)
    })
    load("./Data/all_HU8_shapes.RData")
    
    
    #download_dir = "C:/Users/mpennino/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/"
    #test = read.csv(paste0(download_dir,"HUC8_2007_7_Average_MaxDepth2.csv"))
    #test2 = na.omit(test)
    #test2 = na.roughfix(test)
    #newUserData = read.csv(paste0(download_dir,"HUC8_2007_7_Average_MaxDepth3.csv"))
    newUserData <- uploaded_rf_new_data()  
    
    # Select out specific variables
    #selected_variables = c("NLCD_Dummy", "NLCD_pct_forest2","NLCD_pct_farm2")
    
    #method 1
    #selected_variables = selected_variables_for_regression()
    #HUC8_newUserData = newUserData[,c('HUC8',selected_variables)]
    
    # method 2
    # imprtnce = as.data.frame(importance(rf_model_output$rfr_ranger))
    # names(imprtnce)[1] = "PercIncMSE"
    # setDT(imprtnce, keep.rownames = TRUE)[]
    # imprtnce = imprtnce[,1:2]
    # names(imprtnce)[1] = "Variable.Name"
    # HUC8_newUserData = newUserData[,c('HUC8',imprtnce$Variable.Name)]
    
    # select for variables in the model
    #HUC8_newUserData = na.omit(HUC8_newUserData)
    
    #HUC8_newUserData = subset(HUC8_newUserData,select=-c())
    HUC8_newUserData = na.roughfix(newUserData)
    pred_new = predict(rf_model_output$rfr_ranger, data = HUC8_newUserData)
    #pred_new = predict(rfr_ranger, data = HUC8_newUserData) # this version of code used for testing (MJP)
    
    pred2 = data.frame(predicted = pred_new$predictions)
    #save(pred2,file="./test_RF_pred2.RData")
    ## convert LogTP and LogTN into TP and TN, MJP --> I think in this section predicted value is converted to HU8
    if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="LogTN"){
      pred2$converted=exp(pred2$predicted)
      names(pred2)[2]="HU8"
    }else{
      names(pred2)[1]="HU8"
    }
    
    #names(pred2)[1]="HU8" # use if running this as test
    
    pred2$huc8 = formatC(as.numeric(as.character(HUC8_newUserData$HUC8)), width = 8, format = "d",flag = "0") # add zero in front of any
    to_download$RF_prediction2 = pred2
    
    if (input$select_RF_end_points=="LogTP"){
      names(to_download$RF_prediction2)[1:2]=c("LogTP","TP")
      name_in_plot_title = "TP"
    }else if (input$select_RF_end_points=="LogTN"){
      names(to_download$RF_prediction2)[1:2]=c("LogTN","TN")
      name_in_plot_title = "TN"
    }else{
      names(to_download$RF_prediction2)[1]=input$select_RF_end_points
      name_in_plot_title = input$select_RF_end_points
    }
    
    #print(length(pred2))
    myjoinedPred = merge(joined_HU8,pred2,by="huc8",duplicateGeoms=TRUE) # joined_HU8 comes from all_HU8_shapes.RData
    #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) # original, YD commented this line out because function deprecated
    myjoinedPred_to_map <- st_as_sf(myjoinedPred) # original
    
    #joined_HU8_ = st_as_sf(joined_HU8) # mjp change
    #myjoinedPred = merge(joined_HU8_[,c('huc8','areasqkm','geometry')],pred2,by="huc8",duplicateGeoms=TRUE) # mjp version
    #myjoinedPred_to_map = na.roughfix(myjoinedPred) # mjp version instead of sp.na.omit
    #myjoinedPred_to_map = myjoinedPred %>% drop_na(HU8) # mjp version
    
    #print(length(myjoinedPred_to_map))
    max_HU8 = round(1.2*max(na.omit(myjoinedPred_to_map$HU8))) # original
    min_HU8 = round(min(na.omit(myjoinedPred_to_map$HU8)))
    #max_HU8 = round(1.2*max(myjoinedPred_to_map$predicted)) # mjp
    ##YD changed
    updateSliderInput(session,"RF_color_legend_range",min=0,max=max_HU8,value=c(round(0.8*min_HU8),round(0.8*max_HU8))) 
    #color_HUC8 = colorNumeric(palette=input$select_color,domain=myjoinedPred_to_map$HU8)
    
    #print(color_HUC8)
    rm(joined_HU8,HUC8_newUserData,myjoinedPred)
    ##YD added
    myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$HU8),]
    output$rf_prediction_map_new2 <- renderPlot ({
      
      print("inside rendering the random forest prediction HUC8 map now...")
      
      to_download$RF_map2 <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(myjoinedPred_to_map,mapping=aes(fill=HU8),colour="red",alpha=0.7,size=0.05)+
        scale_fill_gradientn(colors=viridis_pal()(9),name=paste0(name_in_plot_title," (\u03bcg/L)"),limits=c(input$RF_color_legend_range[1],input$RF_color_legend_range[2]))+
        theme(panel.background = element_blank())+ # remove grey background
        theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
        theme(legend.title=element_text(size=14))+
        theme(legend.text=element_text(size=14))+
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$RF_map2)
      
    })
    
  }) #observeEvent2 end
  
  # Original
  output$saveRFPredictionResults <- downloadHandler(
    filename = function(){paste(input$select_RF_end_points,"_random_forest_model_prediction_results_",Sys.Date(),".csv",sep="")},
    content = function(file){
      write.csv(to_download$RF_prediction,file,row.names=FALSE)
    }
  )
  
  # MJP added this for the RF map for new user dataset
  output$saveRFPredictionResults2 <- downloadHandler(
    filename = function(){paste(input$select_RF_end_points,"_random_forest_model_prediction_results_",Sys.Date(),".csv",sep="")},
    content = function(file){
      write.csv(to_download$RF_prediction2,file,row.names=FALSE)
    }
  )
  
  # Original
  output$saveRFPredictionMap <- downloadHandler(
    filename = function(){paste("random_forest_model_prediction_map_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$RF_map,dpi=300,width=12,height=10)
    }
    #saveWidget(to_download$LR_map,my_filename,selfcontained=FALSE)
  )
  
  # MJP added 
  output$saveRFPredictionMap2 <- downloadHandler(
    filename = function(){paste("random_forest_model_prediction_map_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$RF_map2,dpi=300,width=12,height=10)
    }
    #saveWidget(to_download$LR_map,my_filename,selfcontained=FALSE)
  )
  
  #-----------------------------------------------------------------------------------------
  ## this part is added to display the prediction map after setting up a concentration (cutoff) criteria
  
  ## FOR DEFAULT DATASET
  
  observeEvent(input$showModelPredictionMapTwoColors, {
    
    output$ShowRFModelPredictionMapBiColor <- renderUI({
      withSpinner(plotOutput("rf_prediction_map_bicolor",width="800px",height="600px"),type=2)
    })
    
    load("./Data/all_HU8_shapes.RData")
    
    if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="TP"){
      myjoinedPred = merge(joined_HU8,to_download$RF_prediction,by="huc8",duplicateGeoms=TRUE)
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) # original,YD commented this line out because function deprecated
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="HU8",margin=1) # mjp version
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      ##YD added
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
      #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
      name_in_plot_title = "TP"
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$cutoff_value),]
    }else if (input$select_RF_end_points=="LogTN" | input$select_RF_end_points=="TN"){
      myjoinedPred = merge(joined_HU8,to_download$RF_prediction,by="huc8",duplicateGeoms=TRUE)
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) #YD commented this line out because function deprecated
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      ##YD added
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
      name_in_plot_title = "TN"
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$cutoff_value),]
    }
    
    rm(joined_HU8,myjoinedPred)
    
    output$rf_prediction_map_bicolor <- renderPlot ({
      
      print("inside rendering the random forest prediction HUC8 bicolor map now...")
      
      to_download$RF_bicolor_map <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        theme(panel.background = element_blank())+ # remove grey background
        labs(fill=name_in_plot_title)+
        scale_fill_manual(values=c("blue","red"))+
        theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
        theme(legend.title=element_text(size=14))+
        theme(legend.text=element_text(size=14))+
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$RF_bicolor_map)
    })
    
  })
  
  
  output$saveRFPredictionMapTwoColors <- downloadHandler(
    filename = function(){paste("random_forest_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$RF_bicolor_map,dpi=300,width=12,height=10)
    }
  )
  
  
  
  observeEvent(input$confirm1,{
    shinyjs::runjs("swal.close();")
  }) 
  
  observeEvent(input$noFile1,{
    shinyjs::runjs("swal.close();")
  }) 
  
  #-----------------------------------------------------------------------------------------
  ## this part is added to display the prediction map after setting up a concentration (cutoff) criteria
  ## FOR NEW USER DATASET (added by MJP)
  
  observeEvent(input$showModelPredictionMapTwoColors2, {
    
    output$ShowRFModelPredictionMapBiColor2 <- renderUI({
      withSpinner(plotOutput("rf_prediction_map_bicolor2",width="800px",height="600px"),type=2)
    })
    
    load("./Data/all_HU8_shapes.RData")
    
    
    if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="TP"){
      myjoinedPred = merge(joined_HU8,to_download$RF_prediction2,by="huc8",duplicateGeoms=TRUE) # original
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) # original,YD commented this line out because function deprecated
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
      ##YD added
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
      name_in_plot_title = "TP"
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$cutoff_value),]
    }else if (input$select_RF_end_points=="LogTN" | input$select_RF_end_points=="TN"){
      myjoinedPred = merge(joined_HU8,to_download$RF_prediction2,by="huc8",duplicateGeoms=TRUE)
      #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) # YD commented this line out
      myjoinedPred_to_map <- st_as_sf(myjoinedPred)
      name_in_plot_title = "TN"
      ##YD added
      myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
      above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$cutoff_value),]
      below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$cutoff_value),]
    }
    
    rm(joined_HU8,myjoinedPred)
    
    output$rf_prediction_map_bicolor2 <- renderPlot ({
      
      print("inside rendering the random forest prediction HUC8 bicolor map now...")
      
      to_download$RF_bicolor_map2 <- ggplot()+
        ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
        geom_sf(data=NE,color="black",fill=NA,size=0.3)+
        geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
        theme(panel.background = element_blank())+ # remove grey background
        labs(fill=name_in_plot_title)+
        scale_fill_manual(values=c("blue","red"))+
        theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
        theme(legend.title=element_text(size=14))+
        theme(legend.text=element_text(size=14))+
        theme(axis.text = element_blank())+ # remove axis text
        theme(axis.ticks = element_blank()) # remove axis text
      print(to_download$RF_bicolor_map2)
    })
    
  })
  
  
  output$saveRFPredictionMapTwoColors2 <- downloadHandler(
    filename = function(){paste("random_forest_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
    content = function(file){
      ggsave(file,plot=to_download$RF_bicolor_map2,dpi=300,width=12,height=10)
    }
  )
  
  
  
  observeEvent(input$confirm1,{
    shinyjs::runjs("swal.close();")
  }) 
  
  observeEvent(input$noFile1,{
    shinyjs::runjs("swal.close();")
  }) 
=======
   if (input$select_RF_end_points=="LogTP" | input$select_RF_end_points=="TP"){
     myjoinedPred = merge(joined_HU8,to_download$RF_prediction2,by="huc8",duplicateGeoms=TRUE) # original
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TP",margin=1) # original,YD commented this line out because function deprecated
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     #save(myjoinedPred_to_map,file="./test_prediction_map.RData")
     ##YD added
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TP),]
     name_in_plot_title = "TP"
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP >= isolate(input$cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TP < isolate(input$cutoff_value),]
   }else if (input$select_RF_end_points=="LogTN" | input$select_RF_end_points=="TN"){
     myjoinedPred = merge(joined_HU8,to_download$RF_prediction2,by="huc8",duplicateGeoms=TRUE)
     #myjoinedPred_to_map <- sp.na.omit(myjoinedPred,col.name="TN",margin=1) # YD commented this line out
     myjoinedPred_to_map <- st_as_sf(myjoinedPred)
     name_in_plot_title = "TN"
     ##YD added
     myjoinedPred_to_map <- myjoinedPred_to_map[!is.na(myjoinedPred_to_map$TN),]
     above_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN >= isolate(input$cutoff_value),]
     below_cutoff <- myjoinedPred_to_map[myjoinedPred_to_map$TN < isolate(input$cutoff_value),]
   }
   
   rm(joined_HU8,myjoinedPred)
   
   output$rf_prediction_map_bicolor2 <- renderPlot ({
     
     print("inside rendering the random forest prediction HUC8 bicolor map now...")
     
     to_download$RF_bicolor_map2 <- ggplot()+
       ggtitle(paste0(name_in_plot_title," prediction in HUC8"))+
       geom_sf(data=NE,color="black",fill=NA,size=0.3)+
       geom_sf(below_cutoff,mapping=aes(fill=paste0("< ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       geom_sf(above_cutoff,mapping=aes(fill=paste0(">= ",isolate(input$cutoff_value)," \u03bcg/L")),color="grey",alpha=0.8,size=0.05)+
       theme(panel.background = element_blank())+ # remove grey background
       labs(fill=name_in_plot_title)+
       scale_fill_manual(values=c("blue","red"))+
       theme(plot.title=element_text(hjust=0.5,size=14))+ # put the title at center
       theme(legend.title=element_text(size=14))+
       theme(legend.text=element_text(size=14))+
       theme(axis.text = element_blank())+ # remove axis text
       theme(axis.ticks = element_blank()) # remove axis text
     print(to_download$RF_bicolor_map2)
   })
   
 })
 
 
 output$saveRFPredictionMapTwoColors2 <- downloadHandler(
   filename = function(){paste("random_forest_model_prediction_map_in_bicolor_",Sys.Date(),".png",sep="")},
   content = function(file){
     ggsave(file,plot=to_download$RF_bicolor_map2,dpi=300,width=12,height=10)
   }
 )
 
 
 
 observeEvent(input$confirm1,{
   shinyjs::runjs("swal.close();")
 }) 
 
 observeEvent(input$noFile1,{
   shinyjs::runjs("swal.close();")
 }) 
>>>>>>> 91887b4ccbdaa5b1841c7898cf124747ba10ddee
  
  ##################################### model 2: random forest model code end #######################################
  
  #######################################################################################################################
  ######################################## server code end for tabPanel "run models"#####################################
  #######################################################################################################################
  
  ######################################### User Guide code start ####################################################
  
  output$user_guide <- renderText({
    filePath <-"www/User_Guide_Nutrient_Explorer.txt"
    fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
    fileText
  })
  
  
  session$onSessionEnded(cleanMem)
  session$onSessionEnded(stopApp)
}
