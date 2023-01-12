# This is the user-interface definition of Total Phosphorus (TP) Dashboard Shiny web application. 
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyBS)
#library(shinythemes)
library(shinydashboard)
library(shinyalert)
library(ggplot2)
library(ggthemes)
library(DT)
library(plotly)
library(shinycssloaders)

options(spinner.color.background="#ffffff",spinner.size=1)
js1 <- paste0(c(
  "Selectize.prototype.selectall = function(){",
  "  var self = this;",
  "  self.setValue(Object.keys(self.options));",
  "}"), 
  collapse = "\n")

shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  useShinyalert(),
 
  
  mainPanel(width = 12,
            
            tags$style("#big-heading {font-size:15px;color:black;font-style:bold;display:block; }"),
            tags$style(HTML(".shiny-notification{
                            position:fixed;
                            top:calc(50%);
                            left:calc(50%);}")),
            
            
            # spacing
            fluidRow(p()),
            
            # top controls  
            fluidRow(
              
              column(width = 12,
                     actionButton('reset all', label = img(src = "nutrient_explorer_header.png", width = '100%'), width = '100%')
              )
              
            ),
            
  fluidRow(p(),
    
   tabsetPanel(
      tabPanel("Load Data",
              fluidRow(
                column(width = 12,
                       
                       sidebarLayout(
                         sidebarPanel(width=2,
                                      br(),
                                      radioButtons("dataset_option",label="Dataset options",choices=c("LAGOS test","LAGOS"),selected="LAGOS test",inline=FALSE),
                                      br(),
                                      fileInput("uploaded_dataset",label="Upload new dataset",multiple=FALSE,
                                                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                      br(),
                                      actionButton(inputId="loadDataset", label="Load this dataset",style="color:black;background-color:grey"), 
                                      br(),
                                      uiOutput("select"), 
                                      br(),
                                      uiOutput("display_button"),
                                      br(),
                                      uiOutput("download_button"),
                                      br(),
                                      br(),
                                      #uiOutput("display_list") 
                                     
                                      
                                      
                         ),
                         mainPanel(width=10,align="center",
                                   tags$head(tags$style("#summary_table_overall_text,#summary_table_text,#summary_table_text_cat{
                                        color: red;
                                        font-size: 16px;
                                        font-style: bold;
                                       }
                                       ")),
                                   #fluidRow(column(width=8,uiOutput("display_map_button"))),
                                   fluidRow(
                                     column(width=8, align="left",uiOutput("display_map")),
                                     column(width=2, align="left",uiOutput("end_points_option"),
                                                                  uiOutput("color_palette_option")),
                                   ),
                                   hr(),
                                   fluidRow(column(width=8,uiOutput("display_table_button"))
                                   ),#fluidRow end
                                   br(),
                                   shinyjs::hidden(
                                   div(id="summaryCP",   
                                   conditionalPanel(
                                   id = "summary_table_panel",
                                   condition = "input$displayTable > 0",  
                                   fluidRow(column(width=10,align="left",uiOutput("display_overall_text")),
                                            column(width=10,uiOutput("display_table")),
                                            column(width=2,align="left",br(),uiOutput("display_variable_groups")),
                                            column(width=10,align="left",uiOutput("display_text_continuous"))
                                   ),#fluidRow end  
                                   fluidRow(column(width=10,uiOutput("display_table_cat")),
                                            column(width=10,align="left",uiOutput("display_text_cat"))
                                   )# fluidRow end
                                   ) #conditionalPanel end
                                   ) # div close
                                   ) # shinyjs end
                                   
                         ) # mainPanel end
                         
                       ) # sidebarLayout end
                       
                ), #column close
                
                  conditionalPanel(
                  id = "content_table_panel",
                  condition = "input$displayid %% 2 != 0",
                  column(width = 12,
                       tableOutput("rawcontents")
                  ) #column close
                ), # conditionalPanel end
              ) #fluidRow close
              
      ),
      
      tabPanel("Explore Data",
               fluidRow(p()),
               fluidRow(
                 tabsetPanel( id="tabset",
                              tags$head(tags$style(HTML(".radio-inline {margin-right: 40px;}"))),
                              tabPanel("Summary Info", value="tab1",br(),
                                       column(width=8,uiOutput("display_plot1")),
                                       column(width=4,align="left",uiOutput("end_points_option_in_summary"))),
                              tabPanel("Time Series", value="tab2",br(),
                                       column(width=12,align="center",uiOutput("end_points_option_in_series")),
                                       br(),
                                       column(width=6,uiOutput("display_plot2a")),
                                       column(width=6,uiOutput("display_plot2b"))
                                       ),
                              tabPanel("Endpoint HUC2 Summaries", value="tab3",
                                       tabsetPanel(id="tab3b",
                                                   tabPanel("Bar plot/Pie Chart",value = "sb1",br(),
                                                            column(width=12,align="center",uiOutput("end_points_option_in_HUC2")),
                                                            br(),
                                                            column(width=12,uiOutput("display_plot3"))
                                                            ),
                                                   tabPanel("Box plot",value = "sb2",br(),
                                                            column(width=12,align="center",uiOutput("end_points_option_in_HUC2_box")),
                                                            br(),
                                                            column(width=12,uiOutput("display_plot4"))
                                                           ),
                                                   tabPanel("HUC2 watershed zones",value = "sb3",br(),
                                                            column(width=12,align="center",uiOutput("display_HUC2_watershed_map"))
                                                   ))),
                              tabPanel("Predictor Variables HUC2 Summaries",value = "tab3_add",
                                       fluidRow(
                                         column(width=12,
                                                br(),
                                                sidebarLayout(
                                                  sidebarPanel(width=2,
                                                               uiOutput("select_variable"),
                                                               hr(),
                                                               uiOutput("display_Y_axis_slider"),
                                                               hr(),
                                                              # actionButton(inputId="display_HUC2_box_plots", label="Display box plots",style="color:black;background-color:grey"),
                                                  ), #sidebarPanel end
                                                  mainPanel(width=10,
                                                            uiOutput("display_plot4_add")
                                                  ) #mainPanel end
                                                ) #sidebarLayout end
                                         ) #column end
                                       ) #fluidRow end
                              ), #tabPanel end
                              tabPanel("Correlations", value="tab4",
                                       tabsetPanel(id="tab4b",
                                                   tabPanel("Bivariate",value = "sb41",
                                                            tags$head(tags$style("#session1_title,#session2_title{
                                                                                    color: #2fa4e7;
                                                                                    font-family: Arial;
                                                                                    font-size: 16px;
                                                                                    font-style: bold;
                                                                                    }
                                                                                    ")),
                                                            
                                                              fluidRow(
                                                              column(width=12,
                                                                     sidebarLayout(
                                                                     sidebarPanel(width=2,
                                                                                   textOutput("session1_title"),
                                                                                   uiOutput("select_x1"),
                                                                                   uiOutput("select_y1"),
                                                                                   hr(),
                                                                                   textOutput("session2_title"),
                                                                                   uiOutput("select_x2"),
                                                                                   uiOutput("select_y2"),
                                                                                   actionButton(inputId="display_bivariate_plots", label="Display plots",style="color:black;background-color:grey"),
                                                                                  ), #sidebarPanel end
                                                                     mainPanel(width=10,
                                                                               uiOutput("display_plot5")
                                                                     ) #mainPanel end
                                                                     ) #sidebarLayout end
                                                                     ) #column end
                                                            ) #fluidRow end
                                                            ), #tabPanel end
                                                   tabPanel("Multivariate",value = "sb42",
                                                            tags$head(tags$style("#reminder_note{
                                                                                    color: red;
                                                                                    font-size: 14px;
                                                                                    font-style: italic;
                                                                                    }
                                                                                    ")),
                                                            fluidRow(
                                                              column(width=12,
                                                                     sidebarLayout(
                                                                       sidebarPanel(width=2,
                                                                                    uiOutput("select_corr_end_points"),
                                                                                    uiOutput("select_corr_predictors"),
                                                                                    textOutput("reminder_note"),
                                                                                    br(),
                                                                                    actionButton(inputId="display_multivariate_plots", label="Display plots",style="color:black;background-color:grey"),
                                                                                    br(),
                                                                                    uiOutput("display_save_multivariate_button")
                                                                                    ), #sidebarPanel end
                                                                       mainPanel(width=10,
                                                                                 column(width=5,align="center",uiOutput("display_plot6")),
                                                                                 column(width=5,align="center",uiOutput("display_plot7")),
                                                                                 column(width=10,align="center",uiOutput("display_footnote_plot6"))
                                                                       ) #mainPanel end
                                                                     ) #sidebarLayout end
                                                              ) #column end
                                                            ) #fluidRow end
                                                            ) #tabPanel end
                                       )),
                                       
                              tabPanel("Maps", value="tab5",
                                       fluidRow(column(width=12,align="center",uiOutput("end_points_option_in_maps"))),
                                       fluidRow(column(width=6,align="center",uiOutput("display_explore_map_1")),
                                                column(width=6,align="center",uiOutput("display_explore_map_2"))
                                                ) #fluidRow end
                              ), #tabPanel end                 
                              tabPanel("Misc", value="tab6",
                                       br(),
                                       column(width=12,align="center",uiOutput("end_points_option_in_Misc")),
                                       br(),
                                       column(width=12,uiOutput("display_plot8")))
                 ) #tabsetPanel end
               ) #fluidRow close
               
      ),
      
      tabPanel("Create a Subset",
              fluidRow(
                column(width = 12,
                       
                       sidebarLayout(
                         sidebarPanel(width=3,
                                      uiOutput("timeSlider"),
                                      ## the minimum and maximum values for each slider can be arbitrary, they will be updated as soon as the dataset is loaded 
                                      sliderInput("year_range","Year Range:",min=1960,max=2030,value=c(2010,2014),step=1,round=0,sep=""),
                                      sliderInput("month_range","Month Range:",min=1,max=12,value=c(1,12),step=1,round=0),
                                      sliderInput("iws_ha_range","IWS ha Range(ha):",min=0,max=1482385,value=c(1,1000)),
                                      sliderInput("iws_perimkm_range","IWS perimeter Range(km):",min=2,max=3827,value=c(10,1000)),
                                      sliderInput("elevation_range","Elevation Range(m):",min=0,max=908,value=c(200,500)),
                                      sliderInput("lake_area_range","Lake Area Range(ha):",min=100,max=10000,value=c(200,500)),
                                      sliderInput("lake_perimeter_range","Lake Perimeter Range(m)",min=764,max=1326792,value=c(2000,5000)),
                                      sliderInput("lake_meanDepth_range","Lake Mean Depth Range(m)",min=0,max=89,value=c(0,10)),
                                      sliderInput("lake_maxDepth_range","Lake Maximum Depth Range(m)",min=0,max=199,value=c(0,100)),
                                      sliderInput("tn_range","Total Nitrogen Range:",min=0,max=20574,value=c(1,1000)),
                                      sliderInput("LogTN_range","Total LogNitrogen Range:",min=0,max=7,value=c(1,5)),
                                      sliderInput("tp_range","Total Phosphorus Range:",min=0,max=1220,value=c(1,100)),
                                      sliderInput("LogTP_range","Total LogPhosphorus Range:",min=0,max=5,value=c(1,3)),
                                      
                                      div(style="display: inline-block;vertical-align:top; width: 95%;",selectizeInput("selected_program_name",label="Program name",
                                                                                              choices=c("A","B","C"),multiple=TRUE,options = list(hideSelected = FALSE,plugins=list('remove_button')))),  
                                      div(style="display: inline-block;vertical-align:top; width: 95%;",selectizeInput("selected_program_type",label="Program type",
                                                                                              choices=c("A","B","C"),multiple=TRUE,options = list(hideSelected = FALSE,plugins=list('remove_button')))), 
                                      div(style="display: inline-block;vertical-align:top; width: 95%;",selectizeInput("selected_HUC2",label="HUC2",
                                                                                              choices=c("A","B","C"),multiple=TRUE,options = list(hideSelected = FALSE,plugins=list('remove_button')))), 
                                      actionButton(inputId="saveFile", label="Save my subset",style="color:black;background-color:grey"),
                                      uiOutput("display_popup_window")
                                      
                         ),
                         mainPanel(width=9,
                                   fluidRow(
                                     br(),
                                     column(width=9,align="center",actionButton(inputId="subsetSpatial", label="Create my subset spatially",style="color:black;background-color:grey"))),
                                     hr(),
                                   
                                   fluidRow(
                                    column(width=4,align="center",uiOutput("ChooseStateButton")),
                                    column(width=4,align="center",uiOutput("ChooseHUC2Button")),
                                    br()
                                   ),
                                   conditionalPanel(
                                     id = "choose_state_panel",
                                     condition = "input$ChooseState > 0",
                                   fluidRow(
                                     br(),
                                     column(width=8,align="center",uiOutput("display_clickable_map")),
                                     column(width=3,align="center",verbatimTextOutput("State_list_text_title"),
                                                                   verbatimTextOutput("State_list_text"),
                                                                   uiOutput("ClearStateButton")),
                                                                   
                                     br()
                                   ),
                                   ), # conditionalPanel for state end
                                   
                                   conditionalPanel(
                                     id = "choose_HUC2_panel",
                                     condition = "input$ChooseHUC2 > 0 ",
                                     fluidRow(
                                       column(width=8,align="center",uiOutput("display_clickable_HUC2_map")),
                                       column(width=3,align="center",verbatimTextOutput("HUC2_list_text_title"),
                                              verbatimTextOutput("HUC2_list_text"),
                                              uiOutput("ClearHUC2Button")),
                                       br()
                                     ),
                                   ), # conditionalPanel for state end
                         ) # mainPanel end
                         
                       ) # sidebarLayout end
                       
                ), #column close
                
                column(width = 12,
                       #tableOutput("contents")
                ), #column close
              ) #fluidRow close
              
      ),
      
      tabPanel("Explore Subset",
               fluidRow(p()),
               fluidRow(
                 tabsetPanel( id="subset_tabset",
                              tabPanel("Subset Summary Info", value="subset_tab1",br(),
                                       column(width=8,uiOutput("subset_display_plot1")),
                                       column(width=4,align="left",uiOutput("subset_end_points_option_in_summary"))),
                              tabPanel("Subset Time Series", value="subset_tab2",br(),
                                       column(width=12,align="center",uiOutput("subset_end_points_option_in_series")),
                                       br(),
                                       column(width=6,uiOutput("subset_display_plot2a")),
                                       column(width=6,uiOutput("subset_display_plot2b"))
                                       ),
                              tabPanel("Subset Endpoints HUC2 Summaries", value="subset_tab3",
                                       tabsetPanel(id="subset_tab3b",
                                                   tabPanel("Bar plot/Pie Chart",value = "subset_sb1",br(),
                                                            column(width=12,align="center",uiOutput("subset_end_points_option_in_HUC2")),
                                                            br(),
                                                            column(width=12,uiOutput("subset_display_plot3"))
                                                            ),
                                                   tabPanel("Box plot",value = "subset_sb2",br(),
                                                            column(width=12,align="center",uiOutput("subset_end_points_option_in_HUC2_box")),
                                                            br(),
                                                            column(width=12,uiOutput("subset_display_plot4"))
                                                            ),
                                                   tabPanel("HUC2 watershed zones",value = "subset_sb3",br(),
                                                            column(width=12,align="center",uiOutput("subset_display_HUC2_watershed_map"))
                                                            ))),
                              tabPanel("Subset Predictor Variables HUC2 Summaries",value = "subset_tab3_add",
                                       fluidRow(
                                         column(width=12,
                                                br(),
                                                sidebarLayout(
                                                  sidebarPanel(width=2,
                                                               uiOutput("subset_select_variable"),
                                                               hr(),
                                                               uiOutput("subset_display_Y_axis_slider"),
                                                               hr(),
                                                              
                                                  ), #sidebarPanel end
                                                  mainPanel(width=10,
                                                            uiOutput("subset_display_plot4_add")
                                                  ) #mainPanel end
                                                ) #sidebarLayout end
                                         ) #column end
                                       ) #fluidRow end
                              ), #tabPanel end
                              tabPanel("Subset Correlations", value="subset_tab4",
                                       tabsetPanel(id="subset_tab4b",
                                                   tabPanel("Bivariate",value = "subset_sb41",
                                                            tags$head(tags$style("#subset_session1_title,#subset_session2_title{
                                                                                    color: #2fa4e7;
                                                                                    font-family: Arial;
                                                                                    font-size: 16px;
                                                                                    font-style: bold;
                                                                                    }
                                                                                    ")),
                                                            
                                                            fluidRow(
                                                              column(width=12,
                                                                     sidebarLayout(
                                                                       sidebarPanel(width=2,
                                                                                    textOutput("subset_session1_title"),
                                                                                    uiOutput("subset_select_x1"),
                                                                                    uiOutput("subset_select_y1"),
                                                                                    hr(),
                                                                                    textOutput("subset_session2_title"),
                                                                                    uiOutput("subset_select_x2"),
                                                                                    uiOutput("subset_select_y2"),
                                                                                    actionButton(inputId="subset_display_bivariate_plots", label="Display plots",style="color:black;background-color:grey"),
                                                                       ), #sidebarPanel end
                                                                       mainPanel(width=10,
                                                                                 uiOutput("subset_display_plot5")
                                                                       ) #mainPanel end
                                                                     ) #sidebarLayout end
                                                              ) #column end
                                                            ) #fluidRow end
                                                   ), #tabPanel end
                                                   tabPanel("Multivariate",value = "subset_sb42",
                                                            tags$head(tags$style("#subset_reminder_note{
                                                                                    color: red;
                                                                                    font-size: 14px;
                                                                                    font-style: italic;
                                                                                    }
                                                                                    ")),
                                                            fluidRow(
                                                              column(width=12,
                                                                     sidebarLayout(
                                                                       sidebarPanel(width=2,
                                                                                    uiOutput("subset_select_corr_end_points"),
                                                                                    uiOutput("subset_select_corr_predictors"),
                                                                                    textOutput("subset_reminder_note"),
                                                                                    br(),
                                                                                    actionButton(inputId="subset_display_multivariate_plots", label="Display plots",style="color:black;background-color:grey"),
                                                                       ), #sidebarPanel end
                                                                       mainPanel(width=10,
                                                                                 column(width=5,align="center",uiOutput("subset_display_plot6")),
                                                                                 column(width=5,align="center",uiOutput("subset_display_plot7")),
                                                                                 column(width=10,align="center",uiOutput("display_footnote_subset_plot6"))
                                                                       ) #mainPanel end
                                                                     ) #sidebarLayout end
                                                              ) #column end
                                                            ) #fluidRow end
                                                   ) #tabPanel end
                                       )),
                              
                              tabPanel("Subset Maps", value="subset_tab5",
                                       fluidRow(column(width=12,align="center",uiOutput("subset_end_points_option_in_maps"))),
                                       fluidRow(column(width=6,align="center",uiOutput("subset_display_explore_map_1")),
                                                column(width=6,align="center",uiOutput("subset_display_explore_map_2"))
                                       ) #fluidRow end
                              ), #tabPanel end                 
                              tabPanel("Subset Misc", value="subset_tab6",
                                       br(),
                                       column(width=12,align="center",uiOutput("subset_end_points_option_in_Misc")),
                                       br(),
                                       column(width=12,uiOutput("subset_display_plot8")))
                 ) #tabsetPanel end
               ) #fluidRow close
               
      ),
      
      
      tabPanel("Run Models",
               fluidRow(p()),
               fluidRow(
                 column(width =12,
                        hr(),
                        sidebarLayout(
                          sidebarPanel(width=3,
                                       radioButtons("model_type",label="Model Types",choices=c("Random forest (on data subset)","Multilinear regression (on data subset)"),selected=NULL,inline=FALSE),   
                                       actionButton(inputId="ChooseModel", label="Choose this model",style="color:blue;background-color:black"),
                                       
                                      
                                        conditionalPanel(
                                            id = "regression_panel",
                                            condition = "input$ChooseModel %% 2 != 0 && input$model_type == Multilinear regression (on data subset)",
                                            style="overflow-x:scroll",
                                            tags$head(tags$style(HTML("#select_variables_for_LR+ div>.selectize-input{
                                                                                    font-size: 12px;
                                                                                    }
                                                                                    "))),
                                            tags$head(tags$style("#use_RF_variables_button{
                                                                                    font-size: 12px;
                                                                                    }
                                                                                    ")),
                                            tags$head(tags$style("#select_LR_end_points,#corrValue,#nvmax,#nbest{
                                                                                    width: 180px;
                                                                                    }
                                                                                    ")),
                                            tags$head(tags$style("#nbest_models_to_display{
                                                                                    width: 280px;
                                                                                    }
                                                                                    ")),
                                            
                                            br(),
                                            uiOutput("select_LR_end_points"),
                                            #uiOutput("userInput_LR_select_group"),
                                            #uiOutput("display_use_RF_variables_button"),
                                            br(),
                                            #uiOutput("userInput_LR_select"),
                                            #uiOutput("tooltips_space_holder"),
                                            uiOutput("userInput_correlation_criteria"),
                                            uiOutput("correlation_analysis"),
                                            uiOutput("regression_subsets"),
                                            uiOutput("userInput_LR_nvmax"),
                                            uiOutput("userInput_LR_nbest"),
                                            uiOutput("model_selection"),
                                            br(),
                                            uiOutput("userInput_nbest_to_display"),
                                            br(),
                                            uiOutput("confirm_to_run_LR"),
                                            br(),
                                            uiOutput("confirm_to_run_LR_prediction"),
                                            br(),
                                            uiOutput("look_back_step_1to3"),
                                         
                                       ), # conditionalPanel end
                                       
                                       conditionalPanel(
                                         id = "random_forest_panel",
                                         condition = "input$ChooseModel %% 2 !=0 && input$model_type == Random forest (on data subset)",
                                         tags$head(tags$style("#run_LR_variables_button{
                                                                                    font-size: 12px;
                                                                                    }
                                                                                    ")),
                                         tags$head(tags$style("#select_RF_end_points,#ntree,#mtry,#TestingsetPerc,#minnode,#seedvalue{
                                                                                    width: 180px;
                                                                                    }
                                                                                    ")),
                                                br(),
                                                uiOutput("select_RF_end_points"),
                                                uiOutput("userInput_ntree"),
                                                uiOutput("userInput_mtry"),
                                                uiOutput("userInput_percentage"),
                                                uiOutput("userInput_min_node"),
                                                uiOutput("userInput_seed_value"),
                                                br(),
                                                uiOutput("confirm_to_run_RF"),
                                                br(),
                                                uiOutput("display_run_LR_variables_button"),
                                                #uiOutput("display_variables_from_LR")
                                       ) # conditionalPanel end
                                       
                          ), #siderbarPanel end
                          mainPanel(width=9,
                                    tags$style(type="text/css","
                                               #modelrunMessage {
                                               position:fixed;
                                               top:60px;
                                               left:10px;
                                               width:100%;
                                               font-size:20px;
                                               text-align:center;
                                               font-weight:bold;
                                               color:#0022FF;
                                               }"),
                                    tags$head(tags$style(type="text/css","#my_numInput label{display: table-cell;
                                                                                             text-align:center;
                                                                                             vertical-align:middle;}
                                                                          #my_numInput .form-group{display: table-row;}                     
                                                                                    ")),
                                    tags$head(tags$style(type="text/css",'.mygroupclass .shiny-bound-input{overflow-x: scroll;}')),
                                    tags$head(tags$script(js1)),
                                    fluidRow(
                                      column(width=2,align="center",uiOutput("userInput_select_all")),
                                      column(width=2,align="center",uiOutput("userInput_clear_all")),
                                      column(width=2,align="center",uiOutput("userInput_link_models")),
                                      column(width=3,align="left",div(id="my_numInput",uiOutput("userInput_top_n")))
                                    ), #fluidRow end
                                    br(),
                                    fluidRow(
                                    column(width=2,align="center",uiOutput("userInput_actionB_1"),br(),uiOutput("userInput_group_1"),uiOutput("tooltips_for_group_1")),
                                    column(width=2,align="center",uiOutput("userInput_actionB_2"),br(),uiOutput("userInput_group_2"),uiOutput("tooltips_for_group_2")),
                                    column(width=2,align="center",uiOutput("userInput_actionB_3"),br(),uiOutput("userInput_group_3"),uiOutput("tooltips_for_group_3")),
                                    column(width=3,align="center",uiOutput("userInput_actionB_4"),br(),uiOutput("userInput_group_4"),uiOutput("tooltips_for_group_4"))
                                    ), #fluidRow end
                                    fluidRow(
                                    column(width=2,align="center",uiOutput("userInput_actionB_5"),br(),uiOutput("userInput_group_5"),uiOutput("tooltips_for_group_5")),
                                    column(width=2,align="center",uiOutput("userInput_actionB_6"),br(),uiOutput("userInput_group_6"),uiOutput("tooltips_for_group_6")),
                                    column(width=2,align="center",uiOutput("userInput_actionB_7"),br(),uiOutput("userInput_group_7"),uiOutput("tooltips_for_group_7")),
                                    column(width=3,align="center",uiOutput("userInput_actionB_8"),br(),uiOutput("userInput_group_8"),uiOutput("tooltips_for_group_8"))
                                    ), #fluidRow end
                                    hr(),
                                    conditionalPanel(
                                      id ="model_running_panel",
                                      condition = "input$ConfirmRunRandomForest > 0",
                                      withSpinner(textOutput("modelrunMessage"),type=2)),
                                    
                                    conditionalPanel(
                                      tags$head(tags$style("#correlation_table_footnote{
                                        color: red;
                                        font-size: 16px;
                                        font-style: bold;
                                       }
                                       ")),
                                      id = "regression_correlation_analysis_panel",
                                      condition = "input$ChooseModel %% 2 != 0 && input$model_type == Multilinear regression (on data subset)",
                                      DT::dataTableOutput("correlationAnalysis"),
                                      uiOutput("showHighCells"),
                                      DT::dataTableOutput("filteredCorrelationAnalysis"),
                                      uiOutput("display_footnote_text")
                                      
                                    ), # conditionalPanel end
                                    
                                    
                                    conditionalPanel(
                                      
                                      tags$head(tags$style(type="text/css","#lr_summary_table_footnote,#model_selection_footnote{
                                        color: red;
                                        font-size: 16px;
                                        font-style: bold;
                                        white-space: pre-wrap;
                                       }
                                       ")),
                                      
                                      tags$head(tags$style(type="text/css","#pred_title,#obs_title,#diff_title{
                                        color: black;
                                        font-size: 16px;
                                        font-style: bold;
                                        white-space: pre-wrap;
                                       }
                                       ")),
                                      
                                      
                                      id = "regression_model_selection_output_panel",
                                      condition = "input$ChooseModel %% 2 != 0 && input$model_type == Multilinear regression (on data subset)",
                                      uiOutput("RegressionModelSelectionOutput1"),
                                      uiOutput("RegressionModelSelectionOutput2"),
                                      uiOutput("RegressionModelSelectionOutput3"),
                                      uiOutput("RegressionModelSelectionOutput4"),
                                      uiOutput("RegressionModelSelectionOutput5"),
                                      uiOutput("display_model_selection_footnote_text"),
                                      
                                    ), # conditionalPanel end
                                    
                                    
                                    conditionalPanel(
                                     
                                      id = "regression_model_final_panel",
                                      condition = "input$ChooseModel %% 2 != 0 && input$model_type == Multilinear regression (on data subset)",
                                      fluidRow(br(),
                                               column(width=6,align="left",uiOutput("RegressionModelFinalPlot")),
                                               column(width=6,align="right",uiOutput("RegressionModelBetaPlot"))
                                      ),#fluidRow end
                                      
                                      uiOutput("RegressionModelSummaryTable"),
                                      uiOutput("display_lr_summary_table_footnote"),
                                      tableOutput("RegressionFinalModelPerformanceTable")
                                    ), # conditionalPanel end
                                    
                                    conditionalPanel(
                                      id = "regression_model_prediction_panel",
                                      condition = "input$ChooseModel %% 2 != 0 && input$model_type == Multilinear regression (on data subset)",
                                     
                                      fluidRow(hr(),
                                      column(width=3,
                                             uiOutput("select_LR_prediction_year_slider"),
                                             uiOutput("select_LR_prediction_month_slider"),
                                             uiOutput("select_LR_prediction_lake_maxDepth_button"),
                                             br(),
                                             uiOutput("display_map_button"),
                                             br(),
                                             uiOutput("display_download_results_button"),
                                             br(),
                                             uiOutput("download_map_button")
                                      ),
                                      column(width=8,align="center",
                                             uiOutput("adjustRegressionMapColorLegend"),
                                             uiOutput("ShowRegressionModelPredictionMap"))
                                      ), #fluidRow end
                                      
                                      fluidRow(hr(),
                                      column(width=3,
                                             br(),
                                             uiOutput("display_userInput_LR_cutoff"),
                                             br(),
                                             uiOutput("display_LR_map_in_two_colors_button"),
                                             br(),
                                             uiOutput("download_LR_map_in_two_colors_button")
                                             ),
                                             column(width=8,align="center",
                                                    uiOutput("ShowRegressionModelPredictionMapBiColor")),
                                               br(),
                                               br(),
                                      ), #fluidRow end
                                      
                                    ), # conditionalPanel end
                                   
                                    conditionalPanel(
                                      tags$head(tags$style(HTML('#seeMore {margin-top:420px}')
                                      )),
                                      
                                      id = "random_forest_output_panel",
                                      condition = "input$ChooseModel %% 2 !=0 && input$model_type == Random forest (on data subset)",
                                      fluidRow(
                                        column(width=6,plotOutput("observed_predicted_plot",width="600px",height="300px")),
                                        column(width=4,tableOutput("RF_model_output_table"))
                                      ),
                                      
                                      fluidRow(br(),
                                               column(width=6,plotOutput("variable_importance_plot",width="600px",height="500px")),
                                               column(width=4,align="center",uiOutput("seeMoreButton")),
                                               ), #fluidRow end
                                    
                                      fluidRow(hr(), column(width=12,align="left",uiOutput("display_partial_dependence_plot"))),
                                      fluidRow(br(),
                                               column(width=4,uiOutput("morePartialDependence"),
                                                              uiOutput("displayPDSlider"),
                                                              uiOutput("displayMoreButton"),
                                                              br(),
                                                              ),
                                               column(width=6,align="center",uiOutput("display_more_partial_dependence_plot")),
                                               
                                      ), #fluidRow end
                                      fluidRow(hr(),column(width=12,uiOutput("showMapButton")),
                                               column(width=4,align="center",textOutput("pred_title",inline=TRUE),
                                                      uiOutput("display_rf_testing_prediction_map",width="500px",height="400px")),
                                               column(width=4,align="center",textOutput("obs_title",inline=TRUE),
                                                      uiOutput("display_rf_testing_observation_map",width="500px",height="400px")),
                                               column(width=4,align="center",textOutput("diff_title",inline=TRUE),
                                                      uiOutput("display_rf_testing_difference_map",width="500px",height="400px"))
                                               ), #fluidRow end
                                      
                                     fluidRow(hr(),
                                              column(width=3,uiOutput("select_prediction_year_slider"),
                                                             uiOutput("select_prediction_month_slider"),
                                                             uiOutput("select_prediction_lake_maxDepth_button"),
                                                             br(),
                                                             uiOutput("display_rf_map_button"),
                                                             br(),
                                                             uiOutput("display_download_rf_results_button"),
                                                             br(),
                                                             uiOutput("download_rf_map_button")
                                                     ),
                                              column(width=8,align="center",
                                                     uiOutput("adjustRFMapColorLegend"),
                                                     uiOutput("ShowRFModelPredictionMapNew")),
                                              br(),
                                     ), #fluidRow end
                                     
                                     fluidRow(hr(),
                                              column(width=3,
                                                     br(),
                                                     uiOutput("display_userInput_cutoff"),
                                                     br(),
                                                     uiOutput("display_rf_map_in_two_colors_button"),
                                                     br(),
                                                     uiOutput("download_rf_map_in_two_colors_button")
                                              ),
                                              column(width=8,align="center",
                                                     uiOutput("ShowRFModelPredictionMapBiColor")),
                                              br(),
                                              br(),
                                     ), #fluidRow end
                                      
                                    ) #conditionalPanel end
                                    
                          ) # mainPanel end
                          
                        ) # sidebarLayout end
                        
                 ) #column close
                 
               ) #fluidRow end
                        
               
      ), # tabPanel end
      
      tabPanel("User Guide",
               tags$head(tags$style("#user_guide{font-size:16px;color:black;font-style:bold;display:block; }")),
               fluidRow(hr(),
                 column(width=12,align="left",
                        withSpinner(verbatimTextOutput("user_guide"),type=2)
                 )# column close
               ) #fluidRow end
               
      ) #tabPanel end
      
    ) # tabsetPanel close
  ) #fluidRow end
  )
))
