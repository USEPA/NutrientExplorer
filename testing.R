
dir = "C:/Users/mpennino/OneDrive - Environmental Protection Agency (EPA)/R/Shiny/EMVL/Nutrient_Explorer/2022/Final/Nutrient_Explorer_final_paper_version/Nutrient_Explorer_final_paper_version/Data/"
myData = read.csv(paste0(dir,"LAGOS_Data_5000Rows.csv"))

# Figure 1c. Explore Data > Endpoint HUC2 Summaries > Box plot (Starts at line 1111, plot4)
# Figure 1c. Explore Subset > Subset Endpoint HUC2 Summaries > Box plot (Starts at line 2170, subset_plot4)
ggplot(myData, aes(x=as.factor(HU2), y=LogTP,fill=as.factor(HU2)))+
  geom_boxplot(fill=NA)+theme(legend.position="none")+
  labs(x="HUC2 Zone ID", y = paste0("log","(TP, \u03bcg/L)"))

# Figure 1d. Explore Data > Time Series (Starts at line 1031, plot2b)
# Figure 1d. Explore Subset > Subset Time Series (Starts at line 2100, subset_plot2b)
ggplot(myData, aes(x=as.factor(Year), y=LogTP,fill=as.factor(Year))) + 
  geom_boxplot(fill=NA)+ 
  labs(x="Year", y = paste0("log","(TP, ug/L)"))+
  theme_classic()+
  theme(text=element_text(size=14,face = "bold", color="blue"),
        axis.text.x=element_text(angle=45, hjust=1))#+
  #theme(legend.position="none")
  #guides(fill=guide_legend(title="Year"))

# Figure S2. Explore Data > Predictor Variables HUC2 Summaries (starts at line 1172, plot4_add)
# Figure S2. Explore Subset > Subset Predictor Variables HUC2 Summaries (starts at line 2232, subset_plot4_add)

ggplot(myData, aes(x=as.factor(HU2), y=myData$N_Ag_Surplus.1,fill=as.factor(HU2)))+
  geom_boxplot(fill=NA)+
  labs(x="HUC2 Zone ID", y = myData$N_Ag_Surplus.1)+
  theme_classic()+
  guides(fill=guide_legend(title="HUC2 Zone ID"))+ 
  theme(panel.border = element_rect(fill=NA,color="darkred", size=0.5,linetype="solid"))+
  theme(text=element_text(size=15,face = "bold", color="blue"))#+
 # ylim(0,y_axis_max)
