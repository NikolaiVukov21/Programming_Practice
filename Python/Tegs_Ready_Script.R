

#To Run All:
#Windows/ Linux: Control+Shift+Enter
#MAC: Cmd+Shift+Enter

local({

#Varialbes to Change: 
#[
#Change to select round 1 subjects
round1<-(1:3)
#Change to Select round 2 subjects
round2<-(5:8)
#Change to Select round 3 subjects
round3<-(9:12) 

#Desired Standard Deviation for Outlier Detection
SD = 3 

#Variables of Interest
variables= c("R(min)","K(min)","Angle(deg)","MA(mm)")  

#]
#PRE-STEP:
#Required Packages:

print("Loading Required Packages...")
if(requireNamespace("agricolae", quietly = TRUE)==FALSE) install.package ("agricolae")
if(requireNamespace("agricolae", quietly = TRUE)==FALSE) install.package ("agricolae")
if(requireNamespace("readxl", quietly = TRUE)==FALSE) install.package ("readxl")
if(requireNamespace("openxlsx", quietly = TRUE)==FALSE) install.package ("openxlsx")
if(requireNamespace("dplyr", quietly = TRUE)==FALSE) install.package ("dplyr")
if(requireNamespace("ggplot2", quietly = TRUE)==FALSE) install.package ("ggplot2")
if(requireNamespace("tidyverse", quietly = TRUE)==FALSE) install.package ("tidyverse")
if(requireNamespace("patchwork", quietly = TRUE)==FALSE) install.package ("patchwork")
if(requireNamespace("ggsignif", quietly = TRUE)==FALSE) install.package ("ggsignif")
if(requireNamespace("knitr", quietly = TRUE)==FALSE) install.package ("knitr")
if(requireNamespace("patchwork", quietly = TRUE)==FALSE) install.package ("patchwork")

library(agricolae)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggsignif)
library(knitr)
library(patchwork)
print("Required Packages Loaded")

## Load and Clean ExcelSheet

 

# Data Reform: TEG-Hemodilutions 
#*Antonio Renaldo
#*Last Updated: 06/16/2026*

#---- Import----
file_path <- file.choose()

sheet_name <- "Original"

data <- read_excel(file_path, sheet = sheet_name)

print("Loading the Datasets...")

#---- Split Sample Description----





data_sep <- data %>%
  extract(
    SAMPLEDESCRIPTION,
    into = c("Exp", "Subject", "HMT_model", "Hb", "EM_Hb", "Alb", "Version_v1", "Notes"),
    regex = "^([A-Za-z]+)[-_](\\d+)[-_]([^\\-_]+)(?:[-_](\\d+))?(?:-'(\\d+))?(?:-\"([A-Za-z]))?[-_]((?:v\\d+(?:\\.\\d+)?)|[A-Za-z]+)(?:\\*(.*))?$",
    remove = FALSE
  ) %>%
  mutate(
    Subject = as.integer(Subject),
    Hb = ifelse(is.na(Hb), "N/A", Hb)
  )



#---- Clean Data----

#Reorganize Variables
data_sep <- data_sep %>%
  select(
    SAMPLEDESCRIPTION,
    Exp,
    Subject,
    HMT_model,
    Hb,
    EM_Hb,
    Alb,
    Version_v1,
    Notes,
    `R(min)`,
    `K(min)`,
    `Angle(deg)`,
    `MA(mm)`,
    `LY30(%)`,
    `TMA(min)`,
    `TEG ACT(sec)`,
    `SP(min)`,
    `G(d/sc)`,
    `E(d/sc)`,
    `TPI(/sec)`,
    `EPL(%)`,
    `A30(mm)`,
    `CL30(%)`,
    `A60(mm)`,
    `CL60(%)`,
    `LY60(%)`,
    `CLT(min)`,
    `A(mm)`,
    `PMA`,
    `LTE(min)`,
    everything()
  )

#Fix the Negative K/MA values
data_sep <- data_sep %>%
  mutate(
    `K(min)` = abs(`K(min)`),
    `MA(mm)` = abs(`MA(mm)`)
  )

#Create organizing groups for  HMT_Model_01
data_sep <- data_sep %>%
  mutate(
    HMT_model_01 = case_when(
      HMT_model == "fWB"      ~ 1,
      HMT_model == "PRP"      ~ 2,
      HMT_model == "PPP"      ~ 3,
      HMT_model == "1xFDP"    ~ 4,
      HMT_model == "FDP:Buff" ~ 5,
      HMT_model == "EMv1"     ~ 6,
      HMT_model == "EMv2"     ~ 7,
      HMT_model == "EMv3"     ~ 8,
      HMT_model == "EMv4"     ~ 9,
      TRUE ~ NA_real_
    ))

#Rearrange & Sort data
data_sep <- data_sep %>%
  arrange(
    Subject,
    HMT_model_01,
    desc(as.numeric(Hb)),
    EM_Hb,
    Alb,
    Version_v1
  )



#---- GraphPad Pivot----


data_gp <- data_sep %>%
  mutate(
    HMT_label = case_when(
      HMT_model == "fWB" & Hb == "N/A" ~ "fWB",
      TRUE ~ paste0(HMT_model, " (", Hb, ")")
    )
  ) %>% ##modify to create separators for EM_Hb & Alb
  pivot_wider(
    id_cols = c(Exp, Subject, Version_v1),
    names_from = HMT_label,
    values_from = c(
      `R(min)`,
      `K(min)`,
      `Angle(deg)`,
      `MA(mm)`,
      `LY30(%)`,
      `TMA(min)`,
      `TEG ACT(sec)`
    ),
    values_fn = ~ .[1]   # ✅ TAKE FIRST VALUE (prevents list columns)
  ) %>%
  rename_with(
    ~ sub("^(.+?)_(.+)$", "\\2 \\1", .x),
    -c(Exp, Subject, Version_v1)
  )


#----Export to Excel----

# Load workbook
wb <- loadWorkbook(file_path)

clean_df <- function(df) {
  df %>%
    mutate(across(everything(), ~ {
      if (is.list(.)) as.character(.) else .
    }))
}


wb<-loadWorkbook(file_path)

processed_sheets<- list("Processed_Data"=clean_df(data_sep))

for (sheet_name in names(processed_sheets)){
  if (sheet_name %in% names(wb)){
    removeWorksheet(wb,sheet_name)
  }
  addWorksheet(wb,sheet_name)
  writeData(wb,sheet=sheet_name,x=processed_sheets[[sheet_name]])
}  
saveWorkbook(wb,file_path,overwrite=TRUE)

#Graph Pad



graphpad_wb<-createWorkbook()

addWorksheet(graphpad_wb,"GraphPad")

writeData(
  graphpad_wb,
  sheet="GraphPad",x=clean_df(data_gp)
)


saveWorkbook(graphpad_wb,"GraphPad_Export.xlsx",overwrite=TRUE)

## Universal Definitions

 
#Creating a dataframe without exclusions

Cleaned_df<-read_excel(file_path,sheet="Cleaned_Data") %>% 
  filter(Exclude_Instance %in% 0 | is.na(Exclude_Instance))

#Adds function to detect outliers and outputs a table
DetectOutliers<-function(data){
  
  
  #Detects and outputs outliers
  GraphValue_Outlier <- data %>%
    pivot_longer(cols=variables,
                 names_to = "Variable",
                 values_to ="Value") %>% 
    group_by(HMT_model, Variable) %>% 
    mutate(mean_group=mean(Value, na.rm=TRUE),
           mean_sd=sd(Value, na.rm=TRUE),
           Z_score=(Value-mean_group)/mean_sd) %>% 
    ungroup()
  
  
  outliersTable<-GraphValue_Outlier %>% 
    filter(abs(Z_score)>SD) %>% 
    transmute(
      SAMPLEDESCRIPTION,
      Subject,
      HMT_model,
      Hb,
      Variable,
      Value
    )
  
  Graphvalues_clean <- GraphValue_Outlier %>% mutate(
    Value=ifelse(abs(Z_score) >SD, NA,Value)) %>% 
    #Removes the columns that are outliers
    select(-mean_group,-mean_sd,-Z_score) %>% 
    pivot_wider(
      names_from=Variable,
      values_from=Value
    )
  
  #Displays Table
  Table<-as_tibble(outliersTable) %>% 
    kable(format="pipe",
          booktabs=TRUE,
          align="ccc",
          kable.NA="")
  
  
  return(list(CleanedDF=Graphvalues_clean,Out_Tab=Table))
}

#++++++++++++++++++++++++++++++++++++Seperator++++++++++++++++++++++++++++++++++++++

#Function to caculate Significance via Anova & Fischer's LSD test 
#And to plot the graphs
generate_hmt_plots <-function(data){
  
  #HTML models/ Groups of interest
  Defined_Grouping=c("fWB",setdiff(unique(data$HMT_model),"fWB"))
  
  #Used reference/ neutral model
  ref_model="fWB"
  
  #Needed to ensure 'NA' is regiestered as NA and not as a string ("NA")
  data<-data|> mutate( across(where(is.character), ~na_if(., "NA"))) 
  
  if(all(is.na(data$Hb))){
    data$Hb<- data$EM_Hb
    label_prefix <- " EM_Hb"
    val_low <-"300"
    val_high<-"600"
  } 
  else{
    label_prefix <- " Hb"
    val_low <-"4"
    val_high<-"8"
  }
  
  #Creates groups for plotting and statics
  GraphValues<- data %>% 
    mutate(HMT_model=factor(HMT_model,levels=Defined_Grouping)) %>% 
    mutate(stat_group=paste(HMT_model,Hb,sep="_")) %>% 
    mutate(stat_group=factor(stat_group))
  
  #Turns "FWB" into the model of reference
  fwb_stat_group <- GraphValues %>% 
    filter(HMT_model==ref_model) %>% 
    pull(stat_group) %>% 
    first() %>% 
    as.character() #Ran into issues with mutate failing on second run
  
  #Relevling the dataframe so that Fwb goes first followed by other models
  GraphValues<-GraphValues %>% 
    mutate(stat_group=relevel(stat_group,ref=fwb_stat_group))
  
  #Helper function to get the pairwise p-values
  get_p_val<-function(g1,g2,comp_df){
    name1<-paste(g1,"-",g2)
    name2<-paste(g2,"-",g1)
    
    #Searches both directions/instances used for LSD test
    if (name1 %in% rownames(comp_df)) return (comp_df[name1,"pvalue"])
    if (name2 %in% rownames(comp_df)) return (comp_df[name2,"pvalue"])
    return(NA) #Backup default if a comparison isn't found
  }
  
  #Empty Plot List
  plot_list<-list()
  
  #Loop for plotting each variable
  for (v in variables){
    
    GraphValues_gg<-GraphValues %>% 
      filter(!is.na(.data[[v]])) %>% 
      group_by(HMT_model,Hb) %>% 
      mutate(
        n_count=n(),
        x_label=if_else(
          HMT_model==ref_model,
          paste0("Native\nn=", n_count),
          paste0(Hb,label_prefix,"\nn=",n_count)
        )
      ) %>% 
      ungroup()
    
    #Caculating the dynamic y-positions
    y_max<-max(GraphValues_gg[[v]],na.rm=TRUE)
    y_range<-y_max-min(GraphValues_gg[[v]],na.rm=TRUE)
    
    #Dynamic position for the astrics
    star_y_pos_fwb<-y_max+(y_range*0.20)
    star_y_pos_hb<-y_max+(y_range*0.10)
    
    #Fitting Anova
    Anova_Object<-as.formula(paste0("`",v,"`~stat_group"))
    model<-aov(Anova_Object,data=GraphValues_gg)
    
    #Fischer's LSD Test
    fit<-LSD.test(model,"stat_group",console=FALSE,group=FALSE)
    comp_df<-fit$comparison
    unique_groups<-as.character(unique(GraphValues_gg$stat_group))
    
    #Comparison 1: Models vs Based Reference
    other_models<-setdiff(unique_groups,fwb_stat_group)
    fwb_sig_list<-list()
    
    for (og in other_models){
      pval<-get_p_val(fwb_stat_group,og,comp_df)
      
      if(!is.na(pval) && pval<0.05){
        stars<-case_when(pval<0.001~"***",pval<0.01~"**",pval<0.05~"*",TRUE~"")
        fwb_sig_list[[og]]<-data.frame(stat_group=og,p_val=pval,label=stars)
      }
    }
    fwb_sig_df<-bind_rows(fwb_sig_list)
    
    #Prevents overlapping astricks
    
    if (nrow(fwb_sig_df) > 0) {
      pos_df<-GraphValues_gg %>% 
        group_by(stat_group,HMT_model,x_label) %>% 
        summarise(.groups="drop") %>% 
        mutate(y_pos=star_y_pos_fwb)
      
      fwb_sig_df<-fwb_sig_df %>% 
        inner_join(pos_df,by="stat_group") %>% 
        mutate(HMT_model=factor(HMT_model,levels=Defined_Grouping))
    }
    #Comparison 1: Hb=4 vs Hb=8
    hb_sig_list<-list()
    models<-as.character(unique(GraphValues_gg$HMT_model))
    
    for (m in models){
      if(m==ref_model) next
      
      g4<-paste(m,val_low,sep="_")
      g8<-paste(m,val_high,sep="_")
      
      #Gets the Pval
      if (g4 %in% unique_groups && g8 %in% unique_groups){
        pval<-get_p_val(g4,g8,comp_df)
        
        #Star allocation
        if(!is.na(pval) && pval<0.05){
          stars<-case_when(pval<0.001~"***",pval<0.01~"**",pval<0.05~"*",TRUE~"")
          
          x_g4<-unique(GraphValues_gg$x_label[GraphValues_gg$stat_group==g4])
          x_g8<-unique(GraphValues_gg$x_label[GraphValues_gg$stat_group==g8])
          
          #Gets plotting postion for Hb comparisons annotations
          hb_sig_list[[m]]<-data.frame(
            HMT_model=factor(m,levels=Defined_Grouping),
            x_start=x_g4,
            x_mid=1.5,
            x_end=x_g8,
            label=stars,
            y_pos=star_y_pos_hb,
            y_pos_text=star_y_pos_hb+(y_range*0.03)
          )
        }
      }
    }
    
    hb_sig_df<-bind_rows(hb_sig_list)
    
    #Generating Plots
    p<-ggplot(GraphValues_gg,aes(x=x_label, y=.data[[v]]))+
      geom_boxplot(fill="lightgrey",outlier.shape=NA)+
      stat_summary(fun.min=min,fun.max=max,geom="errorbar",width=0.5)+
      geom_point(color="black",fill="lightgrey",shape=21)+
      facet_grid(~HMT_model,scales="free_x",space="free_x",switch="x",drop=FALSE)+
      labs(x=NULL,y=v)+
      theme_classic()+
      theme(
        strip.text=element_text(face="bold",size=12),
        strip.background=element_rect(fill="lightgrey"),
        legend.position="none",
        panel.border=element_rect(color="grey50",fill=NA,linetype="dotted"),
        panel.spacing=unit(1,"lines")
      )
    
    if (nrow(fwb_sig_df)>0){
      p<-p+geom_text(data=fwb_sig_df,aes(x=x_label,y=y_pos,label=label),
                     inherit.aes=FALSE,color="black",size=6,vjust=0.5)
    }
    #
    if(nrow(hb_sig_df)>0){
      p<-p+geom_segment(data=hb_sig_df, aes(x=x_start,xend=x_end,y=y_pos,yend=y_pos), inherit.aes=FALSE,color="red",linewidth=0.8)+
        geom_text(data=hb_sig_df,aes(x=x_mid,y=y_pos_text,label=label),inherit.aes=FALSE,color="red",size=6,vjust=0.5)
    }
    
    #Stores plot into a list
    plot_list[[v]]<-p
  }
  
  #Returns final list
  return(plot_list)
}
print("Dataset Loaded")

print("Generating Plots for Round 1...")

# Round 1: Subjects 1-3

 
#Removes the unneccesary 
Round1_Df <- Cleaned_df %>%
  filter(Subject %in% round1)

#Saves plot 1 with outliers 
p1_1<-wrap_plots(generate_hmt_plots(Round1_Df),ncol=2)

#Detects Outliers
Round1_Outliers<-DetectOutliers(Round1_Df)$CleanedDF
Round1_OutTable<-DetectOutliers(Round1_Df)$Out_Tab

print("Plots Generated for Round 1 Finished")


print("Generating Plots for Round 2...")
#Saves plot 2 without outliers
p2_1<-wrap_plots(generate_hmt_plots(Round1_Outliers),ncol=2)
  
# Round 2: Subjects 5-8

#Picks only the subjects defined in Round 2: 5-8
Round2_Df <- Cleaned_df %>%
  filter(Subject %in% round2)



#Saves plot 2 with outliers 
p1_2<-wrap_plots(generate_hmt_plots(Round2_Df),ncol=2)

#Detects Outliers
Round2_Outliers<-DetectOutliers(Round2_Df)$CleanedDF
Round2_OutTable<-DetectOutliers(Round2_Df)$Out_Tab

#Saves plot 2 without outliers
p2_2<-wrap_plots(generate_hmt_plots(Round2_Outliers),ncol=2)

print("Plots Generated for Round 2 Finished")
print("Generating Plots for Round 3...")

# Round 3: Subjects 9-12

#Picks only the subjects defined in Round 3: 9-12
Round3_Df <- Cleaned_df %>%
  filter(Subject %in% round3,
         (!str_detect(HMT_model, "CFF"))
  )


#Saves plot 2 with outliers 
p1_3<-wrap_plots(generate_hmt_plots(Round3_Df),ncol=2)

#Detects Outliers
Round3_Outliers<-DetectOutliers(Round3_Df)$CleanedDF
Round3_OutTable<-DetectOutliers(Round3_Df)$Out_Tab


#Saves plot 2 without outliers
p2_3<-wrap_plots(generate_hmt_plots(Round3_Outliers),ncol=2)

print("Plots Generated for Round 3 Finished")

print("Combining and Saving Plots...")
Master_Plots<-list(
  round1_with=p1_1,
  round1_without=p2_1,
  round2_with=p1_2,
  round2_without=p2_2,
  round3_with=p1_3,
  round3_without=p2_3
)

pdf("RoundsGraphs.pdf",width=10.75,height=8.25)

for (name in names(Master_Plots)){
    print(wrap_plots(Master_Plots[[name]],ncol=1)+
          plot_annotation(title=name))
}

print("Plots Saved to 'RoundsGraphs.pdf' ")

  print("Round 1 Outliers:")
  print(Round1_OutTable)
  print("Round 2 Outliers:")
  print(Round2_OutTable)
  print("Round 3 Outliers:")
  print(Round3_OutTable)
  
print("Finished")
})