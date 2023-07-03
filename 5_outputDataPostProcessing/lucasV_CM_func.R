rm(list=ls())
library(ggplot2)
library(dplyr)
library(scales)
library(caret)
library(reshape2)
library(gridExtra)
library(DescTools)
library(splitstackshape)
library(tidyr)

#define global vars
diskDir <- '/eos/jeodpp/data/projects/REFOCUS/data'

df_crop85 <- read.csv(file.path(diskDir, '/LUCAS_C_vision/v2/inputs/lucasV_testSet_cap85.csv'), stringsAsFactors = F)

df_lucas <- read.csv(file.path(diskDir, 'LUCAS_C_vision/v2/inputs/gt_LUCAS.csv'), stringsAsFactors = F)
#change pointidyear colname to basename for script to work
colnames(df_lucas)[grep('pointidyear', colnames(df_lucas))] <- 'basename'
head(df_lucas)

runs <- c(4, 49, 58, 78, 88)

#define functions
calculateEntropy <- function(df.crop){
  for(ii in 1:nrow(df.crop)){
    entropy_input <- as.numeric(gsub(" ","",gsub("\\]","",gsub("\\[","",gsub("'", "",strsplit(df.crop[ii,2][1], ',')[[1]])))))
    
    #desctools entropy
    df.crop$cnn_values_DescToolsentropy[ii] <- Entropy(entropy_input)
    
    #calculate the information of the event
    df.crop$cnn_values_Information[ii] <- -log2(df.crop$cnn_values[ii])
    
    #calculate your own entropy
    #df.crop$cnn_values_YourOwnentropy[ii] <- -sum(entropy_input * log2(entropy_input))
  }
  
  return(df.crop)
}
findEntropyCutOff <- function(df, entr_vec){
  
  entr_res_df <- data.frame(matrix(NA, nrow=length(entr_vec), ncol = 3))
  colnames(entr_res_df) <- c('entr_cutoff', 'diffInTFvals_per', 'Tbelow1per')
  for(iii in 1:length(entr_vec)){
    #print(iii)
    
    entr_res_df$entr_cutoff[iii] <- entr_vec[iii]
    
    #make sure there are entropy values of this range
    if(entr_vec[iii] <= max(df$cnn_values_DescToolsentropy)){
      df_highE <- df[df$cnn_values_DescToolsentropy > entr_vec[iii],]
      
      df_highE_T_per <- as.data.frame(table(df_highE$cnn_result) /  nrow(df))
      #print(df_highE_T_per$Freq[df_highE_T_per$Var1 == 'TRUE'])
      
      diff <- abs(df_highE_T_per$Freq[1] - df_highE_T_per$Freq[2]) 
      
      if(!is.na(diff)){
        entr_res_df$diffInTFvals_per[iii] <- diff      
      }else{
        entr_res_df$diffInTFvals_per[iii] <- 0
      }
      
      
      #if i've lost more less than 1 percent of the TRUE classifications, assign T to Tbelow1per
      if(entr_res_df$diffInTFvals_per[iii] != 0){# if there is at least one T classification
        
        if(df_highE_T_per$Freq[df_highE_T_per$Var1 == 'TRUE'] < 0.1){
          
          entr_res_df$Tbelow1per[iii] <- T
        }
      }else{
        #print('F')
        entr_res_df$Tbelow1per[iii] <- F
      }
    }
  }
  
  #remove all NAs
  entr_res_df <- entr_res_df[! is.na(entr_res_df$diffInTFvals_per) | ! is.na(entr_res_df$Tbelow1per),]
  
  result <- entr_res_df$entr_cutoff[entr_res_df$diffInTFvals_per == max(entr_res_df$diffInTFvals_per) & entr_res_df$Tbelow1per == TRUE]
  
  if(length(result) == 0 | is.na(result)){
    result <- entr_res_df$entr_cutoff[entr_res_df$diffInTFvals_per == max(entr_res_df$diffInTFvals_per)]
  }
  
  if(length(result > 1)){
    result <- result[1]
  }
  
  return(result)
}

findProbaCutOff <- function(df, proba_vec){
  
  proba_res_df <- data.frame(matrix(NA, nrow=length(proba_vec), ncol = 3))
  colnames(proba_res_df) <- c('proba_cutoff', 'diffInTFvals_per', 'Tbelow1per')
  for(iii in 1:length(proba_vec)){
    #print(proba_vec[iii])
    
    proba_res_df$proba_cutoff[iii] <- proba_vec[iii]
    
    #make sure there are probaopy values of this range
    if(proba_vec[iii] >= min(df$cnn_values)){
      df_highP <- df[df$cnn_values <= proba_vec[iii],]
      
      df_highP_T_per <- as.data.frame(table(df_highP$cnn_result) /  nrow(df))
      #print(df_highE_T_per$Freq[df_highE_T_per$Var1 == 'TRUE'])
      
      diff <- abs(df_highP_T_per$Freq[1] - df_highP_T_per$Freq[2]) 
      
      if(!is.na(diff)){
        proba_res_df$diffInTFvals_per[iii] <- diff      
      }else{
        proba_res_df$diffInTFvals_per[iii] <- 0
      }
      
      
      #if i've lost more less than 1 percent of the TRUE classifications, assign T to Tbelow1per
      if(proba_res_df$diffInTFvals_per[iii] != 0){# if there is at least one T classification
        
        if(df_highP_T_per$Freq[df_highP_T_per$Var1 == 'TRUE'] < 0.1){
          
          proba_res_df$Tbelow1per[iii] <- T
        }
      }else{
        #print('F')
        proba_res_df$Tbelow1per[iii] <- F
      }
    }
  }
  
  #remove all NAs
  proba_res_df <- proba_res_df[! is.na(proba_res_df$diffInTFvals_per) | ! is.na(proba_res_df$Tbelow1per),]
  proba_res_df <- proba_res_df[! is.na(proba_res_df$Tbelow1per),]
  #return(proba_res_df)
  
  result <- proba_res_df$proba_cutoff[proba_res_df$diffInTFvals_per == max(proba_res_df$diffInTFvals_per) & proba_res_df$Tbelow1per == TRUE]

  if(length(result) == 0 | is.na(result)){
    result <- proba_res_df$proba_cutoff[proba_res_df$diffInTFvals_per == max(proba_res_df$diffInTFvals_per)]
  }

  if(length(result > 1)){
    result <- result[1]
  }

  return(result)
}

#### MAKE IT WORK BY SELECTING WHICH QUADRANT TO REMOVE AS WELL 
load_and_preprocess <- function(pathtocnnoutcsvs, runn, tf85, entropytf, probatf, entropyProbatf, cerealsaggtf){
  #read cnn out csv of model configuration
  df.crop <- read.csv(paste0(pathtocnnoutcsvs,runn,'/cnn_output_data_check.csv'), stringsAsFactors = F)
  
  #if tf85 == T, inference set is capped at 85 images/class
  if(tf85 == T){
    df.crop <- df.crop[df.crop$pointidyear %in% df_crop85$basename,]
  }
  
  #are we calculating entropy thresholds
  if(entropytf == T){
    df.crop <- calculateEntropy(df.crop)
    
    #assign TF for correct classification 
    df.crop$cnn_result <- ifelse(df.crop$cnn_labels == df.crop$lc1, T, F)
    
    entr_vec <- seq(1.5,3.0, 0.01)
    entr_cutoff <- findEntropyCutOff(df.crop, entr_vec)
    print(paste0('entorpy cutoff: ',entr_cutoff))
    
    #remove all examples that have an entropy less than the cutoff $ and information less than 1.0
    df.crop <- df.crop[df.crop$cnn_values_DescToolsentropy < entr_cutoff,] #& df.crop$cnn_values_Information < 1.0,]
    
    #remove __RAW columns (the ones for entropy) & other columns that interfere
    df.crop <- df.crop[,-c(grep('cnn_values_DescToolsentropy', colnames(df.crop)))]
    df.crop <- df.crop[,-c(grep('cnn_values_Information', colnames(df.crop)))]
    df.crop <- df.crop[,-c(grep('cnn_result', colnames(df.crop)))]
  }
  
  #are we calculating probability thresholds
  if(probatf == T){
    #assign TF for correct classification 
    df.crop$cnn_result <- ifelse(df.crop$cnn_labels == df.crop$lc1, T, F)
    
    proba_vec <- seq(0.0,1.0, 0.01)
    proba_cutoff <- findProbaCutOff(df.crop, proba_vec)
    print(paste0('proba cutoff: ',proba_cutoff))
    
    #remove all examples that have an entropy less than the cutoff $ and information less than 1.0
    df.crop <- df.crop[df.crop$cnn_values > proba_cutoff,] #& df.crop$cnn_values_Information < 1.0,]
    
    #remove __RAW columns (the ones for entropy) & other columns that interfere
    df.crop <- df.crop[,-c(grep('cnn_values_DescToolsentropy', colnames(df.crop)))]
    df.crop <- df.crop[,-c(grep('cnn_values_Information', colnames(df.crop)))]
    df.crop <- df.crop[,-c(grep('cnn_result', colnames(df.crop)))]
  }
  
  #are we filtering for both
  if(entropyProbatf == T){
    
    df.crop <- calculateEntropy(df.crop)
    
    #assign TF for correct classification 
    df.crop$cnn_result <- ifelse(df.crop$cnn_labels == df.crop$lc1, T, F)
    
    entr_vec <- seq(1.5,3.0, 0.01)
    entr_cutoff <- findEntropyCutOff(df.crop, entr_vec)
    print(paste0('entorpy cutoff: ',entr_cutoff))
    
    proba_vec <- seq(0.0,1.0, 0.01)
    proba_cutoff <- findProbaCutOff(df.crop, proba_vec)
    print(paste0('proba cutoff: ',proba_cutoff))
    
    #remove all examples that have an entropy less than the cutoff $ and information less than cutoff
    df.crop <- df.crop[df.crop$cnn_values > proba_cutoff,] #& df.crop$cnn_values_Information < 1.0,]
    
    #remove __RAW columns (the ones for entropy) & other columns that interfere
    df.crop <- df.crop[,-c(grep('cnn_values_DescToolsentropy', colnames(df.crop)))]
    df.crop <- df.crop[,-c(grep('cnn_values_Information', colnames(df.crop)))]
    df.crop <- df.crop[,-c(grep('cnn_result', colnames(df.crop)))]
  }
  
  #remove the shitty index column (oh how i hate you)
  if('X' %in% colnames(df.crop)){
    df.crop$X <- NULL
  }
  
  #remove __RAW columns (the ones for entropy)
  df.crop <- df.crop[,-c(grep('__RAW', colnames(df.crop)))]
  df.crop <- df.crop[,c(3,4,1,2)]
  colnames(df.crop) <- c('code_max', 'prob_max', 'basename', 'code')
  df.crop$code_max <- toupper(df.crop$code_max)
  
  #little check
  # print(table(df.crop$basename %in% df_lucas$basename))
  # print(head(df.crop$basename))
  # print(head(df_lucas$basename))
  if(! all(df.crop$basename %in% df_lucas$basename)){
    stop('not all basenames from df.crop are in df_lucas')
  }
  
  #merge with gt lucas
  df.crop <- merge(df.crop, df_lucas, by = 'basename')
  
  #should be all true - check if labels match
  if(! all(df.crop$code == df.crop$lc1)){
    stop('labels from gt lucas and from df.crop do not match')
  }
  
  #should we aggregate the cereals class
  if(cerealsaggtf == T){
    #aggregate cereals into common class (B11)
    # B11 - Common wheat
    # B12 - Durum wheat
    # B13 - Barley
    # B14 - Rye
    
    cereals <- c("B11", "B12", "B13", 'B14')
    for(llc1 in cereals){
      #convert class_max to cereal class
      df.crop$code_max[df.crop$code_max == llc1] <- 'B11'
      
      #convert code to cereal class
      df.crop$code[df.crop$code == llc1] <- 'B11'
      
      #convert lc1 to cereal class
      df.crop$lc1[df.crop$lc1 == llc1] <- 'B11'
    }
  }
  
  return(df.crop)
}

confusionMatrix_calculate_PA_UA_F1_CROP <- function(df){
  u = sort(union(df$code,df$code_max))
  cm = table(factor(df$code, u), factor(df$code_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.index<-confusionMatrix(cm,mode='prec_recall')
  cm.ByClass<-round(data.frame(cm.index$byClass),digit=4)
  table.byClass.1<-data.frame(UA=cm.ByClass$Recall,PA=cm.ByClass$Precision,F_score=cm.ByClass$F1)
  rownames(table.byClass.1)<-rownames(cm)
  
  return(table.byClass.1)
}
confusionMatrix_tableForm_CROP <- function(df, table.byClass.1){
  u = sort(union(df$code,df$code_max))
  cm = table(factor(df$code, u), factor(df$code_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  #Add Precision and Recall to confusion matrix
  cm.with.sums.1 <- rbind(cm.with.sums.1, UA = c(round((table.byClass.1$UA * 100), 1), NA))
  cm.with.sums.1 <- cbind(cm.with.sums.1, PA = c(round((table.byClass.1$PA * 100), 1), NA, NA))
  
  return(cm.with.sums.1)
}
confusionMatrix_produceGrobTable_CROP <- function(df, table.byClass.1){
  u = sort(union(df$code,df$code_max))
  cm = table(factor(df$code, u), factor(df$code_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.index<-confusionMatrix(cm,mode='prec_recall')
  
  #Accuracy grob table to append to confusion matrix graph
  cm.crop.index.overal.table <- as.data.frame(format(round(cm.index$overall, 3), nsmall = 3))
  cm.crop.index.overal.table$Metric <- c('Acc', 'Kappa', 'AccLow', 'AccUp', 'AccNull', 'AccPv', 'McnPv')
  rownames(cm.crop.index.overal.table) <- NULL
  cm.crop.index.overal.table <- cm.crop.index.overal.table[,c(2,1)]
  colnames(cm.crop.index.overal.table) <- c('Metric','Value')
  rownames(cm.crop.index.overal.table) <- NULL
  cm.crop.index.overal.table$Value <- as.numeric(as.character(cm.crop.index.overal.table$Value))
  cm.crop.index.overal.table <- cm.crop.index.overal.table[!cm.crop.index.overal.table$Metric %in% c('AccLow', 'AccUp', 'AccNull', 'AccPv','McnPv'),]
  cm.crop.index.overal.table[nrow(cm.crop.index.overal.table)+1, 1] <- 'M-F1'
  cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1'] <- round(mean(table.byClass.1$F_score), 3)
  
  return(cm.crop.index.overal.table)
}
create_intCompareTable <- function(runn, cm.crop.index.overal.table, pathtoresumetraincsv){
  #put results in compare table
  t_comp <- data.frame(matrix(NA, nrow = 1, ncol = 3))
  colnames(t_comp) <- c('runn', 'mf1', 'acc')
  
  #performence metrics
  t_comp$runn <- runn
  t_comp$mf1 <- as.numeric(as.character(cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']))
  t_comp$acc <- as.numeric(as.character(cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'Acc']))
  
  #add info from resume-train
  resume_train <- read.csv(pathtoresumetraincsv)
  t_comp$trainAcc <- resume_train$Acc[resume_train$run == runn & resume_train$set == 'train']
  t_comp$ValAcc <- resume_train$Acc[resume_train$run == runn & resume_train$set == 'validation']
  t_comp$LR <- unique(resume_train$LR[resume_train$run == runn])
  t_comp$momentum <- unique(resume_train$momentum[resume_train$run == runn])
  t_comp$BS <-  unique(resume_train$BS[resume_train$run == runn])
  
  return(t_comp)
}
melt_and_order_df <- function(cm.with.sums.1){
  
  # melt
  df.bbch.melt <- melt(cm.with.sums.1)
  
  #order to have confusion matrix margins at bottom and right
  df.bbch.melt <- df.bbch.melt %>%
    mutate(Var1 = factor(Var1), # alphabetical order by default
           Var2 = factor(Var2, levels = rev(unique(Var2))))
  
  return(df.bbch.melt)
}
plot_confusionMatrix_CROP <- function(df.melt, df.sums, cm.index.overal.table, save.name){
  
  ggplot(df.melt, aes(Var1, Var2)) +
    geom_tile(aes(fill = value), colour = "white") +
    #scale_fill_gradientn(limits = c(0,300),colours=c("navyblue", "darkmagenta", "darkorange1"),breaks=b, labels=format(b))
    scale_fill_distiller(limits = c(1,max(sort(df.sums$value[df.sums$Var1 != 'TOTAL' | df.sums$Var2 != 'TOTAL']))),type = 'div', palette = "RdYlGn",name="Number of\n Examples", direction = 1)+
    geom_text(aes(label = ifelse(value > 0 ,as.character(value),'')), color = 'gray32')+
    geom_tile(data=df.sums, fill = 'gray')+
    geom_text(data=df.sums, aes(label = as.character(value)))+
    theme(axis.text.x = element_text(angle = -90))+
    labs(x = 'Label')+
    labs(y = 'Prediction')+
    scale_x_discrete(position = "top")+
    theme(legend.justification =  'top') +
    annotation_custom(tableGrob(cm.index.overal.table, rows = NULL), xmin=9.9, xmax=21, ymin=4, ymax=5)
  ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
}

#run it
dfCompareAll <- data.frame()
for(i in runs){
  print(i)
  
  #declare save directory
  save.dir <- paste0(diskDir,'/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/figures-all_images-entr/',i)
  if(!dir.exists(save.dir)){
    dir.create(save.dir, recursive = T)
  }
  
  #load in cnn output
  df.crop <- load_and_preprocess(file.path(diskDir,'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-all-images/'), i, F, T, F)
  
  #preprocess for confusion Matrix
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop, table.crop.byClass.1)
  
  #temporary table for comparison between runs
  t_comp <- create_intCompareTable(i, cm.crop.index.overal.table, file.path(diskDir,'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/resume-train.csv'))
  dfCompareAll <- rbind(dfCompareAll, t_comp)
  
  #melt and order the confusion matrix
  df.crop.melt <- melt_and_order_df(cm.crop.with.sums.1)
  
  #collect rows and columns to exclude from coloring scheme and leave in neutral gray
  df.crop.sums <- df.crop.melt[df.crop.melt$Var2 %in% c('TOTAL', 'PA', 'UA') | df.crop.melt$Var1 %in% c('TOTAL', 'PA', 'UA') ,]
  
  #plot and save CM
  plot_confusionMatrix_CROP(df.crop.melt, df.crop.sums, cm.crop.index.overal.table, 'cm_crop.png')
}

dfCompareAll[order(dfCompareAll$mf1, decreasing = T),]
write.csv(dfCompareAll, file.path(save.dir, 'dfCompareAll.csv'))
