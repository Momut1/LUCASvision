rm(list=ls())
library(DescTools)
library(ggplot2)
library(ggpubr)
library(Hmisc) # cut2
library(gridExtra)
library(caret)
library(xtable)

##### SHANON ENTROPY FILTERING
#The lowest entropy is calculated for a random variable that has a single event with a probability of 1.0, a certainty. 
#The largest entropy for a random variable will be if all events are equally likely.

#In the case where one event dominates, such as a skewed probability distribution, then there is less surprise and the distribution will have a lower entropy. 
#In the case where no event dominates another, such as equal or approximately equal probability distribution, 
#then we would expect larger or maximum entropy.
load_and_preprocess_entropy <- function(pathtocnnoutcsvs, runn){
  #read cnn out csv of model configuration
  df.crop <- read.csv(paste0(pathtocnnoutcsvs,runn,'/cnn_output_data_check.csv'), stringsAsFactors = F)
  
  #convert cnn_labels to uppercase
  df.crop$cnn_labels <- toupper(df.crop$cnn_labels)
  
  #are we calculating entropy
  df.crop <- calculateEntropy(df.crop)
  
  #assign TF for correct classification 
  df.crop$cnn_result <- ifelse(df.crop$cnn_labels == df.crop$lc1, T, F)
  
  return(df.crop)
}

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

findInformationCutOff <- function(df, info_vec){
  
  info_res_df <- data.frame(matrix(NA, nrow=length(info_vec), ncol = 3))
  colnames(info_res_df) <- c('info_cutoff', 'diffInTFvals_per', 'Tbelow1per')
  for(iii in 1:length(info_vec)){
    #print(iii)
    
    info_res_df$info_cutoff[iii] <- info_vec[iii]
    
    #make sure there are entropy values of this range
    if(info_vec[iii] <= max(df$cnn_values_Information)){
      df_highI <- df[df$cnn_values_Information > info_vec[iii],]
      
      df_highI_T_per <- as.data.frame(table(df_highI$cnn_result) /  nrow(df))
      #print(df_highI_T_per$Freq[df_highI_T_per$Var1 == 'TRUE'])
      
      diff <- abs(df_highI_T_per$Freq[1] - df_highI_T_per$Freq[2]) 
      
      if(!is.na(diff)){
        info_res_df$diffInTFvals_per[iii] <- diff      
      }else{
        info_res_df$diffInTFvals_per[iii] <- 0
      }
      
      
      #if i've lost more less than 1 percent of the TRUE classifications, assign T to Tbelow1per
      if(info_res_df$diffInTFvals_per[iii] != 0){# if there is at least one T classification
        
        if(df_highI_T_per$Freq[df_highI_T_per$Var1 == 'TRUE'] < 0.1){
          
          info_res_df$Tbelow1per[iii] <- T
        }
      }else{
        #print('F')
        info_res_df$Tbelow1per[iii] <- F
      }
    }
  }
  
  #remove all NAs
  info_res_df <- info_res_df[! is.na(info_res_df$diffInTFvals_per),]
  info_res_df <- info_res_df[! is.na(info_res_df$Tbelow1per),]
  
  result <- info_res_df$info_cutoff[info_res_df$diffInTFvals_per == max(info_res_df$diffInTFvals_per) & info_res_df$Tbelow1per == TRUE]
  
  if(length(result) == 0 | is.na(result)){
    result <- info_res_df$info_cutoff[info_res_df$diffInTFvals_per == max(info_res_df$diffInTFvals_per)]
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

confusionMatrix_calculate_PA_UA_F1_CROP <- function(df){
  u = sort(union(df$lc1,df$cnn_labels))
  cm = table(factor(df$lc1, u), factor(df$cnn_labels, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.index<-confusionMatrix(cm,mode='prec_recall')
  cm.ByClass<-round(data.frame(cm.index$byClass),digit=4)
  table.byClass.1<-data.frame(UA=cm.ByClass$Recall,PA=cm.ByClass$Precision,F_score=cm.ByClass$F1)
  rownames(table.byClass.1)<-rownames(cm)
  
  #control for instances of NaN for Precision or Recall
  if(any(is.na(table.byClass.1))){
    table.byClass.1[is.na(table.byClass.1)] <- 0
  }
  
  return(table.byClass.1)
}
confusionMatrix_tableForm_CROP <- function(df, table.byClass.1){
  u = sort(union(df$lc1,df$cnn_labels))
  cm = table(factor(df$lc1, u), factor(df$cnn_labels, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  #Add Precision and Recall to confusion matrix
  cm.with.sums.1 <- rbind(cm.with.sums.1, UA = c(round((table.byClass.1$UA * 100), 1), NA))
  cm.with.sums.1 <- cbind(cm.with.sums.1, PA = c(round((table.byClass.1$PA * 100), 1), NA, NA))
  
  return(cm.with.sums.1)
}
confusionMatrix_produceGrobTable_CROP <- function(df, table.byClass.1){
  u = sort(union(df$lc1,df$cnn_labels))
  cm = table(factor(df$lc1, u), factor(df$cnn_labels, u),useNA = "ifany")
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


#Recall = TruePositives / (TruePositives + FalseNegatives)
0/ (0+1)
#Precision = TruePositives / (TruePositives + FalsePositives)
0 / (0 + 0) 
0/0

diskDir <- '/eos/jeodpp/data/projects/REFOCUS/data'
pathtocnnoutcsvs <- file.path(diskDir, "LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-all-images/")
runn <- 78
save.dir <- file.path(diskDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/figures-entropyAndInf2')

if(!dir.exists(save.dir)){
  dir.create(save.dir)
}


#read cnn out csv of model configuration
df.crop <- load_and_preprocess_entropy(pathtocnnoutcsvs, runn)

#information vs probability plot
save.name <- paste0('information_', runn, '.png')
ggplot(df.crop, aes(cnn_values, cnn_values_Information)) + 
  geom_point()+
  geom_line()
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

#entropy vs probability plot
save.name <- paste0('entropy_', runn, '.png')
ggplot(df.crop, aes(cnn_values, cnn_values_DescToolsentropy)) + 
  geom_point()
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

#information vs entropy plot
#low information == high probability
#low entropy == a more prounounced maximum probability for the winning class
# ? should variables in this plot be standardized in order to compare them ? - (i - mean(pop)) / std(pop)
df.crop_toscale <- df.crop

#SCALE ENTROPY AND INFORMATION
#get information and etropy out in order to scale only them
scaled.dat <- as.data.frame(scale(df.crop_toscale[, c(7,8)], center = T, scale = T))
scaled.dat <- cbind(scaled.dat, df.crop_toscale[,9])
colnames(scaled.dat) <- c('cnn_values_DescToolsentropy', 'cnn_values_Information', 'cnn_result')

save.name <- paste0('scaledEntropy_', runn, '.png')
ggplot(scaled.dat, aes(cnn_values_DescToolsentropy, cnn_values_Information)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data = scaled.dat[scaled.dat$cnn_result == F,], color = 'red')+
  geom_smooth(data = scaled.dat[scaled.dat$cnn_result == T,], color = 'blue') + 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


# centering - like standardizing, but without dividing by the std
save.name <- paste0('centeredEntropy_', runn, '.png')
ggplot(df.crop, aes(cnn_values_DescToolsentropy, cnn_values_Information)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =df.crop[df.crop$cnn_result == F,], color = 'red')+
  geom_smooth(data =df.crop[df.crop$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

#PLOT ENTROPY AND PROBABILITY
entr_vec <- seq(1.5,3.0, 0.01)
entr_cutoff <- findEntropyCutOff(df.crop, entr_vec)

proba_vec <- seq(0.0,1.0, 0.01)
proba_cutoff <- findProbaCutOff(df.crop, proba_vec)

#information
info_vec <- seq(min(df.crop$cnn_values_Information), max(df.crop$cnn_values_Information), 0.01)
#info_cutoff <- findInformationCutOff(df.crop, info_vec)
info_cutoff <- 1.0

save.name <- paste0('entropyAndProba_', runn, '.png')
cplot <- ggplot(df.crop, aes(cnn_values_DescToolsentropy, cnn_values_Information)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =df.crop[df.crop$cnn_result == F,], color = 'red')+
  geom_smooth(data =df.crop[df.crop$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')+
  geom_vline(xintercept = entr_cutoff, linetype = "dashed")+
  geom_hline(yintercept = info_cutoff, linetype = "dashed")
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

# Marginal density plot of x (top panel)
yplot <- ggplot(df.crop, aes(cnn_values, fill=cnn_result_Information)) + 
  geom_density(alpha=.5) + 
  #scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")+
  theme_minimal()+
  coord_flip()

# Marginal density plot of y (right panel)
xplot <- ggplot(df.crop, aes(cnn_values_DescToolsentropy, fill=cnn_result)) + 
  geom_density(alpha=.5) + 
  #scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")+
  theme_minimal()

# Arranging the plot
save.name <- paste0('entropyAndProbaWMarginals_', runn, '.png')
ggarrange(xplot, NULL, cplot, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


####compare proba and entropy for a fixed entropy (1.69)-info(1.0) orthogonal point 
df.crop_eFiltered <- df.crop[df.crop$cnn_values_DescToolsentropy <= 1.69,]

#preprocess for confusion Matrix
table.crop.byClass.1_e <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_eFiltered)
table.crop.byClass.1_e$f_type <- 'entropy'
table.crop.byClass.1_e$lc1 <- rownames(table.crop.byClass.1_e)

####find proba filter than gives the same number of images as the en-info one
nrow(df.crop[df.crop$cnn_values >= 0.5226,]) == nrow(df.crop_eFiltered)
df.crop_pFiltered <- df.crop[df.crop$cnn_values >= 0.5226,]

#preprocess for confusion Matrix
table.crop.byClass.1_p <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_pFiltered)
table.crop.byClass.1_p$f_type <- 'probability'
table.crop.byClass.1_p$lc1 <- rownames(table.crop.byClass.1_p)

for(i in 1:3){
  #bind them both together
  table.crop.byClass.1_b <- rbind(table.crop.byClass.1_e[c(i,4,5)],table.crop.byClass.1_p[c(i,4,5)])
  table.crop.byClass.1_b$colour <- ifelse(table.crop.byClass.1_b[,1][table.crop.byClass.1_b$f_type == 'probability'] <  table.crop.byClass.1_b[,1][table.crop.byClass.1_b$f_type == 'entropy'], "000000", NA)
  
  if(i == 1){
    save.name <- paste0('barplotCompareFilters_UAPerClass_', runn, '.png')
    #plot
    ggplot(data=table.crop.byClass.1_b, aes(x=lc1, y=UA, fill=f_type, colour = as.factor(colour))) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_manual(values=c('#999999','#E69F00'))+
      theme_minimal()
    ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
  }
  if(i == 2){
    save.name <- paste0('barplotCompareFilters_PAPerClass_', runn, '.png')
    #plot
    ggplot(data=table.crop.byClass.1_b, aes(x=lc1, y=PA, fill=f_type, colour = as.factor(colour))) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_manual(values=c('#999999','#E69F00'))+
      theme_minimal()
    ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
  }
  if(i == 3){
    save.name <- paste0('barplotCompareFilters_F1PerClass_', runn, '.png')
    #plot
    ggplot(data=table.crop.byClass.1_b, aes(x=lc1, y=F_score, fill=f_type, colour = as.factor(colour))) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_manual(values=c('#999999','#E69F00'))+
      theme_minimal()
    ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
  }
  
}


#plot the confusion matrix for this proba threshold
cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_pFiltered, table.crop.byClass.1_p)
cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_pFiltered, table.crop.byClass.1_p)
#melt and order the confusion matrix
df.crop.melt <- melt_and_order_df(cm.crop.with.sums.1)

#collect rows and columns to exclude from coloring scheme and leave in neutral gray
df.crop.sums <- df.crop.melt[df.crop.melt$Var2 %in% c('TOTAL', 'PA', 'UA') | df.crop.melt$Var1 %in% c('TOTAL', 'PA', 'UA') ,]

#plot and save CM
plot_confusionMatrix_CROP(df.crop.melt, df.crop.sums, cm.crop.index.overal.table, 'cm_crop.png')


#####SPLIT THE DATA IN terms of 10 equal intervals for both E and P and check the number of images at each split
df.crop_split_E <- split(df.crop, cut2(df.crop$cnn_values_DescToolsentropy, g=10))

#fetch the E split point
e_cutoff10 <- c()
for(i in 1:length(df.crop_split_E)){
  #print(i)
  df.crop_split_E_i <- df.crop_split_E[[i]]
  e_cutoff10 <- c(e_cutoff10, max(df.crop_split_E_i$cnn_values_DescToolsentropy))
  #print('-------------------')
}

df.crop_split_P <- split(df.crop, cut2(df.crop$cnn_values, g=10))

#fetch the E split point
p_cutoff10 <- c()
for(i in 1:length(df.crop_split_P)){
  #print(i)
  df.crop_split_P_i <- df.crop_split_P[[i]]
  p_cutoff10 <- c(p_cutoff10, max(df.crop_split_P_i$cnn_values))
  #print('-------------------')
}

#remove the last threshold as it produces no confusion matrix and add a first one at 0
p_cutoff10 <- head(p_cutoff10, -1)
p_cutoff10 <- c(p_cutoff10, 0.0)
p_cutoff10 <- sort(p_cutoff10)

#plot
# Overlaid histograms
e_hist_cutoff <- ggplot(df.crop, aes(x=cnn_values_DescToolsentropy, color=cnn_result)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+
  geom_density(alpha=.9) +
  geom_vline(xintercept = e_cutoff10[1],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[2],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[3],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[4],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[5],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[6],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[7],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[8],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[9],linetype = 'dashed')+
  geom_vline(xintercept = e_cutoff10[10],linetype = 'dashed')+
  theme_minimal()

p_hist_cutoff <- ggplot(df.crop, aes(x=cnn_values, color=cnn_result)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+
  geom_density(alpha=.2) +
  geom_vline(xintercept = p_cutoff10[1],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[2],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[3],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[4],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[5],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[6],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[7],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[8],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[9],linetype = 'dashed')+
  geom_vline(xintercept = p_cutoff10[10],linetype = 'dashed')+
  theme_minimal()


b_hist_cutoff <- ggarrange(e_hist_cutoff, p_hist_cutoff, labels = c("A", "B"),
                           common.legend = TRUE, legend = "bottom")
save.name <- paste0('b_hist_cutoff_', runn, '.png')
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


#table showing the situation at each of these cutoff points
e_cutoff_resultsdf <- data.frame(matrix(NA, nrow = length(e_cutoff10), ncol = 5))
colnames(e_cutoff_resultsdf) <- c('c_idx', 'c_value', 'nrows', 'mf1', 'f_left')
#iterate over each cutoff point 
for(c in 1:length(e_cutoff10)){
  c_i <- e_cutoff10[c]
  
  #drop all images with an entropy bigger than entropy cutoff10 i
  df.crop_c_i <- df.crop[df.crop$cnn_values_DescToolsentropy <= c_i,]
  
  #get the MF1
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_c_i)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_c_i, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_c_i, table.crop.byClass.1)
  
  mf1 <- cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']
  
  #assign to table
  e_cutoff_resultsdf$c_idx[c] <- c
  e_cutoff_resultsdf$c_value[c] <- c_i
  e_cutoff_resultsdf$nrows[c] <- nrow(df.crop_c_i)
  e_cutoff_resultsdf$mf1[c] <- mf1
  
  if(nrow(table(df.crop_c_i$cnn_result)) > 1){
    e_cutoff_resultsdf$f_left[c] <- table(df.crop_c_i$cnn_result)[1]
  }else{
    e_cutoff_resultsdf$f_left[c] <- 0
  }
  
}


p_cutoff_resultsdf <- data.frame(matrix(NA, nrow = length(p_cutoff10), ncol = 5))
colnames(p_cutoff_resultsdf) <- c('c_idx', 'c_value', 'nrows', 'mf1', 'f_left')
#iterate over each cutoff point 
for(c in 1:length(p_cutoff10)){
  c_i <- p_cutoff10[c]
  
  #drop all images with an entropy bigger than entropy cutoff10 i
  df.crop_c_i <- df.crop[df.crop$cnn_values >= c_i,]
  
  #get the MF1
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_c_i)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_c_i, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_c_i, table.crop.byClass.1)
  
  mf1 <- cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']
  
  #assign to table
  p_cutoff_resultsdf$c_idx[c] <- c
  p_cutoff_resultsdf$c_value[c] <- c_i
  p_cutoff_resultsdf$nrows[c] <- nrow(df.crop_c_i)
  p_cutoff_resultsdf$mf1[c] <- mf1
  
  if(nrow(table(df.crop_c_i$cnn_result)) > 1){
    p_cutoff_resultsdf$f_left[c] <- table(df.crop_c_i$cnn_result)[1]
  }else{
    p_cutoff_resultsdf$f_left[c] <- 0
  }
}


#break the space up into quadrants and compare the performance from keeping all the images from a specific quandrant or combo thereof
### WITH PROBA
quadrant_df <- data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(quadrant_df) <- c('Quadrant', 'True', 'False', 'TF Ratio')
for(i in 1:4){
  
  #quadrant 1 - bottom left corner
  if(i == 1){
    df.crop_q <- df.crop[df.crop$cnn_values < proba_cutoff & df.crop$cnn_values_DescToolsentropy < entr_cutoff,]
  }
  
  #quadrant 2 - bottom right corner
  if(i == 2){
    df.crop_q <- df.crop[df.crop$cnn_values < proba_cutoff & df.crop$cnn_values_DescToolsentropy > entr_cutoff,]
  }
  
  #quadrant 3 - top left corner
  if(i == 3){
    df.crop_q <- df.crop[df.crop$cnn_values > proba_cutoff & df.crop$cnn_values_DescToolsentropy < entr_cutoff,]
  }
  
  #quadrant 4 - top right corner
  if(i == 4){
    df.crop_q <- df.crop[df.crop$cnn_values > proba_cutoff & df.crop$cnn_values_DescToolsentropy > entr_cutoff,]
  }
  
  quadrant_df$Quadrant[i] <- i
  quadrant_df$False[i] <- table(df.crop_q$cnn_result)[1]
  quadrant_df$True[i] <- table(df.crop_q$cnn_result)[2]
  quadrant_df$`TF Ratio`[i] <- round(table(df.crop_q$cnn_result)[2] / table(df.crop_q$cnn_result)[1],2)
  
  # print(i)
  # print(table(df.crop_q$cnn_result))
  # print(table(df.crop_q$cnn_result)[2] / table(df.crop_q$cnn_result)[1])
  # print('----------------')
}

### WITH INFO
quadrant_df <- data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(quadrant_df) <- c('Quadrant', 'True', 'False', 'TF Ratio')
for(i in 1:4){
  
  #quadrant 1 - bottom left corner
  if(i == 1){
    df.crop_q <- df.crop[df.crop$cnn_values_Information < info_cutoff & df.crop$cnn_values_DescToolsentropy < entr_cutoff,]
  }
  
  #quadrant 2 - bottom right corner
  if(i == 2){
    df.crop_q <- df.crop[df.crop$cnn_values_Information < info_cutoff & df.crop$cnn_values_DescToolsentropy > entr_cutoff,]
  }
  
  #quadrant 3 - top left corner
  if(i == 3){
    df.crop_q <- df.crop[df.crop$cnn_values_Information > info_cutoff & df.crop$cnn_values_DescToolsentropy < entr_cutoff,]
  }
  
  #quadrant 4 - top right corner
  if(i == 4){
    df.crop_q <- df.crop[df.crop$cnn_values_Information > info_cutoff & df.crop$cnn_values_DescToolsentropy > entr_cutoff,]
  }
  
  quadrant_df$Quadrant[i] <- i
  quadrant_df$False[i] <- table(df.crop_q$cnn_result)[1]
  quadrant_df$True[i] <- table(df.crop_q$cnn_result)[2]
  quadrant_df$`TF Ratio`[i] <- round(table(df.crop_q$cnn_result)[2] / table(df.crop_q$cnn_result)[1],2)
  
  # print(i)
  # print(table(df.crop_q$cnn_result))
  # print(table(df.crop_q$cnn_result)[2] / table(df.crop_q$cnn_result)[1])
  # print('----------------')
}

save.name <- paste0('entropyAndProbaQuadrantsTable_', runn, '.png')
ggplot(df.crop, aes(cnn_values_DescToolsentropy, cnn_values)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =df.crop[df.crop$cnn_result == F,], color = 'red')+
  geom_smooth(data =df.crop[df.crop$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')+
  geom_vline(xintercept = entr_cutoff, linetype = "dashed")+
  geom_hline(yintercept = proba_cutoff, linetype = "dashed")+
  annotate("text", x=0.1, y=0.3, label= "1", size=15) + 
  annotate("text", x=3, y=0.3, label = "2", size=15)+
  annotate("text", x=0.1, y=0.8, label= "3", size=15) + 
  annotate("text", x=3, y=0.8, label = "4", size=15)+
  annotation_custom(tableGrob(quadrant_df, rows = NULL), xmin=2, xmax=3, ymin=0.9, ymax=1)+
  labs(x="Entropy", y="Maximum probability")
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


#WITH MARGINAL PLOTS
quadrant_df$`TF Ratio` <- NULL
colnames(quadrant_df) <- c('Q', 'True', "False")
save.name <- paste0('entropyAndInfoQuadrantsTable_', runn, '.png')
cplot <- ggplot(df.crop, aes(cnn_values_DescToolsentropy, cnn_values_Information)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =df.crop[df.crop$cnn_result == F,], color = 'red')+
  geom_smooth(data =df.crop[df.crop$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')+
  geom_vline(xintercept = entr_cutoff, linetype = "dashed")+
  geom_hline(yintercept = info_cutoff, linetype = "dashed")+
  annotate("text", x=0.1, y=0.3, label= "1", size=15) + 
  annotate("text", x=3, y=0.3, label = "2", size=15)+
  annotate("text", x=0.1, y=1.8, label= "3", size=15) + 
  annotate("text", x=3, y=1.8, label = "4", size=15)+
  annotation_custom(tableGrob(quadrant_df, rows = NULL), xmin=0.5, xmax=1, ymin=1.5, ymax=2)+
  labs(x="Entropy", y="Information")
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

# Marginal density plot of x (top panel)
yplot <- ggplot(df.crop, aes(cnn_values_Information, fill=cnn_result)) + 
  geom_density(alpha=.5) + 
  #scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")+
  theme_minimal()+
  coord_flip()+
  labs(x="Information", y="density")+
  theme(legend.position =  'none')

# Marginal density plot of y (right panel)
xplot <- ggplot(df.crop, aes(cnn_values_DescToolsentropy, fill=cnn_result)) + 
  geom_density(alpha=.5) + 
  #scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")+
  theme_minimal()+
  labs(x="Entropy", y="density")+
  theme(legend.position =  'none')


# Arranging the plot
save.name <- paste0('entropyAndProbaWMarginals_', runn, '.png')
ggarrange(xplot, NULL, cplot, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = F)
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


# WITH INFO
#### test the performance for either of the following configurations:
##### - 1. ONLY Information filter - Q1 + Q2
##### - 2. ONLY entropy filter - Q1 + Q3
##### - 3. Keep accepted by BOTH filters - Q1
##### - 4. Remove ONLY access of both - Q1 + Q2 + Q3

quadrantFilter_dr <- data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(quadrantFilter_dr) <- c('QM', 'Qs', '# of images', 'M-F1')
for(i in 1:4){
  
  if(i == 1){
    df.crop_f <- df.crop[df.crop$cnn_values_Information < info_cutoff,]
    qts <- 'Q1, Q2' 
  }
  
  if(i == 2){
    df.crop_f <- df.crop[df.crop$cnn_values_DescToolsentropy < entr_cutoff,]
    qts <- 'Q1, Q3' 
  }
  
  if(i == 3){
    df.crop_f <- df.crop[df.crop$cnn_values_DescToolsentropy < entr_cutoff & df.crop$cnn_values_Information < info_cutoff,]
    qts <- 'Q1' 
  }
  
  if(i == 4){
    df.crop_Q4 <- df.crop[df.crop$cnn_values_Information > info_cutoff & df.crop$cnn_values_DescToolsentropy > entr_cutoff,]
    df.crop_f <- df.crop[! df.crop$pointidyear %in% df.crop_Q4$pointidyear,]
    qts <- 'Q1, Q2, Q3' 
  }
  
  #get the MF1
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_f)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_f, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_f, table.crop.byClass.1)
  
  mf1 <- cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']
  
  quadrantFilter_dr$`QM`[i] <- i
  quadrantFilter_dr$Qs[i] <- qts
  quadrantFilter_dr$`# of images`[i] <- nrow(df.crop_f)
  quadrantFilter_dr$`M-F1`[i] <- mf1
}
quadrantFilter_dr
xtable(quadrantFilter_dr)

# WITH PROBA
#### test the performance for either of the following configurations:
##### - 1. ONLY probability filter - Q3 + Q4
##### - 2. ONLY entropy filter - Q3 + Q1
##### - 3. Keep accepted by BOTH filters - Q3
##### - 4. Remove ONLY access of both - Q1 + Q3 + Q4

quadrantFilter_dr <- data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(quadrantFilter_dr) <- c('QM idx', 'Quadrants', 'Number of images', 'M-F1')
for(i in 1:4){
  
  if(i == 1){
    df.crop_f <- df.crop[df.crop$cnn_values > proba_cutoff,]
    qts <- 'Q3, Q4' 
  }
  
  if(i == 2){
    df.crop_f <- df.crop[df.crop$cnn_values_DescToolsentropy < entr_cutoff,]
    qts <- 'Q3, Q1' 
  }
  
  if(i == 3){
    df.crop_f <- df.crop[df.crop$cnn_values_DescToolsentropy < entr_cutoff & df.crop$cnn_values > proba_cutoff,]
    qts <- 'Q3' 
  }
  
  if(i == 4){
    df.crop_Q2 <- df.crop[df.crop$cnn_values < proba_cutoff & df.crop$cnn_values_DescToolsentropy > entr_cutoff,]
    df.crop_f <- df.crop[! df.crop$pointidyear %in% df.crop_Q2$pointidyear,]
    qts <- 'Q1, Q3, Q4' 
  }
  
  #get the MF1
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_f)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_f, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_f, table.crop.byClass.1)
  
  mf1 <- cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']
  
  quadrantFilter_dr$`QM idx`[i] <- i
  quadrantFilter_dr$Quadrants[i] <- qts
  quadrantFilter_dr$`Number of images`[i] <- nrow(df.crop_f)
  quadrantFilter_dr$`M-F1`[i] <- mf1
}
quadrantFilter_dr








### normalize the entropy in order to compare entropy filter to probability filter
#### formula: y = ((x - xmin) / range(x)) * n 
df.crop$cnn_values_DescToolsentropy_normalized <- ((df.crop$cnn_values_DescToolsentropy - min(df.crop$cnn_values_DescToolsentropy)) / (max(df.crop$cnn_values_DescToolsentropy - min(df.crop$cnn_values_DescToolsentropy)))) * 1


#assign TF for correct classification 
df.crop$cnn_result <- ifelse(df.crop$cnn_labels == df.crop$lc1, T, F)

entr_vec <- seq(0.01,1.0, 0.01)

entro_mf1_evol <- data.frame(matrix(NA, ncol = 5, nrow = length(entr_vec)))
colnames(entro_mf1_evol) <- c('entropy_cutoff', 'nrows', 'mf1', 'prec_rec_0', 'numOfClasses')
for(ii in 1:length(entr_vec)){
  entro_mf1_evol$entropy_cutoff[ii] <- entr_vec[ii]
  
  #remove all examples that have an entropy less than the cutoff
  df.crop_int <- df.crop[df.crop$cnn_values_DescToolsentropy_normalized < entr_vec[ii],]
  entro_mf1_evol$numOfClasses[ii] <- length(unique(df.crop_int$lc1))
  
  #how many images left
  entro_mf1_evol$nrows[ii] <- nrow(df.crop_int)
  
  #preprocess for confusion Matrix
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_int)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_int, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_int, table.crop.byClass.1)
  
  mf1 <- cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']
  
  entro_mf1_evol$mf1[ii] <- mf1
  
  #if there is any the precisions or recalls == 0, then flag these
  if(any(table.crop.byClass.1 == 0)){
    entro_mf1_evol$prec_rec_0[ii] <- T
  }else{
    entro_mf1_evol$prec_rec_0[ii] <- F
  }
}

entro_mf1_evol$numOfClasses <- as.factor(entro_mf1_evol$numOfClasses)
save.name <- paste0('entropyFilter_', runn, '.png')
en <- ggplot(entro_mf1_evol, aes(entropy_cutoff, mf1)) +
  geom_point(color = '#999999', aes(size = nrows, alpha = numOfClasses))+
  geom_line()+
  theme_minimal()+
  labs(x="Entropy filter", y="M-F1", size="Number of images", alpha="Number of classes")+ 
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position="None")
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


###filter on probability to see the difference
#read cnn out csv of model configuration
df.crop <- read.csv(paste0(pathtocnnoutcsvs,runn,'/cnn_output_data_check.csv'), stringsAsFactors = F)

hist(df.crop$cnn_values)
round(min(df.crop$cnn_values), 3)

proba_vec <- seq(round(min(df.crop$cnn_values), 2), 1.0, 0.01)
proba_mf1_evol <- data.frame(matrix(NA, ncol = 5, nrow = length(proba_vec)))
colnames(proba_mf1_evol) <- c('proba_cutoff', 'nrows', 'mf1', 'prec_rec_0', 'numOfClasses')
for(ii in 1:length(entr_vec)){
  proba_mf1_evol$proba_cutoff[ii] <- proba_vec[ii]
  
  #remove all examples that have an entropy less than the cutoff
  df.crop_int <- df.crop[df.crop$cnn_values >= proba_vec[ii],]
  proba_mf1_evol$numOfClasses[ii] <- length(unique(df.crop_int$lc1))
  
  #how many images left
  proba_mf1_evol$nrows[ii] <- nrow(df.crop_int)
  
  #preprocess for confusion Matrix
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_int)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_int, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_int, table.crop.byClass.1)
  
  mf1 <- cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']
  
  proba_mf1_evol$mf1[ii] <- mf1
  
  #if there is any the precisions or recalls == 0, then flag these
  if(any(table.crop.byClass.1 == 0)){
    proba_mf1_evol$prec_rec_0[ii] <- T
  }else{
    proba_mf1_evol$prec_rec_0[ii] <- F
  }
}


proba_mf1_evol$numOfClasses <- as.factor(proba_mf1_evol$numOfClasses)
save.name <- paste0('probaFilter_', runn, '.png')
prob <- ggplot(proba_mf1_evol, aes(proba_cutoff, mf1)) +
  geom_point(color = '#E69F00', aes(size = nrows, alpha = numOfClasses))+
  geom_line()+
  scale_x_reverse()+ 
  theme_minimal()+
  labs(x="Probability filter", y="M-F1", size="Number of images", alpha="Number of classes")+ 
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position='None')
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

# ggplot(proba_mf1_evol, aes(proba_cutoff, mf1)) +
#   geom_point(aes(size = nrows, alpha = numOfClasses, color = prec_rec_0))+
#   geom_line() + 
#   scale_x_reverse()+
#   theme_minimal()


#combine the two dfs and see which filtering performs better
#make sure they are the same length
entro_mf1_evol_cut <- entro_mf1_evol[entro_mf1_evol$entropy_cutoff >= min(proba_mf1_evol$proba_cutoff),]

#label the dfs before rbinding
entro_mf1_evol_cut$cutoff_type <- 'entropy'
proba_mf1_evol$cutoff_type <- 'probability'

#take only the columns you need
dfCompareTypes <- as.data.frame(rbind(as.matrix(entro_mf1_evol_cut[,c(1,2,6)]), as.matrix(proba_mf1_evol[,c(1,2,6)])))
colnames(dfCompareTypes) <- c('cutoff', 'nrows', 'cutoff_type')

#convert to numeric
dfCompareTypes$cutoff <- as.numeric(dfCompareTypes$cutoff)
dfCompareTypes$nrows <- as.numeric(dfCompareTypes$nrows)

#flip the probability values, because they are inversely related to entropy 
dfCompareTypes$nrows[dfCompareTypes$cutoff_type == 'probability'] <- rev(dfCompareTypes$nrows[dfCompareTypes$cutoff_type == 'probability'])

#plot
save.name <- paste0('barplotCompareFilters4Nrows_', runn, '.png')
filt <- ggplot(data=dfCompareTypes, aes(x=cutoff, y=nrows, fill=cutoff_type)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  theme_minimal()+ 
  labs(x="Filter value", y="Number of images", fill="Type of filter")+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.1,.75))
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

#grid arrange
save.name <- paste0('gridArrangeCompareFilters4Nrows_', runn, '.png')
ggarrange(filt,                                                 # First row with scatter plot
          ggarrange(en, prob, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A"                                        # Labels of the scatter plot
)
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')





### IDEA - ROC curve with entropy instead of probability

#remove __RAW columns (the ones for entropy)
df.crop <- df.crop[,-c(grep('__RAW', colnames(df.crop)))]
df.crop <- df.crop[,c(3,4,1,2)]
colnames(df.crop) <- c('code_max', 'prob_max', 'basename', 'code')

#little check
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



hist(df.crop$cnn_values_DescToolsentropy)
# #check if desctools entropy makes sense
# # Basic scatter plot
ggplot(df.crop, aes(x=cnn_values, y=cnn_values_DescToolsentropy, color=cnn_result)) + geom_point()
# ggsave(file.path(save.dir, 'entr_prob_scatter.png'), width = 11, height = 8, dpi = 150, units = "in", device='png')
# 

#### FIT WEIRD FUNCTIONS ON GRAPH FOR THRESHOLDING
#installing package, if not yet available
library(devtools)
#install_github("onofriandreapg/aomisc")
#library(aomisc)

#install_github("onofriandreapg/drcSeedGerm")
library(drcSeedGerm)

Tlev <- c(2, 5, 10, 15, 20, 25)
GR <- c(0, 0, 0.21, 0.49, 0.68, 0.86)
modGH <- drm(GR ~ Tlev, fct = GRT.GH())
library(sandwich); library(lmtest)
coeftest(modGH, vcov = sandwich)




pathtocnnoutcsvs <- "/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results-all-images_py/"
runn <- 4
df.crop <- load_and_preprocess_funkyFunc(pathtocnnoutcsvs, runn)








