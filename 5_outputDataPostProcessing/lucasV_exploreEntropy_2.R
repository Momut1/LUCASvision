rm(list=ls())
library(DescTools)
library(ggplot2)
library(caret)
library(patchwork)
library(gridExtra)


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
    lc1_outputVec <- gsub(" ","",gsub("\\]","",gsub("\\[","",gsub("'", "",strsplit(df.crop[ii,1][1], ',')[[1]]))))
    
    #idx of the winning class (ci)
    c_i <- df.crop$cnn_labels[ii]
    p_i <- entropy_input[which(c_i == lc1_outputVec)]
    
    #idx of the reference class (ci*)
    c_i_star <- df.crop$lc1[ii]
    p_i_star <- entropy_input[which(c_i_star == lc1_outputVec)]
    
    #difference in information
    d_info <- log2(p_i / p_i_star)
    
    #remove the reference class from the vectors
    edi_input_proba <- entropy_input[-c(which(c_i_star == lc1_outputVec))]
    
    #edi_input_proba<- c(0.3, 0.1, 0.1)
    #p_i_star <- 0.5
    
    #log2(p_i_star) - (1/1-p_i_star) * sum(edi_input_proba * log2(edi_input_proba))
    
    #sum((edi_input_proba / 1 - p_i_star) * log(p_i_star / edi_input_proba))
    
    #how many values need to be added together to reach .95 cumsum probability
    #df.crop$mtoe95cumsum[ii] <- match(TRUE,cumsum(entropy_input) >= 0.95)
    
    ##how many of the values are >= 0.1 (hold meaningful information about the confusion made from the example)
    df.crop$mtoe01proba[ii] <- max(which(entropy_input >= 0.1))
    
    #use only these values (1:mtoe95) to calculate the entropy
    #df.crop$cnn_values_meaningfulEntropy[ii] <- Entropy(entropy_input[1:df.crop$mtoe95cumsum[ii]])
    #df.crop$cnn_values_meaningfulRPH[ii] <- Entropy(entropy_input[1:df.crop$mtoe95cumsum[ii]]) / log2(df.crop$mtoe95cumsum[ii])
    
    #use only these values (1:mtoe01) to calculate the entropy
    df.crop$cnn_values_meaningfulEntropy[ii] <- Entropy(entropy_input[1:df.crop$mtoe01proba[ii]])
    df.crop$cnn_values_meaningfulRPH[ii] <- Entropy(entropy_input[1:df.crop$mtoe01proba[ii]]) / log2(df.crop$mtoe01proba[ii])
    
    #calculate entropy with mtoe95cumsum
    #df.crop$cnn_values_RPH[ii] <- Entropy(entropy_input) / log2(df.crop$mtoe95cumsum[ii])
    
    #calculate entropy with mtoe01proba
    df.crop$cnn_values_RPH[ii] <- Entropy(entropy_input) / log2(df.crop$mtoe01proba[ii])
    
    #desctools entropy
    df.crop$cnn_values_DescToolsentropy[ii] <- Entropy(entropy_input)
    
    #calculate the information of the event
    df.crop$cnn_values_Information[ii] <- -log2(df.crop$cnn_values[ii])
    
    #calculate your own entropy
    #df.crop$cnn_values_YourOwnentropy[ii] <- -sum(entropy_input * log2(entropy_input))
  }
  
  return(df.crop)
}

calculateERP <- function(df.crop){
  for(ii in 1:nrow(df.crop)){
    entropy_input <- as.numeric(gsub(" ","",gsub("\\]","",gsub("\\[","",gsub("'", "",strsplit(df.crop[ii,2][1], ',')[[1]])))))
    lc1_outputVec <- gsub(" ","",gsub("\\]","",gsub("\\[","",gsub("'", "",strsplit(df.crop[ii,1][1], ',')[[1]]))))
    
    k <- length(entropy_input)
    
    #idx of the winning class (ci*) == reference class
    c_i_star <- df.crop$cnn_labels[ii]
    p_i_star <- entropy_input[which(c_i_star == lc1_outputVec)]
    
    #edi input
    edi_input_proba <- entropy_input[2:length(entropy_input)]
    
    df.crop$edi[ii] <- log(p_i_star) - (1/(1-p_i_star)) * sum(edi_input_proba * log(edi_input_proba))
    df.crop$lpi[ii] <- log(p_i_star) - log(1-p_i_star)
    df.crop$upi[ii] <- log(p_i_star) - log((1 - p_i_star) / (k - 1))
    
    df.crop$erp[ii] <- exp(df.crop$edi[ii]) / (exp(df.crop$edi[ii]) + (k - 1))
    
    #desctools entropy
    df.crop$cnn_values_DescToolsentropy[ii] <- Entropy(entropy_input)
  }
  
  df.crop$cnn_result <- ifelse(df.crop$cnn_labels == df.crop$lc1, T, F)
  

  
  return(df.crop)
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

findERPCutOff <- function(df, erp_vec){
  
  erp_res_df <- data.frame(matrix(NA, nrow=length(erp_vec), ncol = 3))
  colnames(erp_res_df) <- c('erp_cutoff', 'diffInTFvals_per', 'Tbelow1per')
  for(iii in 1:length(erp_vec)){
    #print(iii)
    
    erp_res_df$erp_cutoff[iii] <- erp_vec[iii]
    
    #make sure there are erp values of this range
    if(erp_vec[iii] <= max(df$erp)){
      df_highE <- df[df$erp > erp_vec[iii],]
      
      df_highE_T_per <- as.data.frame(table(df_highE$cnn_result) /  nrow(df))
      #print(df_highE_T_per$Freq[df_highE_T_per$Var1 == 'TRUE'])
      
      diff <- abs(df_highE_T_per$Freq[1] - df_highE_T_per$Freq[2]) 
      
      if(!is.na(diff)){
        erp_res_df$diffInTFvals_per[iii] <- diff      
      }else{
        erp_res_df$diffInTFvals_per[iii] <- 0
      }
      
      
      #if i've lost more less than 1 percent of the TRUE classifications, assign T to Tbelow1per
      if(erp_res_df$diffInTFvals_per[iii] != 0){# if there is at least one T classification
        
        if(df_highE_T_per$Freq[df_highE_T_per$Var1 == 'TRUE'] < 0.1){
          
          erp_res_df$Tbelow1per[iii] <- T
        }
      }else{
        #print('F')
        erp_res_df$Tbelow1per[iii] <- F
      }
    }
  }
  
  #remove all NAs
  erp_res_df <- erp_res_df[! is.na(erp_res_df$diffInTFvals_per) | ! is.na(erp_res_df$Tbelow1per),]
  
  result <- erp_res_df$erp_cutoff[erp_res_df$diffInTFvals_per == max(erp_res_df$diffInTFvals_per) & erp_res_df$Tbelow1per == TRUE]
  
  if(length(result) == 0 | is.na(result)){
    result <- erp_res_df$erp_cutoff[erp_res_df$diffInTFvals_per == max(erp_res_df$diffInTFvals_per)]
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

#res78 <- load_and_preprocess_entropy('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS_2/Results-all-images_py/', 78)
#head(res78)

#res78$cnn_values_RPH_Maselli <- res78$cnn_values_DescToolsentropy / max(res78$cnn_values_DescToolsentropy)

df.crop <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS_2/Results-all-images_py/78/cnn_output_data_check.csv', stringsAsFactors = F)
res78 <- calculateERP(df.crop)
save.dir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/figs4paper/'

#histogram w density
a <- ggplot(res78[res78$cnn_result == T,], aes(x=erp)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") 

b <- ggplot(res78[res78$cnn_result == F,], aes(x=erp)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

a + b

#just histograms
a <- ggplot(res78[res78$cnn_result == T,], aes(x=erp)) + 
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  xlab('ERP')+ylab('Frequency')+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold",size=14,),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))

b <- ggplot(res78[res78$cnn_result == F,], aes(x=erp)) + 
  geom_histogram(color="black", fill="white")+
  xlab('ERP')+ylab('Frequency')+
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold",size=14,),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))

histoplot <- a+b+ plot_annotation(tag_levels = 'A')
save.name <- 'histoplot.png'
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

#compare with max probability
#just histograms
a1 <- ggplot(res78[res78$cnn_result == T,], aes(x=cnn_values)) + 
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  xlab('Maximum Probability')+ylab('Frequency')+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold",size=14,),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))

b1 <- ggplot(res78[res78$cnn_result == F,], aes(x=cnn_values)) + 
  geom_histogram(color="black", fill="white")+
  xlab('Maximum Probability')+ylab('Frequency')+
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold",size=14,),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))

histoplot1 <- a1+b1+ plot_annotation(tag_levels = 'A')
save.name <- 'histoplot_MP.png'
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


#mean class confidence and accuracy assessment
mERP_class <- aggregate(res78, erp ~ lc1, mean)
PA_UA_F1 <- confusionMatrix_calculate_PA_UA_F1_CROP(res78)
PA_UA_F1$lc1 <- row.names(PA_UA_F1)

mERP_F1 <- merge(mERP_class, PA_UA_F1[,c(4,3)], by = 'lc1')

#lucas_legend = read.csv(file.path(dir.dropbox,'/JRC/LANDSENSE/CASE-STUDY-8-LUCAS-COPERNICUS-CLASSIF/table/legend-lucas3.csv'),sep=";")
lucas_legend = read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/scripts/legend-lucas (2).csv', stringsAsFactors = F)
lucas_legend<-lucas_legend[lucas_legend$originalClass %in% unique(mERP_F1$lc1),]
lucas_legend<-lucas_legend[order(lucas_legend$originalClass),]

mClassConfidenceScatter <- ggplot(mERP_F1, aes(x=F_score,y=erp)) +
  geom_point(aes(x=F_score,y=erp,colour=factor(lc1), size = 2))+xlab('M-F1')+ylab('ERP')+ 
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred")+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_manual('Crops',breaks=lucas_legend$originalClass,
                     values=as.character(lucas_legend$colorRGBcdl),
                     labels = as.character(lucas_legend$label))+
  geom_text(x=0.3, y=0.7, label=paste0("Pearson-R = ", round(cor(mERP_F1$erp, mERP_F1$F_score, method = c("pearson")), 4)))+
  theme_minimal()+ 
  guides(size = "none")+
  xlim(0, 1)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold",size=14,),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))
save.name <- 'mClassConfidenceScatter.png'
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

#compare with maximum probability
#mean class confidence and accuracy assessment
mmaxP_class <- aggregate(res78, cnn_values ~ lc1, mean)
PA_UA_F1 <- confusionMatrix_calculate_PA_UA_F1_CROP(res78)
PA_UA_F1$lc1 <- row.names(PA_UA_F1)

mmaxP_F1 <- merge(mmaxP_class, PA_UA_F1[,c(4,3)], by = 'lc1')

#lucas_legend = read.csv(file.path(dir.dropbox,'/JRC/LANDSENSE/CASE-STUDY-8-LUCAS-COPERNICUS-CLASSIF/table/legend-lucas3.csv'),sep=";")
lucas_legend = read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/scripts/legend-lucas (2).csv', stringsAsFactors = F)
lucas_legend<-lucas_legend[lucas_legend$originalClass %in% unique(mERP_F1$lc1),]
lucas_legend<-lucas_legend[order(lucas_legend$originalClass),]

mClassConfidenceScatter_MP <- ggplot(mmaxP_F1, aes(x=F_score,y=cnn_values)) +
  geom_point(aes(x=F_score,y=cnn_values,colour=factor(lc1), size = 2))+xlab('M-F1')+ylab('Maximum Probability')+ 
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred")+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_manual('Crops',breaks=lucas_legend$originalClass,
                     values=as.character(lucas_legend$colorRGBcdl),
                     labels = as.character(lucas_legend$label))+
  geom_text(x=0.3, y=0.7, label=paste0("Pearson-R = ", round(cor(mmaxP_F1$cnn_values, mERP_F1$F_score, method = c("pearson")), 4)))+
  theme_minimal()+ 
  guides(size = "none")+
  xlim(0, 1)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold",size=14,),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))
save.name <- 'mClassConfidenceScatter_MP.png'
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')



####quadrant scatter

ggplot(res78, aes(erp, cnn_values)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =res78[res78$cnn_result == F,], color = 'red')+
  geom_smooth(data =res78[res78$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')#+
#geom_vline(xintercept = entr_cutoff, linetype = "dashed")+
#geom_hline(yintercept = info_cutoff, linetype = "dashed")

entr_proba <- ggplot(res78, aes(erp, cnn_values)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =res78[res78$cnn_result == F,], color = 'red')+
  geom_smooth(data =res78[res78$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')#+
#geom_vline(xintercept = entr_cutoff, linetype = "dashed")+
#geom_hline(yintercept = info_cutoff, linetype = "dashed")

entr_erp <- ggplot(res78, aes(erp, cnn_values_DescToolsentropy)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =res78[res78$cnn_result == F,], color = 'red')+
  geom_smooth(data =res78[res78$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')#+
#geom_vline(xintercept = entr_cutoff, linetype = "dashed")+
#geom_hline(yintercept = info_cutoff, linetype = "dashed")

entr_proba + entr_erp


#finding thresholds
#assign TF for correct classification 
#res78$cnn_result <- ifelse(df.crop$cnn_labels == df.crop$lc1, T, F)
df.crop <- res78
erp_vec <- seq(0.01,1.0, 0.01)

erp_mf1_evol <- data.frame(matrix(NA, ncol = 5, nrow = length(erp_vec)))
colnames(erp_mf1_evol) <- c('erp_cutoff', 'nrows', 'mf1', 'prec_rec_0', 'numOfClasses')
for(ii in 1:length(erp_vec)){
  erp_mf1_evol$erp_cutoff[ii] <- erp_vec[ii]
  
  #remove all examples that have an entropy less than the cutoff
  df.crop_int <- df.crop[df.crop$erp >= erp_vec[ii],]
  
  if((nrow(df.crop_int) > 0) & (length(unique(df.crop_int$lc1)) > 2)){
    erp_mf1_evol$numOfClasses[ii] <- length(unique(df.crop_int$lc1))
    
    #how many images left
    erp_mf1_evol$nrows[ii] <- nrow(df.crop_int)
    
    #preprocess for confusion Matrix
    table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_int)
    cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop_int, table.crop.byClass.1)
    cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop_int, table.crop.byClass.1)
    
    mf1 <- cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1']
    
    erp_mf1_evol$mf1[ii] <- mf1
    
    #if there is any the precisions or recalls == 0, then flag these
    if(any(table.crop.byClass.1 == 0)){
      erp_mf1_evol$prec_rec_0[ii] <- T
    }else{
      erp_mf1_evol$prec_rec_0[ii] <- F
    }
  }
  
}


erp_mf1_evol$numOfClasses <- as.factor(erp_mf1_evol$numOfClasses)
save.name <- paste0('erpFilter_', runn, '.png')
erp_p <- ggplot(erp_mf1_evol, aes(erp_cutoff, mf1)) +
  geom_point(color = '#999999', aes(size = nrows, alpha = numOfClasses))+
  geom_line()+
  theme_minimal()+
  labs(x="ERP filter", y="M-F1", size="Number of images", alpha="Number of classes")+ 
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position="None")+
  xlim(0,1)+ ylim(0,1)
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


###filter on probability to see the difference
#read cnn out csv of model configuration
#df.crop <- read.csv(paste0(pathtocnnoutcsvs,runn,'/cnn_output_data_check.csv'), stringsAsFactors = F)
df.crop <- res78

hist(df.crop$cnn_values)
round(min(df.crop$cnn_values), 3)

proba_vec <- seq(round(min(df.crop$cnn_values), 2), 1.0, 0.01)
proba_mf1_evol <- data.frame(matrix(NA, ncol = 5, nrow = length(proba_vec)))
colnames(proba_mf1_evol) <- c('proba_cutoff', 'nrows', 'mf1', 'prec_rec_0', 'numOfClasses')
for(ii in 1:length(proba_vec)){
  proba_mf1_evol$proba_cutoff[ii] <- proba_vec[ii]
  
  #remove all examples that have an entropy less than the cutoff
  df.crop_int <- df.crop[df.crop$cnn_values >= proba_vec[ii],]
  
  if((nrow(df.crop_int) > 0) & (length(unique(df.crop_int$lc1)) > 2)){
    
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
  
 
}


proba_mf1_evol$numOfClasses <- as.factor(proba_mf1_evol$numOfClasses)
save.name <- paste0('probaFilter_', runn, '.png')
prob_p <- ggplot(proba_mf1_evol, aes(proba_cutoff, mf1)) +
  geom_point(color = '#E69F00', aes(size = nrows, alpha = numOfClasses))+
  geom_line()+
  scale_x_reverse()+ 
  theme_minimal()+
  labs(x="Probability filter", y="M-F1", size="Number of images", alpha="Number of classes")+ 
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position='None')+
  xlim(0,1)+ ylim(0,1)
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

erp_p + prob_p

#combine the two
erp_mf1_evol$cutoff_type <- 'ERP'
proba_mf1_evol$cutoff_type <- 'MP'

colnames(erp_mf1_evol)[1] <- 'cutoff'
colnames(proba_mf1_evol)[1] <- 'cutoff'

both <- rbind(erp_mf1_evol, proba_mf1_evol)

both_p <- ggplot(both, aes(cutoff, mf1)) +
  geom_point(aes(size = nrows, alpha = numOfClasses, color = cutoff_type))+
  geom_line(aes(color = cutoff_type))+
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal(base_size = 20)+
  labs(x="Threshold point", y="M-F1", size="Number of images", alpha="Number of classes", color='Threshold variable')+ 
  theme(legend.position='bottom')+
  xlim(0,1)+ ylim(0.7,1)

both$proportion <- both$nrows / max(both$nrows[!is.na(both$nrows)])
#double Y axis with proportion
both_p_perc <- ggplot(both, aes(cutoff, mf1)) +
  geom_point(aes(size = as.integer(numOfClasses) , color = cutoff_type))+
  geom_line(aes(color = cutoff_type))+
  geom_line(aes(cutoff, proportion, color = cutoff_type))+
  scale_y_continuous(name = 'M-F1',
    sec.axis = sec_axis(trans = ~ . * 100, name = "Percentage of remaining images"))+
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal(base_size = 20)+
  labs(x="Threshold point", y="M-F1", size="Number of classes", color='Threshold variable')+ 
  theme(legend.position='bottom')#+
  #xlim(0,1)+ ylim(0.7,1)

#double Y axis with total number
scaleFactor <- max(both$mf1[!is.na(both$mf1)]) / max(both$nrows[!is.na(both$nrows)])
both_p_num <- ggplot(both, aes(cutoff, mf1)) +
  geom_point(aes(size = as.integer(as.character(numOfClasses)) , color = cutoff_type))+
  geom_line(aes(color = cutoff_type))+
  geom_line(aes(cutoff, proportion, color = cutoff_type, size = 5))+
  scale_y_continuous(name = 'M-F1',
                     sec.axis = sec_axis(trans = ~ . / scaleFactor, name = "Number of remaining images"))+
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal(base_size = 20)+
  labs(x="Threshold point", y="M-F1", size="Number of classes", color='Threshold variable')+ 
  theme(legend.position='bottom')


#combine with histoplot?
###histograms on a single plot
erp_plot <- ggplot(res78, aes(x=erp, color=cnn_result, fill = cnn_result)) +
  geom_histogram(position="dodge")+
  scale_color_manual(values=c("red", "#999999"))+
  scale_fill_manual(values=c("red", "#999999"))+
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+
  scale_y_log10()+
  labs(x="ERP", y="Frequency")

proba_plot <- ggplot(res78, aes(x=cnn_values, color=cnn_result, fill = cnn_result)) +
  geom_histogram(position="dodge")+
  scale_color_manual(values=c("red", '#E69F00'))+
  scale_fill_manual(values=c("red", '#E69F00'))+
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+
  scale_y_log10()+
  labs(x="MP", y="Frequency")

proba_erp_histo_evolution_plot <- both_p_num / (erp_plot + proba_plot) + 
  plot_layout(heights = c(10,4))+ 
  plot_annotation(tag_levels = 'A')
save.name <- 'proba_erp_histo_evolution_plot_2yaxis_2.png'
ggsave(file.path(save.dir, save.name), width = 18.5, height = 12.5, dpi = 150, units = "in", device='png')

#with custom cutoff stuff
erp_vec <- seq(0.0,1.0, 0.01)
erp_cutoff <- findERPCutOff(df.crop, erp_vec)

proba_vec <- seq(round(min(df.crop$cnn_values), 2),1.0, 0.01)
proba_cutoff <- findProbaCutOff(df.crop, proba_vec)

entr_vec <- seq(round(min(df.crop$cnn_values_DescToolsentropy),2),round(max(df.crop$cnn_values_DescToolsentropy),2), 0.01)
entr_cutoff <- findEntropyCutOff(df.crop, entr_vec)


# Arranging the plot
save.name <- paste0('entropyAndProbaWMarginals_', runn, '.png')
ggarrange(xplot, NULL, cplot, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = F)
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

# Arranging the plot
save.name <- paste0('entropyAndProbaWMarginals_', runn, '.png')
ggarrange(xplot, NULL, cplot, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)
ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

((xplot / cplot + plot_layout(guides = 'auto')) | yplot) + plot_layout(guides = 'auto')


plots_2 <- cplot_wproba + yplot +
  plot_layout(ncol = 2, 
              widths = c(15, 3.5),
              guides = "collect")

xplot / plots_2 + plot_layout(nrow = 2, 
                              byrow = TRUE,
                              heights = c(3.5, 15),
                              widths = c(1, 20),
                              guides = 'collect')
# Arranging the plot


#break the space up into quadrants and compare the performance from keeping all the images from a specific quandrant or combo thereof
### WITH PROBA
quadrant_df <- data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(quadrant_df) <- c('Quadrant', 'True', 'False', 'TF Ratio')
for(i in 1:4){
  
  #quadrant 1 - bottom left corner
  if(i == 1){
    df.crop_q <- df.crop[df.crop$cnn_values <= proba_cutoff & df.crop$erp <= erp_cutoff,]
  }
  
  #quadrant 2 - bottom right corner
  if(i == 2){
    df.crop_q <- df.crop[df.crop$cnn_values < proba_cutoff & df.crop$erp > erp_cutoff,]
  }
  
  #quadrant 3 - top left corner
  if(i == 3){
    df.crop_q <- df.crop[df.crop$cnn_values > proba_cutoff & df.crop$erp < erp_cutoff,]
  }
  
  #quadrant 4 - top right corner
  if(i == 4){
    df.crop_q <- df.crop[df.crop$cnn_values > proba_cutoff & df.crop$erp > erp_cutoff,]
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

# WITH PROBA
#### test the performance for either of the following configurations:
##### - 1. ONLY probability filter - Q3 + Q4
##### - 2. ONLY ERP filter - Q4 + Q2
##### - 3. Keep accepted by BOTH filters (restrictive) - Q4
##### - 4. Remove ONLY access of both (inclusive) - Q2 + Q3 + Q4

quadrantFilter_dr <- data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(quadrantFilter_dr) <- c('QM idx', 'Quadrants', 'Number of images', 'M-F1')
for(i in 1:4){
  
  if(i == 1){
    df.crop_f <- df.crop[df.crop$cnn_values > proba_cutoff,]
    qts <- 'Q3, Q4' 
  }
  
  if(i == 2){
    df.crop_f <- df.crop[df.crop$erp > erp_cutoff,]
    qts <- 'Q4, Q2' 
  }
  
  if(i == 3){
    df.crop_f <- df.crop[df.crop$erp > erp_cutoff & df.crop$cnn_values > proba_cutoff,]
    qts <- 'Q4' 
  }
  
  if(i == 4){
    df.crop_Q1 <- df.crop[df.crop$cnn_values < proba_cutoff & df.crop$erp < erp_cutoff,]
    df.crop_f <- df.crop[! df.crop$pointidyear %in% df.crop_Q2$pointidyear,]
    qts <- 'Q2, Q3, Q4' 
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

#WITH MARGINAL PLOTS
runn <- 78
quadrant_df$`TF Ratio` <- NULL
colnames(quadrant_df) <- c('Q', 'True', "False")
cplot_save.name <- paste0('erpAndProbaQuadrantsTable_cplot_', runn, '.png')
cplot <- ggplot(df.crop, aes(erp, cnn_values)) + 
  geom_point(aes(shape = cnn_result, color = cnn_result))+
  geom_smooth(data =df.crop[df.crop$cnn_result == F,], color = 'red')+
  geom_smooth(data =df.crop[df.crop$cnn_result == T,], color = 'blue')+ 
  geom_rug(aes(color = cnn_result)) +
  theme_minimal()+
  theme(legend.position =  'none')+
  geom_vline(xintercept = erp_cutoff, linetype = "dashed")+
  geom_hline(yintercept = proba_cutoff, linetype = "dashed")+
  annotate("text", x=0.1, y=0.25, label= "1", size=15) + 
  annotate("text", x=0.9, y=0.25, label = "2", size=15)+
  annotate("text", x=0.1, y=0.9, label= "3", size=15) + 
  annotate("text", x=0.9, y=0.9, label = "4", size=15)+
  annotation_custom(tableGrob(quadrant_df, rows = NULL), xmin=0.5, xmax=1, ymin=0.6, ymax=0.6)+
  labs(x="ERP", y="Maxmimum probability")
ggsave(file.path(save.dir, cplot_save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

# Marginal density plot of x (top panel)
yplot_save.name <- paste0('erpAndProbaQuadrantsTable_yplot_', runn, '.png')
yplot <- ggplot(df.crop, aes(cnn_values, fill=cnn_result)) + 
  geom_density(alpha=.5) + 
  #scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")+
  theme_minimal()+
  coord_flip()+
  labs(x="Maximum Probability", y="density")+
  theme(legend.position =  'none')
ggsave(file.path(save.dir, yplot_save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')

# Marginal density plot of y (right panel)
xplot_save.name <- paste0('erpAndProbaQuadrantsTable_xplot_', runn, '.png')
xplot <- ggplot(df.crop, aes(erp, fill=cnn_result)) + 
  geom_density(alpha=.5) + 
  #scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")+
  theme_minimal()+
  labs(x="ERP", y="density")+
  theme(legend.position =  'none')
ggsave(file.path(save.dir, xplot_save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')


#arrange with patchwork
design <- c(
  area(1, 1, 1, 3),
  area(2, 1, 3, 3),
  area(2, 4, 3, 4)
)

comboQuandrant_marginal_plot <- xplot + cplot + yplot+ plot_layout(design = design)
xplot_save.name <- 'comboQuandrant_marginal_plot.png'
ggsave(file.path(save.dir, xplot_save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')













#dummy example
vec_proba <- c(0.6, 0.2, 0.2)
k <- length(vec_proba)+1
p_i_star <- vec_proba[1]
edi_input_proba <- vec_proba[2:length(vec_proba)]


edi <- log(p_i_star) - (1/(1-p_i_star)) * sum(edi_input_proba * log(edi_input_proba))
lpi <- log(p_i_star) - log(1-p_i_star)
upi <- log(p_i_star) - log((1 - p_i_star) / (k - 1))

erp <- exp(edi) / (exp(edi) + (k - 1))
