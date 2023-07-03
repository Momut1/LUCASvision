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
  #print(head(df.crop))
  
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

df <- df.crop_j
confusionMatrix_calculate_PA_UA_F1_CROP <- function(df){
  u = sort(union(df$code,df$code_max))
  cm = table(factor(df$code, u), factor(df$code_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum <- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1 <- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  #print(cm.with.sums.1)
  
  # UA, PA; FSCORE ####
  cm.index <- confusionMatrix(cm,mode='prec_recall')
  cm.ByClass <- round(data.frame(cm.index$byClass),digit=4)
  
  if( ! 'F1' %in% colnames(cm.ByClass)){
    cm.ByClass <- as.data.frame(t(cm.ByClass))
  }
  
  if(! is.na(cm.ByClass$F1)){
    table.byClass.1 <- data.frame(UA=cm.ByClass$Recall,PA=cm.ByClass$Precision,F_score=cm.ByClass$F1)
    #rownames(table.byClass.1) <- rownames(cm)
    
  }else{
    
  }
  
  return(table.byClass.1)
}

diskDir <- '/eos/jeodpp/data/projects/REFOCUS/data'
ccFromExpMod <- read.csv(file.path(diskDir, 'LUCAS_C_vision/cropCalendarExpertAndModelInput.csv'), stringsAsFactors = F)

allImg_78 <- read.csv(file.path(diskDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-all-images/78/cnn_output_data_check.csv'), stringsAsFactors = F)
head(allImg_78)
allImg_78$cnn_result <- ifelse(allImg_78$cnn_labels == allImg_78$lc1, T, F)
table(allImg_78$cnn_result)
length(unique(allImg_78$pointidyear))

#join with the lucas_cover_attr to get the nuts0
gt_LUCAS <- read.csv('/eos/jeodpp/data/projects/REFOCUS/LucasHarmo/fromFTP/lucas_harmo_uf.csv', stringsAsFactors = F)
gt_LUCAS$pointidyear <- paste0(gt_LUCAS$point_id, gt_LUCAS$year)

table(allImg_78$pointidyear %in% gt_LUCAS$pointidyear)

allImg_78_attr <- merge(allImg_78, gt_LUCAS, by = 'pointidyear')



#load in cnn output
i <- 78
df_lucas <- gt_LUCAS
colnames(df_lucas)[grep('pointidyear',colnames(df_lucas)) ] <- 'basename'
df.crop <- load_and_preprocess(file.path(diskDir,'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-all-images/'), i, F, F, F, F, F)

#preprocess for confusion Matrix
ref_table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop)
ref_table.crop.byClass.1$lc1 <- rownames(ref_table.crop.byClass.1)

for(j in 1:nrow(ccFromExpMod)){
  print(j)
  df.crop_j <- df.crop[df.crop$lc1 == ccFromExpMod$lc1[j] & df.crop$nuts0 == ccFromExpMod$nuts0[j],]
  
  if(nrow(df.crop_j) > 1){
    
    if(all(df.crop_j$code_max == df.crop_j$code)){
      ccFromExpMod$f1[j] <- 1.0
    }else{
      
      table.crop.byClass.1_j <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop_j)
      table.crop.byClass.1_j <- table.crop.byClass.1_j[! is.na(table.crop.byClass.1_j$F_score),]
      #print(table.crop.byClass.1_j)
      
      if(nrow(table.crop.byClass.1_j) != 0){
        ccFromExpMod$f1[j] <- table.crop.byClass.1_j$F_score
      }else{
        ccFromExpMod$f1[j] <- NA
      }
      
    }
  }else{
    print('only 1 examples')
    ccFromExpMod$f1[j] <- NA
  }

}

for(jj in 1:nrow(ccFromExpMod)){
  lc1_jj <- ccFromExpMod$lc1[jj]
  f1_jj  <- ccFromExpMod$f1[jj]
  
  f1_ref <- ref_table.crop.byClass.1$F_score[ref_table.crop.byClass.1$lc1 == lc1_jj]
  
  ccFromExpMod$f1LowerThanRef <- ifelse(f1_jj < f1_ref, T, F)
}

write.csv(ccFromExpMod, file.path(diskDir, 'LUCAS_C_vision/cropCalendarExpertAndModelInput_f1_tf.csv'))
