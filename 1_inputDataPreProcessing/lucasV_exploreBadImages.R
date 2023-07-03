library(splitstackshape)
library(magick)
library(caret)
library(reshape2)
library(gridExtra)

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

#explore results of bad images
diskDir <- '/eos/jeodpp/data/projects/REFOCUS/data'
resultsDir <- file.path(diskDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-str1-bad/78')
runn <- '78'

savedir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-str1-bad/78/'
save.dir <- savedir

#the og data - for the pointidyear
df_bad_og <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-str1-bad_og/78/cnn_output_data_check.csv', stringsAsFactors = F)
length(unique(df_bad_og$pointidyear)) == nrow(df_bad_og)

#the results data
df_bad <- read.csv(file.path(resultsDir, 'cnn_output_data_check.csv'), stringsAsFactors = F)
head(df_bad)
df_bad$cnn_labels <- toupper(df_bad$cnn_labels)

#merge
df_bad_m <- merge(df_bad_og, df_bad, by='pointidyear')
head(df_bad_m)

#extract pointyear
for(i in 1:nrow(df_bad_m)){
  df_bad_m$year[i] <- gsub('LUCAS','',strsplit(strsplit(df_bad_m$files[i], '/')[[1]][13], '_')[[1]][1])
  df_bad_m$pointidyear[i] <- paste0(strsplit(df_bad_m$pointidyear[i], 'v')[[1]][1], df_bad_m$year[i])
  df_bad_m$lc1[i] <- strsplit(df_bad_m$files[i], '/')[[1]][12]
  df_bad_m$condition[i] <- gsub(".jpg']",'',strsplit(strsplit(df_bad_m$files[i], '/')[[1]][13], '_')[[1]][4])
  df_bad_m$files[i] <- gsub("\\']","",gsub("\\['", "", df_bad_m$files[i]))
}

head(df_bad_m)

#keep onlythe cols you need and rename them because of mergex xy
df_bad <- df_bad_m[,c(1,2,3,4,11,12,13,14,15)]
colnames(df_bad) <- c('pointidyear', 'cnn_labels_RAW', 'cnn_values_RAW', 'files', 'cnn_labels', 'cnn_values', 'year', 'lc1', 'condition')
head(df_bad)

write.csv(df_bad, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-str1-bad/78/cnn_output_data_check_corr.csv')

####### explore the results
#do a figure to whow the kind of data
df_bad_b11 <- df_bad[df_bad$lc1 == 'B11',]

#take one image per condition
df_bad_b11_str1 <- stratified(df_bad_b11, 'condition', 1)

#order  with custom order
customOrder = c("Early", "PHarvest","Close", 'Landscape', 'Blurry', 'Object')
df_bad_b11_str1_o <- df_bad_b11_str1[ order(match(df_bad_b11_str1$condition, customOrder)), ]

#plot them for vis purposes
p_Early <- image_border(image_scale(image_read(df_bad_b11_str1_o$files[1]), '320x240'), "#FFFFFF", "20x10") #Early
#image_annotate(p_Early, "Early", size = 10, color = "black", boxcolor = "white", degrees=0, location = "+50+100")
p_PHarvest <- image_border(image_scale(image_read(df_bad_b11_str1_o$files[2]), '320x240'), "#FFFFFF", "20x10") #PHarvest
p_Close <- image_border(image_scale(image_read(df_bad_b11_str1_o$files[3]), '320x240'), "#FFFFFF", "20x10") #Close
p_Landscape <- image_border(image_scale(image_read(df_bad_b11_str1_o$files[4]), '320x240'), "#FFFFFF", "20x10") #Landscape
p_Blurry <- image_border(image_scale(image_read(df_bad_b11_str1_o$files[5]), '320x240'), "#FFFFFF", "20x10") #Blurry
p_Object <- image_border(image_scale(image_read(df_bad_b11_str1_o$files[6]), '320x240'), "#FFFFFF", "20x10") #Object

#append to row
imgRow_i <- c(p_Early, p_PHarvest, p_Close, p_Landscape, p_Blurry, p_Object)

imgRow_i_app <- image_append(imgRow_i)

image_write(imgRow_i_app, file.path(savedir, 'badImg_row.jpg'))


#some stats on bad images
badImgdf_all <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/lucasV_badImg_df.csv', stringsAsFactors = F)
t_cond_lc1 <- table(badImgdf_all$condition, badImgdf_all$lc1)
cm.with.sum<- cbind(t_cond_lc1,TOTAL=margin.table(t_cond_lc1,margin=1))
cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))

table(df_bad$condition, df_bad$lc1)

#overview result
df_bad$result <- ifelse(df_bad$cnn_labels == df_bad$lc1, T, F)
table(df_bad$result, df_bad$condition)
table(df_bad$condition, df_bad$lc1,df_bad$result)

#confMatrices
for(conditionn in customOrder){
  df_bad_c <- df_bad[df_bad$condition == conditionn,]
  df_bad_c <- df_bad_c[!is.na(df_bad_c$lc1),]
  
  colnames(df_bad_c) <- c("pointidyear","cnn_labels_RAW", "cnn_values_RAW","files","code_max","cnn_values","year","code","condition","result"  ) 
  
  #preprocess for confusion Matrix
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df_bad_c)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df_bad_c, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df_bad_c, table.crop.byClass.1)
  
  #melt and order the confusion matrix
  df.crop.melt <- melt_and_order_df(cm.crop.with.sums.1)
  
  #collect rows and columns to exclude from coloring scheme and leave in neutral gray
  df.crop.sums <- df.crop.melt[df.crop.melt$Var2 %in% c('TOTAL', 'PA', 'UA') | df.crop.melt$Var1 %in% c('TOTAL', 'PA', 'UA') ,]
  
  #plot and save CM
  plot_confusionMatrix_CROP(df.crop.melt, df.crop.sums, cm.crop.index.overal.table, paste0(conditionn, '_cm_crop.png'))
}

#do the image magick, so they on the same row - todo!
confMatrices_files <- file.path(save.dir, list.files(save.dir)[grep('cm_', list.files(save.dir))])


##mistake plot
missclass_csv_retrainLogs <- read.csv(file.path(diskDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/retrain_logs', paste0(runn, 'missclass.csv')), stringsAsFactors = F)
for(i in 1:nrow(missclass_csv_retrainLogs)){
  missclass_csv_retrainLogs$filename[i] <- strsplit(missclass_csv_retrainLogs$image[i], '/')[[1]][5]
}
head(missclass_csv_retrainLogs)

missclass_csv_sumDir <- read.csv(file.path(diskDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/summaries_dir', paste0(runn, 'missclass.csv')), stringsAsFactors = F)
for(i in 1:nrow(missclass_csv_sumDir)){
  missclass_csv_sumDir$filename[i] <- strsplit(missclass_csv_sumDir$image[i], '/')[[1]][13]
}
head(missclass_csv_sumDir)

nrow(missclass_csv_retrainLogs)
nrow(missclass_csv_sumDir)

missclass_csv_retrainLogs$filename %in% missclass_csv_sumDir$filename
