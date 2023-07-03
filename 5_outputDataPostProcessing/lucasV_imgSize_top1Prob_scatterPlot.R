rm(list=ls())

library(jpeg)
library(ggplot2)
library(BAMMtools)

dataDir <- '/eos/jeodpp/data/projects/REFOCUS/data/'
df_output78 <- read.csv(file.path(dataDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-all-images/78/cnn_output_data_check.csv'), stringsAsFactors = F)
head(df_output78)
df_output78$cnn_result <- ifelse(df_output78$lc1 == df_output78$cnn_labels, T, F)
table(df_output78$cnn_result)

#add the imageSize 
lc_exif <- read.csv('/eos/jeodpp/data/projects/REFOCUS/LUCAS_COVER_W/tables/lucas_cover_exif.csv', stringsAsFactors = F)
head(lc_exif)

#should be T
table(df_output78$pointidyear %in% lc_exif$pointidyear)

#subset the exif df
lc_exif_78 <- lc_exif[lc_exif$pointidyear %in% df_output78$pointidyear,]

table(is.na(lc_exif_78$ExifImageWidth))

#extract the image dimensions where they're missing
for(i in 1:nrow(lc_exif_78)){
  if(i %% 100 == 0){
    print(i)
  }
  localpath <- file.path('/eos/jeodpp/data/projects/REFOCUS/LUCAS_COVER_W/',paste0(strsplit(lc_exif_78$file_path_ftp_cover[i], '/')[[1]][8:12], collapse = '/'))
  
  img <- readJPEG(localpath) 
  
  dim_img <- dim(img)
  
  lc_exif_78$ExtractedHeight[i] <- dim_img[1]
  lc_exif_78$ExtractedLength[i] <- dim_img[2]
  lc_exif_78$ExtractedChannels[i] <- dim_img[3]
}

table(is.na(lc_exif_78$ExtractedLength))
table(is.na(lc_exif_78$ExtractedHeight))

lc_exif_78$ExtractedNumberOfPixels <- lc_exif_78$ExtractedHeight * lc_exif_78$ExtractedLength
lc_exif_78_m <- merge(lc_exif_78, df_output78, by = 'pointidyear')


ggplot(lc_exif_78_m, aes(x=ExtractedNumberOfPixels)) +
  geom_histogram(fill="black", bins = 40)+
  theme_minimal()


max(lc_exif_78_m$ExtractedHeight) / min(lc_exif_78_m$ExtractedHeight)

max(lc_exif_78_m$ExtractedLength) / min(lc_exif_78_m$ExtractedLength)

lc_exif_78_m[order(lc_exif_78_m$ExtractedNumberOfPixels, decreasing = T),]

write.csv(lc_exif_78_m, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/lc_exif_78_m.csv')

# scatter plot
ggplot(lc_exif_78_m, aes(x=ExtractedNumberOfPixels, y=cnn_values, shape=cnn_result, color=cnn_result)) +
  geom_point()+
  geom_rug()+
  theme_minimal()+
  labs(x ="Numer of pixels", y = "Top1 probability")+ 
  theme(legend.position = c('none'))

#boxplot
natBreaks <- getJenksBreaks(lc_exif_78_m$ExtractedNumberOfPixels, 10)

for(i in 1:nrow(lc_exif_78_m)){
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[1]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[1]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[1] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[2]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[2]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[2] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[3]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[3]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[3] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[4]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[4]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[4] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[5]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[5]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[5] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[6]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[6]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[6] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[7]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[7]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[7] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[8]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[8]
  }
  
  if(lc_exif_78_m$ExtractedNumberOfPixels[i] > natBreaks[8] & lc_exif_78_m$ExtractedNumberOfPixels[i] <= natBreaks[9]){
    lc_exif_78_m$ExtractedNumberOfPixels_Breaks[i] <- natBreaks[9]
  }
}

ggplot(lc_exif_78_m, aes(factor(ExtractedNumberOfPixels_Breaks), cnn_values)) +
  geom_boxplot(aes(colour = cnn_result), varwidth = TRUE)
