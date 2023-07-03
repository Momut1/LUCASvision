rm(list=ls())

library(rgdal)
library(ggplot2)
library(magick)
library(xtable)
library(rgdal)
library(rgeos)

diskDir <- '/eos/jeodpp/data/projects/REFOCUS/data'
lucasV_trainSet <- read.csv(file.path(diskDir, 'LUCAS_C_vision/lucasV_traininSet_manutreNoHarvest_400.csv'), stringsAsFactors = F)
head(lucasV_trainSet)
table(lucasV_trainSet$lc1)
table(lucasV_trainSet$lc1, lucasV_trainSet$nuts0)

lucasV_fullSet <- read.csv(file.path(diskDir, 'LUCAS_C_vision/lucasvision_mature12.csv'), stringsAsFactors = F)
nrow(lucasV_fullSet)

### select 100 images per class for the inference set with the same stratification as the train set
tab_country<-table(lucasV_trainSet$nuts0,lucasV_trainSet$lc1)
tab_country.sum<- cbind(tab_country,'Total # records'=margin.table(tab_country,margin=1))
tab_country.sums.1<- rbind(tab_country.sum,'Total # records'=margin.table(tab_country.sum,margin=2))
tab_country.sum.1<-rbind(tab_country.sum,'Total # records'=margin.table(tab_country.sum,margin=2))

#what proportion of the train set comes from each country
tab_country.sum_df <- as.data.frame(tab_country.sum)
tab_country.sum_df$nuts0 <- rownames(tab_country.sum_df)

tab_country.sum_df_per <- data.frame(matrix(NA, nrow = nrow(tab_country.sum_df), ncol = ncol(tab_country.sum_df)))
colnames(tab_country.sum_df_per) <- colnames(tab_country.sum_df)
for(i in 1:nrow(tab_country.sum_df)){
  for(j in 1:(ncol(tab_country.sum_df)-2)){
    tab_country.sum_df_per[i,j] <- round((tab_country.sum_df[i,j] / 400) * 100, 3)
  }
}

#check
colSums(tab_country.sum_df_per) == 100

tab_country.sum_df_per$nuts0 <- tab_country.sum_df$nuts0
tab_country.sum_df_per <- tab_country.sum_df_per[,-c(13)]

write.csv(tab_country.sum_df_per, '/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/lucasV_trainSet_tab_country_sum_df.csv', row.names = F)


ggplot(lucasV_trainSet)+
  geom_point(aes(x=th_long,y=th_lat,colour=lc1_label))+xlab('long')+ylab('lat')
theme_minimal()

lucasV_testSet <- read.csv(file.path(diskDir , "/LUCAS_C_vision/v2/inputs/lucasV_testSet_cap85.csv"), stringsAsFactors = F)

# Table by country - in paper
lucasV_trainOK <- read.csv(file.path(diskDir, 'LUCAS_C_vision/lucasvision_trainok_aug.csv'), stringsAsFactors = F)
lucasV_trainOK <- lucasV_trainOK[lucasV_trainOK$lc1 != 'B17',]

lucasV_M12 <- read.csv(file.path(diskDir, 'LUCAS_C_vision/lucasvision_mature12.csv'), stringsAsFactors = F)
lucasV_M12 <- lucasV_M12[lucasV_M12$lc1 != 'B17',]

tab_country<-table(lucasV_trainOK$nuts0,lucasV_trainOK$lc1)
tab_country.sum<- cbind(tab_country,'Total'=margin.table(tab_country,margin=1))
tab_country.sums.1<- rbind(tab_country.sum,'Total'=margin.table(tab_country.sum,margin=2))
tab_country.sum.1<-rbind(tab_country.sum,'Total'=margin.table(tab_country.sum,margin=2))
#tab_country<-rbind(colnames(tab_country.sum.1),)

#now add the entire set of mature crops
tab_country.sum.1 <- cbind(tab_country.sum.1, 'Total MMEC'=t(t(c(t(t(table(lucasV_M12$nuts0))), 0))))
tab_country.sum.1 <- rbind(tab_country.sum.1, 'Total MMEC'=c(table(lucasV_M12$lc1), 0, nrow(lucasV_M12)))

#convert to integer
mode(tab_country.sum.1) <- 'integer'

xtable(tab_country.sum.1)

#geographical distro of the set - fig in paper
#project in wgs84
coords <- lucasV_trainOK[,c(10,9)]
sp_lv_toplot <- SpatialPointsDataFrame(coords = coords, data = lucasV_trainOK, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))

#reproject in 3035
sp_lv_toplot_3035 <- spTransform(sp_lv_toplot, CRSobj = CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
sp_lv_toplot_3035$th_lat_3035 <- sp_lv_toplot_3035@coords[,1]
sp_lv_toplot_3035$th_long_3035 <- sp_lv_toplot_3035@coords[,2]
#sp_lv_toplot_3035_tst <- sp_lv_toplot_3035[sp_lv_toplot_3035$revisit == 3,]

#lucas_legend = read.csv(file.path(dir.dropbox,'/JRC/LANDSENSE/CASE-STUDY-8-LUCAS-COPERNICUS-CLASSIF/table/legend-lucas3.csv'),sep=";")
lucas_legend = read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/scripts/legend-lucas (2).csv', stringsAsFactors = F)
lucas_legend<-lucas_legend[lucas_legend$originalClass %in% unique(lucasV_trainOK$lc1),]
lucas_legend<-lucas_legend[order(lucas_legend$originalClass),]

#plot and save
#with polygons
library(giscoR)

nuts0 <- gisco_get_nuts(year = '2016', epsg = '3035', nuts_level = '0')

p <- ggplot() +
  geom_point(data = sp_lv_toplot_3035@data, aes(x = th_lat_3035, y = th_long_3035,col=lc1),size=1)+
  scale_color_manual('Crops',breaks=lucas_legend$originalClass,
                     values=as.character(lucas_legend$colorRGBcdl),
                     labels = as.character(lucas_legend$label))+
  theme(legend.text=element_text(size=40))+
  guides(colour = guide_legend(override.aes = list(size=20)))+
  coord_quickmap()+theme_minimal() + theme(text = element_text(size=20))+theme(legend.title=element_blank())+theme(legend.position="top")+
  xlab("") + ylab("")+ theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  coord_cartesian()+
  geom_sf(data = nuts0, fill = NA,colour='black',lwd = 0.2)+
  coord_sf(
    crs = 3035, xlim = c(2377294, 6400000),
    ylim = c(1313597, 5628510)
  ) +# theme_bw()+
  # theme(text = element_text(size = 7))+
  labs(x = "")+ labs(y = "")

file_name <- 'lucasV_trainok_geog_3035_nuts.png'
plot_dir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/figs4paper/'
ggsave(filename=file_name,plot = p,
       scale = 1, width = 120*3, height = 100*5 ,units = "mm",
       dpi = 300,path=plot_dir)

#### FIG WITH ONE IMG PER CROP
lucasV_testSet_1class <- data.frame()
for(llc1 in sort(unique(lucasV_testSet$lc1))){
  lucasV_testSet_llc1 <- lucasV_testSet[lucasV_testSet$lc1 == llc1,]
  lucasV_testSet_llc1_1 <- lucasV_testSet_llc1[sample(nrow(lucasV_testSet_llc1), 1),]
  
  lucasV_testSet_1class <- rbind(lucasV_testSet_1class, lucasV_testSet_llc1_1)
}

nrow(lucasV_testSet_1class)
table(lucasV_testSet_1class$lc1)

b11 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B11']), '320x240'), "#FFFFFF", "20x10")
b12 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B12']), '320x240'), "#FFFFFF", "20x10")
b13 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B13']), '320x240'), "#FFFFFF", "20x10")
b14 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B14']), '320x240'), "#FFFFFF", "20x10")
b15 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B15']), '320x240'), "#FFFFFF", "20x10")
b16 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B16']), '320x240'), "#FFFFFF", "20x10")
b21 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B21']), '320x240'), "#FFFFFF", "20x10")
b22 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B22']), '320x240'), "#FFFFFF", "20x10")
b31 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B31']), '320x240'), "#FFFFFF", "20x10")
b32 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B32']), '320x240'), "#FFFFFF", "20x10")
b33 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B33']), '320x240'), "#FFFFFF", "20x10")
b55 <- image_border(image_scale(image_read(lucasV_testSet_1class$filepatheos[lucasV_testSet_1class$lc1 == 'B55']), '320x240'), "#FFFFFF", "20x10")

#3x4 images
imgRow_i_1 <- c(b11, b12, b13, b14)
imgRow_i_2 <- c(b15, b16, b21, b22)
imgRow_i_3 <- c(b31, b32, b33, b55)

#append them 
imgRow_i_1_app <- image_border(image_append(imgRow_i_1), "#FFFFFF", "50x30")
imgRow_i_2_app <- image_border(image_append(imgRow_i_2), "#FFFFFF", "50x30")
imgRow_i_3_app <- image_border(image_append(imgRow_i_3), "#FFFFFF", "50x30")

#stack them
img_Row_all <- c(imgRow_i_1_app, imgRow_i_2_app, imgRow_i_3_app)
img_Row_all_app <- image_append(img_Row_all, stack = T)


image_write(img_Row_all_app, file.path(diskDir, 'LUCAS_C_vision/figs4paper/1imgperclass_3x4.png'))


#table of best model results
results85 <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results85.csv', stringsAsFactors = F)
head(results85)

#order desc
results85_o <- results85[order(results85$test_acc, decreasing = T),]

#take top3
results85_o_top3 <- results85_o[c(1,2,3),]
results85_o_top3$MF1 <- results85_o_top3$test_acc
results85_o_top3$X <- NULL

colnames(results85_o_top3) <- c('Model', 'Level', 'LR', 'BS', 'Momentum', 'Optimizer', 'Validation Accuracy', 'Training Accuracy', 'Test Accuracy', 'M-F1')

#change to scientific notation relevant cols
results85_o_top3$Optimizer <- "GD"
results85_o_top3$`Number of Images` <- 1020

#round cols
results85_o_top3$LR <- round(results85_o_top3$LR, 4)
results85_o_top3$`Validation Accuracy` <- round(results85_o_top3$`Validation Accuracy`, 4)
results85_o_top3$`Training Accuracy` <- round(results85_o_top3$`Training Accuracy`, 4)
results85_o_top3$`Test Accuracy` <- round(results85_o_top3$`Test Accuracy`, 4)
results85_o_top3$`M-F1` <- round(results85_o_top3$`M-F1`, 4)

BestModelRow <- c('78', 'Best Model', '0.0035', '1024', '0', 'GD', '0.7768', '0.8945', '0.7854', '0.7572', '8642')

results85_o_top3_best <- rbind(results85_o_top3, BestModelRow)

xtable(t(results85_o_top3_best))


#### boxplot of badImages
results_badImgs <- read.csv(file.path(diskDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-str1-bad/78/cnn_output_data_check_corr.csv'), stringsAsFactors = F)
#results_badImgs <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-str1-bad/78/cnn_output_data_check_corr.csv', stringsAsFactors = F)
head(results_badImgs)
table(results_badImgs$condition)
table(is.na(results_badImgs$condition))
results_badImgs[is.na(results_badImgs$condition),]
results_badImgs$result <- ifelse(results_badImgs$cnn_labels == results_badImgs$lc1, T, F)
table(results_badImgs$result, results_badImgs$condition )

#remove NAs and format
results_badImgs <- results_badImgs[! is.na(results_badImgs$condition),]
results_badImgs$X <- NULL
results_badImgs$files <- NULL
results_badImgs$year <- NULL

#include a sample of 59 from the normal inference set
table(results_badImgs$lc1[results_badImgs$condition == 'Blurry'])

testSet85_78 <- read.csv(file.path(diskDir,'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results85/78/cnn_output_data_check.csv'), stringsAsFactors = F)

testSet85_78_s59 <- data.frame()
for(llc1 in sort(unique(testSet85_78$lc1))){
  testSet85_78_llc1 <- testSet85_78[testSet85_78$lc1 == llc1,]
  
  if(llc1 != 'B55'){
    testSet85_78_llc1_s <- testSet85_78_llc1[sample(nrow(testSet85_78_llc1), 5),]
  }else{
    testSet85_78_llc1_s <- testSet85_78_llc1[sample(nrow(testSet85_78_llc1), 4),]
  }
  
  testSet85_78_s59 <- rbind(testSet85_78_s59, testSet85_78_llc1_s)
  
}

table(testSet85_78_s59$lc1)
testSet85_78_s59$condition <- 'Reference'
testSet85_78_s59$cnn_labels <- toupper(testSet85_78_s59$cnn_labels)
testSet85_78_s59$result <- ifelse(testSet85_78_s59$cnn_labels == testSet85_78_s59$lc1, T, F)
colnames(testSet85_78_s59) <- c('cnn_labels_RAW', 'cnn_values_RAW', "pointidyear", "lc1","cnn_labels","cnn_values","condition","result")
head(testSet85_78_s59)

#rbind them
allImgs <- rbind(testSet85_78_s59, results_badImgs)

#now put the complete imbalanced set in there
testSet_78 <- read.csv(file.path(diskDir,'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-all-images/78/cnn_output_data_check.csv'), stringsAsFactors = F)
colnames(testSet_78)
testSet_78$condition <- 'Reference-all'
testSet_78$result <- ifelse(testSet_78$lc1 == testSet_78$cnn_labels, T, F)
colnames(testSet_78) <- c('cnn_labels_RAW', 'cnn_values_RAW', "pointidyear","lc1","cnn_labels","cnn_values","condition","result")

allImgs2 <- rbind(testSet_78, allImgs)
table(allImgs2$condition)

allImgs2$nroww <- NULL
for(cond in sort(unique(allImgs2$condition))){
  
  if(cond != 'Reference-all'){
    allImgs2$nroww[allImgs2$condition == cond] <- 59
  }else{
    allImgs2$nroww[allImgs2$condition == cond] <- nrow(testSet_78)
  }
  
}

save.dir <- file.path(diskDir, 'LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-str1-bad/78/')
save.name <- 'lucasV_boxplot_badImg.png'
ggplot(allImgs2, aes(x=condition, y=cnn_values)) +
  geom_boxplot(position=position_dodge(1))+
  geom_vline(xintercept = 6.5, linetype = 'dashed')+
  annotate("text", label = "59", x = 1, y = 0.2, size = 6) +
  annotate("text", label = "59", x = 2, y = 0.2, size = 6) +
  annotate("text", label = "59", x = 3, y = 0.2, size = 6) +
  annotate("text", label = "59", x = 4, y = 0.2, size = 6) +
  annotate("text", label = "59", x = 5, y = 0.2, size = 6) +
  annotate("text", label = "59", x = 6, y = 0.2, size = 6) +
  annotate("text", label = "59", x = 7, y = 0.2, size = 6) +
  annotate("text", label = as.character(nrow(testSet_78)), x = 8, y = 0.2, size = 6) +
  labs(x="Condition", y="Top 1 Probability")+
  theme_minimal()+ 
  theme(axis.text.x = element_text(face="bold", size=18),
        axis.text.y = element_text(face="bold",size=18))+ 
  theme(axis.title = element_text(size = 20))
ggsave(file.path(save.dir, save.name), width = 13, height = 8, dpi = 150, units = "in", device='png')

#second attempt
ggplot(allImgs2, aes(factor(condition), cnn_values)) +
  geom_boxplot(aes(colour = result), varwidth = TRUE)

#table of metrics
results_badImgs_dt <- as.data.frame(t(as.data.frame.matrix(table(results_badImgs$result, results_badImgs$condition ))))
results_badImgs_dt$OA <- round(results_badImgs_dt[,2] / (results_badImgs_dt[,1] + results_badImgs_dt[,2]), 2)

xtable(results_badImgs_dt)

### ua - pa diff between cereals and other
otherPA = c(95.5, 87.5, 91.4, 94.4, 94.6, 85.3)
otherUA = c(95, 87.8, 91.2, 83, 94, 86)

cerealPA = c(61.2, 67.1, 55.7, 57.3, 76.3)
cerealUA = c(78.2, 10.8, 72.6, 57.3, 74.3)

mean(otherPA) - mean(cerealPA)
mean(otherUA) - mean(cerealUA)












