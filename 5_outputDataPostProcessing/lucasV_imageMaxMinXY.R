##### rapha's question anout MMEC
mok <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/lucasvision_trainok_aug.csv', stringsAsFactors = F)
table(mok$lc1)

ftpDir <- '/mnt/cidstorage/cidportal/data/OpenData/LUCAS/LUCAS_COVER/'

for(i in 1:nrow(mok)){
  mok$filepath_ftp[i] <- file.path(ftpDir,
                                   paste0('LUCAS', mok$year[i]),
                                   mok$nuts0[i],
                                   substr(mok$point_id[i], 1,3),
                                   substr(mok$point_id[i], 4,6),
                                   paste0('LUCAS', mok$year[i], '_', mok$point_id[i], '_Cover.jpg'))
}

table(file.exists(mok$filepath_ftp))
table(mok$lc1)
mok <- mok[mok$lc1 != 'B17',]

mmecDir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/'
for(i in 1:nrow(mok)){
  sourcefile <- mok$filepath_ftp[i]
  destfile <- file.path(mmecDir, mok$lc1[i], strsplit(mok$filepath_ftp[i], '/')[[1]][14])
  
  if(!dir.exists(file.path(mmecDir, mok$lc1[i]))){
    dir.create(file.path(mmecDir, mok$lc1[i]))
  }
  
  file.copy(sourcefile, destfile)
}

#check
mmec_files <- as.data.frame(list.files(mmecDir, recursive = T))
colnames(mmec_files) <- 'file'

for(i in 1:nrow(mmec_files)){
  mmec_files$lc1[i] <- strsplit(mmec_files$file[i], '/')[[1]][1]
}

table(mmec_files$lc1)

write.csv(mok, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_trainok.csv', row.names = F)

#test set 85
testset85 <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/test_set_eos_lucas85.csv', stringsAsFactors = F)

testset85$filepath_ftp <- gsub('/eos/jeodpp/data/projects/REFOCUS/LUCAS_COVER_W//', '/mnt/cidstorage/cidportal/data/OpenData/LUCAS/LUCAS_COVER//', testset85$filepatheos)

table(testset85$filepath_ftp %in% mok$filepath_ftp)

testset85_wattr <- merge(testset85, mok, by = 'filepath_ftp')
testset85_wattr$filepatheos <- NULL

write.csv(testset85_wattr, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_testSet85.csv', row.names = F)

#testset all
results_all_78 <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS_2/Results-all-images_py/78/cnn_output_data_check.csv', stringsAsFactors = F)

table(results_all_78$pointidyear %in%  mok$pointidyear)

mok_results_all_78 <- mok[mok$pointidyear %in% results_all_78$pointidyear,]

write.csv(mok_results_all_78, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_testSetAll.csv', row.names = F)

#trainset400
trainsetdir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/dataset_LUCAS/'
tranisetDf <- as.data.frame(list.files(trainsetdir, recursive = T))
colnames(tranisetDf) <- 'file'

for(i in 1:nrow(tranisetDf)){
  tranisetDf$pointid[i] <- substr(strsplit(strsplit(tranisetDf$file[i], '/')[[1]][[2]], '_')[[1]][[5]], 1, 8)
  tranisetDf$year[i] <- gsub('LUCAS','',strsplit(strsplit(tranisetDf$file[i], '/')[[1]][[2]], '_')[[1]][[1]])
}
tranisetDf$pointidyear <- paste0(tranisetDf$pointid, tranisetDf$year)

table(tranisetDf$pointidyear %in% mok$pointidyear)

trainset_mok <- mok[mok$pointidyear %in% tranisetDf$pointidyear,]

write.csv(trainset_mok, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_trainSet.csv', row.names = F)

##### rapha's question anout MMEC

lc_exif <- read.csv('/mnt/cidstorage/cidportal/data/OpenData/LUCAS/LUCAS_COVER/tables/lucas_cover_exif.csv', stringsAsFactors = F)
head(lc_exif)

#lc_exif$pointyearid <- gsub('\\(','',lc_exif$pointyearid)
#lc_exif$pointyearid <- gsub('\\)','',lc_exif$pointyearid)
#lc_exif$pointyearid <- gsub('\\,','',lc_exif$pointyearid)

lv_gt <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/gt_LUCAS.csv', stringsAsFactors = F)
head(lv_gt)
table(lv_gt$lc1)

#trainok
trainok_mok <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_trainok.csv', stringsAsFactors = F)

trainok_mok_exif <- lc_exif[lc_exif$pointidyear %in% trainok_mok$pointidyear, ]

#get x and y out
# for(i in 1:nrow(trainok_mok_exif)){
#   
#   trainok_mok_exif$image_x[i] <- as.integer(strsplit(trainok_mok_exif$imagesize[i], 'x')[[1]][1])
#   trainok_mok_exif$image_y[i] <- as.integer(strsplit(trainok_mok_exif$imagesize[i], 'x')[[1]][2])
#   
#   
# }

#max and min x and y of images in training set
# trainok_maxX <- max(trainok_mok_exif$image_x[!is.na(trainok_mok_exif$image_x)])
# trainok_minX <- min(trainok_mok_exif$image_x[!is.na(trainok_mok_exif$image_x) & trainok_mok_exif$image_x != 1])
# 
# trainok_maxY <- max(trainok_mok_exif$image_y[!is.na(trainok_mok_exif$image_y)])
# trainok_minY <- min(trainok_mok_exif$image_y[!is.na(trainok_mok_exif$image_y) & trainok_mok_exif$image_y != 1])

trainok_maxX <- max(trainok_mok_exif$ExifImageLength[!is.na(trainok_mok_exif$ExifImageLength)])
trainok_minX <- min(trainok_mok_exif$ExifImageLength[!is.na(trainok_mok_exif$ExifImageLength) & trainok_mok_exif$ExifImageLength != 0])

trainok_maxY <- max(trainok_mok_exif$ExifImageWidth[!is.na(trainok_mok_exif$ExifImageWidth)])
trainok_minY <- min(trainok_mok_exif$ExifImageWidth[!is.na(trainok_mok_exif$ExifImageWidth) & trainok_mok_exif$ExifImageWidth != 0])

#concat the xny
trainok_mok_exif$ImageSize <- paste0(trainok_mok_exif$ExifImageLength,'x', trainok_mok_exif$ExifImageWidth)
as.data.frame(table(trainok_mok_exif$ImageSize[!grepl('NA', trainok_mok_exif$ImageSize)]))

#trainset
trainset_mok <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_trainSet.csv', stringsAsFactors = F)

table(trainset_mok$pointidyear %in% lc_exif$pointidyear)

trainset_mok_exif <- lc_exif[lc_exif$pointidyear %in% trainset_mok$pointidyear, ]

#get x and y out
#for(i in 1:nrow(trainset_mok_exif)){
  
  trainset_mok_exif$image_x[i] <- as.integer(strsplit(trainset_mok_exif$imagesize[i], 'x')[[1]][1])
  trainset_mok_exif$image_y[i] <- as.integer(strsplit(trainset_mok_exif$imagesize[i], 'x')[[1]][2])
  
  
}

#max and min x and y of images in training set
# train_maxX <- max(trainset_mok_exif$image_x[!is.na(trainset_mok_exif$image_x)])
# train_minX <- min(trainset_mok_exif$image_x[!is.na(trainset_mok_exif$image_x)])
# 
# train_maxY <- max(trainset_mok_exif$image_y[!is.na(trainset_mok_exif$image_y)])
# train_minY <- min(trainset_mok_exif$image_y[!is.na(trainset_mok_exif$image_y)])

train_maxX <- max(trainset_mok_exif$ExifImageLength[!is.na(trainset_mok_exif$ExifImageLength)])
train_minX <- min(trainset_mok_exif$ExifImageLength[!is.na(trainset_mok_exif$ExifImageLength) & trainset_mok_exif$ExifImageLength != 0])

train_maxY <- max(trainset_mok_exif$ExifImageWidth[!is.na(trainset_mok_exif$ExifImageWidth)])
train_minY <- min(trainset_mok_exif$ExifImageWidth[!is.na(trainset_mok_exif$ExifImageWidth) & trainset_mok_exif$ExifImageWidth != 0])

#test85
test85_mok <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_testSet85.csv', stringsAsFactors = F)

table(test85_mok$pointidyear %in% lc_exif$pointidyear)

test85_mok_exif <- lc_exif[lc_exif$pointidyear %in% test85_mok$pointidyear, ]

#get x and y out
#for(i in 1:nrow(test85_mok_exif)){
  
  test85_mok_exif$image_x[i] <- as.integer(strsplit(test85_mok_exif$imagesize[i], 'x')[[1]][1])
  test85_mok_exif$image_y[i] <- as.integer(strsplit(test85_mok_exif$imagesize[i], 'x')[[1]][2])
  
  
}

#max and min x and y of images in training set
# test85_maxX <- max(test85_mok_exif$image_x[!is.na(test85_mok_exif$image_x)])
# test85_minX <- min(test85_mok_exif$image_x[!is.na(test85_mok_exif$image_x)])
# 
# test85_maxY <- max(test85_mok_exif$image_y[!is.na(test85_mok_exif$image_y)])
# test85_minY <- min(test85_mok_exif$image_y[!is.na(test85_mok_exif$image_y)])

test85_maxX <- max(test85_mok_exif$ExifImageLength[!is.na(test85_mok_exif$ExifImageLength)])
test85_minX <- min(test85_mok_exif$ExifImageLength[!is.na(test85_mok_exif$ExifImageLength) & test85_mok_exif$ExifImageLength != 0])

test85_maxY <- max(test85_mok_exif$ExifImageWidth[!is.na(test85_mok_exif$ExifImageWidth)])
test85_minY <- min(test85_mok_exif$ExifImageWidth[!is.na(test85_mok_exif$ExifImageWidth) & test85_mok_exif$ExifImageWidth != 0])

#testAll
testAll_mok <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/MMEC_trainOK/lucasvision_MMEC_testSetAll.csv', stringsAsFactors = F)

table(testAll_mok$pointidyear %in% lc_exif$pointyearid)

testAll_mok_exif <- lc_exif[lc_exif$pointidyear %in% testAll_mok$pointidyear, ]

#get x and y out
#for(i in 1:nrow(testAll_mok_exif)){
  
  testAll_mok_exif$image_x[i] <- as.integer(strsplit(testAll_mok_exif$imagesize[i], 'x')[[1]][1])
  testAll_mok_exif$image_y[i] <- as.integer(strsplit(testAll_mok_exif$imagesize[i], 'x')[[1]][2])
  
  
}

#max and min x and y of images in training set
# testAll_maxX <- max(testAll_mok_exif$image_x[!is.na(testAll_mok_exif$image_x)])
# testAll_minX <- min(testAll_mok_exif$image_x[!is.na(testAll_mok_exif$image_x)])
# 
# testAll_maxY <- max(testAll_mok_exif$image_y[!is.na(testAll_mok_exif$image_y)])
# testAll_minY <- min(testAll_mok_exif$image_y[!is.na(testAll_mok_exif$image_y)])

testAll_maxX <- max(testAll_mok_exif$ExifImageLength[!is.na(testAll_mok_exif$ExifImageLength)])
testAll_minX <- min(testAll_mok_exif$ExifImageLength[!is.na(testAll_mok_exif$ExifImageLength) & testAll_mok_exif$ExifImageLength != 0])

testAll_maxY <- max(testAll_mok_exif$ExifImageWidth[!is.na(testAll_mok_exif$ExifImageWidth)])
testAll_minY <- min(testAll_mok_exif$ExifImageWidth[!is.na(testAll_mok_exif$ExifImageWidth) & testAll_mok_exif$ExifImageWidth != 0])

#make df to compare
ImageSets <- c('MMEC', 'Train', 'Test85', 'TestAll')
MaxX <- c(trainok_maxX, train_maxX, test85_maxX, testAll_maxX)
MinX <- c(trainok_minX, train_minX, test85_minX, testAll_minX)
MaxY <- c(trainok_maxY, train_maxY, test85_maxY, testAll_maxY)
MinY <- c(trainok_minY, train_minY, test85_minY, testAll_minY)

df_compare <- data.frame(ImageSets, MaxX, MinX, MaxY, MinY)

df_compare$RatioX <- df_compare$MaxX / df_compare$MinX
df_compare$RatioY <- df_compare$MaxY / df_compare$MinY

xtable::xtable(df_compare)
