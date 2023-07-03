library(splitstackshape)

lucas_cover <- read.csv('/eos/jeodpp/data/projects/REFOCUS/LUCAS_COVER_W/tables/lucas_cover_attr.csv', stringsAsFactors = F)

lucasV_crops <- c('B11','B12','B13','B14','B15','B16','B21','B22','B31','B32','B33','B55')

#keep only the ones for which crops we are interested in
lucas_cover_Vcrops <- lucas_cover[lucas_cover$lc1 %in% lucasV_crops,]
table(lucas_cover_Vcrops$lc1)

#remove the ones in trainOK
lucasV_trainok <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/lucasvision_trainok_aug.csv', stringsAsFactors = F)
table(lucasV_trainok$lc1)

table(lucasV_trainok$pointidyear %in% lucas_cover_Vcrops$pointidyear)

lucas_cover_Vcrops_minusTrainOK <- lucas_cover_Vcrops[! lucas_cover_Vcrops$pointidyear %in% lucasV_trainok$pointidyear,]
table(lucas_cover_Vcrops_minusTrainOK$lc1)

#sample 1000 randomly per each lc1
lucasV_s1000 <- data.frame()
for(llc1 in lucasV_crops){
  lucas_cover_Vcrops_minusTrainOK_llc1 <- lucas_cover_Vcrops_minusTrainOK[lucas_cover_Vcrops_minusTrainOK$lc1 == llc1,]
  
  lucas_cover_Vcrops_minusTrainOK_llc1_s1000 <- lucas_cover_Vcrops_minusTrainOK_llc1[sample(nrow(lucas_cover_Vcrops_minusTrainOK_llc1), 1000),]
  
  lucasV_s1000 <- rbind(lucasV_s1000, lucas_cover_Vcrops_minusTrainOK_llc1_s1000)
}
table(lucasV_s1000$lc1)


#set filepatheos
lucasV_s1000$filepatheos <- file.path('/eos/jeodpp/data/projects/REFOCUS/LUCAS_COVER_W', paste0('LUCAS', lucasV_s1000$year), lucasV_s1000$nuts0, substr(lucasV_s1000$point_id, 1,3), substr(lucasV_s1000$point_id, 4,6), paste0('LUCAS', lucasV_s1000$year, '_', lucasV_s1000$point_id, '_Cover.jpg'))
table(file.exists(lucasV_s1000$filepatheos))

copydir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/testSet_badImgs'
#copy them to new folders to visually check
for(i in 1:nrow(lucasV_s1000)){
  sourcefile <- lucasV_s1000$filepatheos[i]
  targetfile <- file.path(copydir, lucasV_s1000$lc1[i], paste0('LUCAS', lucasV_s1000$year[i], '_', lucasV_s1000$point_id[i], '_Cover.jpg'))
  
  file.copy(sourcefile, targetfile)
}


###turn selection do df
badImgDir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/testSet_badImgs'
badImg_df <- as.data.frame(file.path(badImgDir, list.files(badImgDir, recursive = T)))
colnames(badImg_df) <- 'filepatheos'
head(badImg_df)

for(i in 1:nrow(badImg_df)){
  badImg_df$lc1[i] <- strsplit(badImg_df$filepatheos[i], '/')[[1]][12]
  badImg_df$year[i] <- gsub('LUCAS','',strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][1])
  badImg_df$pointid[i] <- strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][2]
  badImg_df$pointidyear[i] <- paste0(badImg_df$pointid[i], badImg_df$year[i])
  badImg_df$condition[i] <- gsub('.jpg','',strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][4])
}
head(badImg_df)


#check which crops dont have a image 
dfToResampleFor <- data.frame(matrix(NA, nrow = 1, ncol = 3))
colnames(dfToResampleFor) <- c('lc1', 'year', 'conditions2sample4')
yearss <- unique(badImg_df$year)
for(i in 1:length(yearss)){
  year_i <- yearss[i]
  
  #work on a single year
  badImg_df_i <- badImg_df[badImg_df$year == year_i,]
  
  #make occurrence table for relevant year
  badImg_df_i_t <- as.data.frame.matrix(table(badImg_df_i$lc1, badImg_df_i$condition))
  badImg_df_i_t$lc1 <- row.names(badImg_df_i_t)
  
  #iterate over every row and extract the 0 and its position
  for(j in 1:nrow(badImg_df_i_t)){
    colIdx_df <- as.data.frame(which(badImg_df_i_t[j,] == 0,arr.ind=T))
    
    #assign to new df the output
    
    #control for instances where there is more than one condition with 0 examples for the relevant year
    if(nrow(colIdx_df) == 1){
      colIdx <- which(badImg_df_i_t[j,] == 0,arr.ind=T)[2]
      conditionType <- colnames(badImg_df_i_t[j,])[colIdx]
      rowVec <- as.data.frame(t(c(badImg_df_i_t[j,]$lc1, year_i, conditionType)))
      colnames(rowVec) <- c('lc1', 'year', 'conditions2sample4')
      dfToResampleFor <- rbind(dfToResampleFor, rowVec)
    }else{
      
      conditionType <- c()
      for(jj in 1:nrow(colIdx_df)){
        conditionType <- c(conditionType,colnames(badImg_df_i_t[j,])[colIdx_df$col[jj]])
      }
      
      conditionType <- paste0(conditionType, collapse = ',')
      rowVec <- as.data.frame(t(c(badImg_df_i_t[j,]$lc1, year_i, conditionType)))
      colnames(rowVec) <- c('lc1', 'year', 'conditions2sample4')
      dfToResampleFor <- rbind(dfToResampleFor, rowVec)
      
    }
  }
}

#add missing for for 2006 B55
# conditionType <- paste0(c('Blurry', 'Close Early', 'Landscape', 'Object', 'PHarvest'), collapse = ',')
# rowVec <- as.data.frame(t(c('B55', '2006', conditionType)))
# colnames(rowVec) <- c('lc1', 'year', 'conditions2sample4')
# dfToResampleFor <- rbind(dfToResampleFor, rowVec)
#### NB !! - NO B55 CLASS IN LUCAS2006

dfToResampleFor_noNA <- dfToResampleFor[! is.na(dfToResampleFor$lc1),]
dfToResampleFor_noNA <- dfToResampleFor_noNA[dfToResampleFor_noNA$conditions2sample4 != 'NA',]
write.csv(dfToResampleFor_noNA, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/reSampleDir/dfToResampleFor_noNA.csv', row.names = F)



####Ready to resample
#remove the badImages already selected
nrow(badImg_df)
lucas_cover_Vcrops_minusTrainOK_minBadImgdf <- lucas_cover_Vcrops_minusTrainOK[! lucas_cover_Vcrops_minusTrainOK$pointidyear %in% badImg_df$pointidyear,]

dfSample100 <- data.frame()
#now sample per year
for(year_i in yearss){
  #print(year_i)
  
  #see what we need for the relevant year
  dfToResampleFor_yearI <- dfToResampleFor_noNA[dfToResampleFor_noNA$year == year_i,]
  
  #take only images from the year
  lucas_cover_Vcrops_minusTrainOK_minBadImgdf_yearI <- lucas_cover_Vcrops_minusTrainOK_minBadImgdf[lucas_cover_Vcrops_minusTrainOK_minBadImgdf$year == year_i,]
  
  for(llc1 in sort(unique(dfToResampleFor_yearI$lc1))){
    #print(llc1)
    lucas_cover_Vcrops_minusTrainOK_minBadImgdf_yearI_llc1 <- lucas_cover_Vcrops_minusTrainOK_minBadImgdf_yearI[lucas_cover_Vcrops_minusTrainOK_minBadImgdf_yearI$lc1 == llc1,]
    
    sample100 <- lucas_cover_Vcrops_minusTrainOK_minBadImgdf_yearI_llc1[sample(nrow(lucas_cover_Vcrops_minusTrainOK_minBadImgdf_yearI_llc1), 100),]
    
    dfSample100 <- rbind(dfSample100, sample100)
    #print('--------------------------------------------------------------------')
  }
}
table(dfSample100$lc1)

dfSample100$filepatheos <- file.path('/eos/jeodpp/data/projects/REFOCUS/LUCAS_COVER_W', paste0('LUCAS', dfSample100$year), dfSample100$nuts0, substr(dfSample100$point_id, 1,3), substr(dfSample100$point_id, 4,6), paste0('LUCAS', dfSample100$year, '_', dfSample100$point_id, '_Cover.jpg'))
table(file.exists(dfSample100$filepatheos))


#copy them to new folders for manual screening
reSampleDir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/reSampleDir'
for(i in 1:nrow(dfSample100)){
  
  if(! dir.exists(file.path(reSampleDir, dfSample100$year[i], dfSample100$lc1[i]))){
    dir.create(file.path(reSampleDir, dfSample100$year[i], dfSample100$lc1[i]), recursive = T)
  }
  
  sourcefile <- dfSample100$filepatheos[i]
  targetfile <- file.path(reSampleDir, dfSample100$year[i], dfSample100$lc1[i], paste0('LUCAS', dfSample100$year[i], '_', dfSample100$point_id[i], '_Cover.jpg'))
  
  file.copy(sourcefile, targetfile)
}

#sample 1 bad condition per class
badImgDir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/testSet_badImgs'
badImg_df <- as.data.frame(file.path(badImgDir, list.files(badImgDir, recursive = T)))
colnames(badImg_df) <- 'filepatheos'
head(badImg_df)

for(i in 1:nrow(badImg_df)){
  badImg_df$lc1[i] <- strsplit(badImg_df$filepatheos[i], '/')[[1]][12]
  badImg_df$year[i] <- gsub('LUCAS','',strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][1])
  badImg_df$pointid[i] <- strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][2]
  badImg_df$pointidyear[i] <- paste0(badImg_df$pointid[i], badImg_df$year[i])
  badImg_df$condition[i] <- gsub('.jpg','',strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][4])
}
head(badImg_df)

table(badImg_df$lc1, badImg_df$condition, badImg_df$year)

##### RESAMPLE 2
#####left to sample for
# 2009 - B16
# 2015 - B12
# 2018 - B11
# 2018 - B12
# 2018 - B14

year <- c(2009, 2015, 2018, 2018, 2018)
lc1 <- c('B16', 'B12', 'B11', 'B12', 'B14')
df_toSample4 <- data.frame(year, lc1)

#remove the badImages already selected
nrow(badImg_df)
lucas_cover_Vcrops_minusTrainOK_minBadImgdf <- lucas_cover_Vcrops_minusTrainOK[! lucas_cover_Vcrops_minusTrainOK$pointidyear %in% badImg_df$pointidyear,]

dfSample1000 <- data.frame()
#now sample per year
for(i in 1:nrow(df_toSample4)){
  #print(year_i)
  
  #see what we need for the relevant year and lc1
  lucas_cover_Vcrops_minusTrainOK_minBadImgdf_i <- lucas_cover_Vcrops_minusTrainOK_minBadImgdf[lucas_cover_Vcrops_minusTrainOK_minBadImgdf$year == df_toSample4$year[i] & lucas_cover_Vcrops_minusTrainOK_minBadImgdf$lc1 == df_toSample4$lc1[i],]
  
  sample1000 <- lucas_cover_Vcrops_minusTrainOK_minBadImgdf_i[sample(nrow(lucas_cover_Vcrops_minusTrainOK_minBadImgdf_i), 1000),]
  
  dfSample1000 <- rbind(dfSample1000, sample1000)
  #print('--------------------------------------------------------------------')
}
table(dfSample1000$lc1, dfSample1000$year)

dfSample1000$filepatheos <- file.path('/eos/jeodpp/data/projects/REFOCUS/LUCAS_COVER_W', paste0('LUCAS', dfSample1000$year), dfSample1000$nuts0, substr(dfSample1000$point_id, 1,3), substr(dfSample1000$point_id, 4,6), paste0('LUCAS', dfSample1000$year, '_', dfSample1000$point_id, '_Cover.jpg'))
table(file.exists(dfSample1000$filepatheos))

#copy them to new folders for manual screening
reSampleDir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/reSampleDir2'
for(i in 1:nrow(dfSample1000)){
  
  if(! dir.exists(file.path(reSampleDir, dfSample1000$year[i], dfSample1000$lc1[i]))){
    dir.create(file.path(reSampleDir, dfSample1000$year[i], dfSample1000$lc1[i]), recursive = T)
  }
  
  sourcefile <- dfSample1000$filepatheos[i]
  targetfile <- file.path(reSampleDir, dfSample1000$year[i], dfSample1000$lc1[i], paste0('LUCAS', dfSample1000$year[i], '_', dfSample1000$point_id[i], '_Cover.jpg'))
  
  file.copy(sourcefile, targetfile)
}



#after second resample
badImgDir <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/testSet_badImgs'
badImg_df <- as.data.frame(file.path(badImgDir, list.files(badImgDir, recursive = T)))
colnames(badImg_df) <- 'filepatheos'
head(badImg_df)

for(i in 1:nrow(badImg_df)){
  badImg_df$lc1[i] <- strsplit(badImg_df$filepatheos[i], '/')[[1]][12]
  badImg_df$year[i] <- gsub('LUCAS','',strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][1])
  badImg_df$pointid[i] <- strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][2]
  badImg_df$pointidyear[i] <- paste0(badImg_df$pointid[i], badImg_df$year[i])
  badImg_df$condition[i] <- gsub('.jpg','',strsplit(strsplit(badImg_df$filepatheos[i], '/')[[1]][13], '_')[[1]][4])
}
head(badImg_df)

table(badImg_df$lc1, badImg_df$condition, badImg_df$year)

write.csv(badImg_df, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/lucasV_badImg_df.csv')


#select 1 image per crop per year per bad image condition
table(badImg_df$lc1, badImg_df$condition)
badImg_df_str1 <- stratified(badImg_df, c('year', 'condition', 'lc1'), 1)

table(badImg_df_str1$lc1, badImg_df_str1$condition)

write.csv(badImg_df_str1, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/lucasV_badImg_df_str1.csv')

writeLines(badImg_df_str1$filepatheos, '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/test_set_bad_eos_str1.txt')
