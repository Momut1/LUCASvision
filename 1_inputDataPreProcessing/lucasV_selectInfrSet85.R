############ FIND THE APPROPRIATE INFERENCE SET OF 85 IMG/CLASS WITH THE SAME DISTRIBUTION AS THE TRAIN SET #########
trainSet_tab_country.sum_df <- read.csv('/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/lucasV_trainSet_tab_country_sum_df.csv', stringsAsFactors = F)
df_lucas <- read.csv('/data/work/Ispra/LUCAS/finishingWork/lucas_last_checks/data/output/table/lucas_harmo_uf.csv', stringsAsFactors = F)
df_lucas <- df_lucas[,c(2,3,4,27)]
df_lucas$basename <- paste0(df_lucas$point_id, df_lucas$year)

df.crop.sample <- read.csv('/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results-all-images_py/0/cnn_output_data_check.csv', stringsAsFactors = F)
colnames(df.crop.sample)[grep('pointidyear', colnames(df.crop.sample))] <- 'basename'
df.crop.sample <- merge(df.crop.sample, df_lucas, by = 'basename')
table(df.crop.sample$lc1.x == df.crop.sample$lc1.y)
df.crop.sample$lc1.x <- NULL

df.crop85 <- data.frame()
#stratified sample of 85img/class with the same distribution as the train set
while(all(table(df.crop85$lc1) < 85)){
  for(llc1 in sort(unique(df.crop.sample$lc1))){
    dfInt_lc1 <- df.crop.sample[df.crop.sample$lc1 == llc1,]
    
    #dfSample <- dfInt[sample(nrow(dfInt), 85),]
    #df.crop85 <- rbind(df.crop85, dfSample)
    
    for(nutss0 in sort(unique(dfInt_lc1$nuts0))){
      dfInt_lc1_nuts0 <- dfInt_lc1[dfInt_lc1$nuts0 == nutss0,]
      proportion <- trainSet_tab_country.sum_df[trainSet_tab_country.sum_df$nuts0 == nutss0 ,grep(llc1, colnames(trainSet_tab_country.sum_df))]
      
      #control for EE and LU countries and small percentages
      if(proportion < 1){
        proportion <- ceiling(proportion)
      }
      
      #control for EE and LU crap
      if(!nutss0 %in% c('EE', 'LU')){
        #if number extracted from proportion is bigger than the population, just take the whole population
        if(as.integer(((proportion/100) * 85)) > nrow(dfInt_lc1_nuts0)){
          dfInt_lc1_nuts0_sample_prop <- dfInt_lc1_nuts0[sample(nrow(dfInt_lc1_nuts0), size = nrow(dfInt_lc1_nuts0), replace = FALSE),]
        }else{
          dfInt_lc1_nuts0_sample_prop <- dfInt_lc1_nuts0[sample(nrow(dfInt_lc1_nuts0), size = floor(((proportion/100) * 85)), replace = FALSE),]
        }
        
      }else{
        #if number extracted from proportion is bigger than the population, just take the whole population
        if(as.integer(((proportion/100) * 85)) > nrow(dfInt_lc1_nuts0)){
          dfInt_lc1_nuts0_sample_prop <- dfInt_lc1_nuts0[sample(nrow(dfInt_lc1_nuts0), size = nrow(dfInt_lc1_nuts0), replace = FALSE),]
        }else{
          dfInt_lc1_nuts0_sample_prop <- dfInt_lc1_nuts0[sample(nrow(dfInt_lc1_nuts0), size = ceiling(((proportion/100) * 85)), replace = FALSE),]
        }
      }
      
      
      if(nutss0 %in% c('EE', 'LU')){
        print(paste(llc1, nutss0, proportion, nrow(dfInt_lc1_nuts0_sample_prop)))
        #print(dfInt_lc1_nuts0)
        #print(sample(85, size = ceiling(((proportion/100) * 85))))
        #print(dfInt_lc1_nuts0_sample_prop)
      }
      
      
      df.crop85 <- rbind(df.crop85, dfInt_lc1_nuts0_sample_prop)
    }
  }
}

#remove NAs
df.crop85 <- df.crop85[!is.na(df.crop85$lc1.y),]
table(df.crop85$lc1.y)

#duplicated?
table(duplicated(df.crop85$basename))
df.crop85 <- df.crop85[!duplicated(df.crop85$basename),]
table(duplicated(df.crop85$basename))
table(df.crop85$lc1.y)

#sample veramente 85 because of while loop fuckery
underRepClasses <- c()
df.crop85_85fr <- data.frame()
for(llc1 in sort(unique(df.crop85$lc1.y))){
  print(llc1)
  dfInt_lc1 <- df.crop85[df.crop85$lc1.y == llc1,]
  
  if(nrow(dfInt_lc1) >= 85){
    dfSample <- dfInt_lc1[sample(nrow(dfInt_lc1), 85),]
    df.crop85_85fr <- rbind(df.crop85_85fr, dfSample)
  }else{
    underRepClasses <- c(underRepClasses, llc1)
    print(paste('class ', llc1, 'has less than 85 examples'))
  }
}
table(df.crop85_85fr$lc1)

#handle shitty under-represented classes
table(df.crop$lc1)

#format to match
df.crop85_85fr <- df.crop85_85fr[,-c(grep('RAW',colnames(df.crop85_85fr)))]
colnames(df.crop85_85fr)[grep('lc1', colnames(df.crop85_85fr))] <- 'lc1'
colnames(df.crop85_85fr)[grep('cnn_labels', colnames(df.crop85_85fr))] <- 'class_max'
colnames(df.crop85_85fr)[grep('cnn_values', colnames(df.crop85_85fr))] <- 'prob_max'
df.crop85_85fr$code <- df.crop85_85fr$lc1

#add B12's 85 records
df.crop85_85fr <- rbind(df.crop85_85fr, df.crop[df.crop$lc1 == 'B12',])
table(df.crop85_85fr$lc1)

# 
# #add B31
# df.crop85_b31 <- df.crop85[df.crop85$lc1.y == 'B31',]
# df.crop_b31 <- df.crop[df.crop$lc1 == 'B31',]
# df.crop_b31_extra <- df.crop_b31[sample(nrow(df.crop_b31), (85-nrow(df.crop85_b31)), replace = F),]
# #should be all F
# table(df.crop_b31_extra$basename %in% df.crop85_b31$basename)
# 
# #remove the ones that are duplicate
# df.crop_b31_extra <- df.crop_b31_extra[!df.crop_b31_extra$basename %in% df.crop_b31_extra$basename[df.crop_b31_extra$basename %in% df.crop85_b31$basename],]
# 
# #select extra_extra
# df.crop_b31_extra_extra <- df.crop_b31[sample(nrow(df.crop_b31), (85-(nrow(df.crop_b31_extra) + nrow(df.crop85_b31))), replace = F),]
# 
# table(df.crop_b31_extra_extra$basename %in% df.crop85_b31$basename)
# 
# #remove the ones that are duplicate
# df.crop_b31_extra_extra <- df.crop_b31_extra_extra[!df.crop_b31_extra_extra$basename %in% df.crop_b31_extra_extra$basename[df.crop_b31_extra_extra$basename %in% df.crop85_b31$basename],]
# 
# #bind the extra together
# df.crop_b31_extra <- rbind(df.crop_b31_extra, df.crop_b31_extra_extra)
# 
# #select extra_extra again
# df.crop_b31_extra_extra <- df.crop_b31[sample(nrow(df.crop_b31), (85-(nrow(df.crop_b31_extra) + nrow(df.crop85_b31))), replace = F),]
# 
# #should both be FALSE
# table(df.crop_b31_extra_extra$basename %in% df.crop85_b31$basename)
# table(df.crop_b31_extra_extra$basename %in% df.crop_b31_extra$basename)
# 
# #bind the extra together again
# df.crop_b31_extra <- rbind(df.crop_b31_extra, df.crop_b31_extra_extra)
# table(duplicated(df.crop_b31_extra$basename))
# 
# df.crop_b31_extra <- df.crop_b31_extra[!duplicated(df.crop_b31_extra$basename),]
# nrow(df.crop_b31_extra)
# table(duplicated(df.crop_b31_extra$basename))
# 
# #select extra_extra again again
# df.crop_b31_extra_extra <- df.crop_b31[sample(nrow(df.crop_b31), (85-(nrow(df.crop_b31_extra) + nrow(df.crop85_b31))), replace = F),]
# 
# #should both be FALSE
# table(df.crop_b31_extra_extra$basename %in% df.crop85_b31$basename)
# table(df.crop_b31_extra_extra$basename %in% df.crop_b31_extra$basename)
# 
# #bind the extra together again
# df.crop_b31_extra <- rbind(df.crop_b31_extra, df.crop_b31_extra_extra)
# table(duplicated(df.crop_b31_extra$basename))
# 
# #format to match
# df.crop85_b31 <- df.crop85_b31[,-c(grep('RAW',colnames(df.crop85_b31)))]
# colnames(df.crop85_b31)[grep('lc1', colnames(df.crop85_b31))] <- 'lc1'
# colnames(df.crop85_b31)[grep('cnn_labels', colnames(df.crop85_b31))] <- 'class_max'
# colnames(df.crop85_b31)[grep('cnn_values', colnames(df.crop85_b31))] <- 'prob_max'
# df.crop85_b31$code <- df.crop85_b31$lc1
# 
# #bind b31 together
# df.crop85_b31_all <- rbind(df.crop85_b31, df.crop_b31_extra)
# 
# #check
# nrow(df.crop85_b31_all) == 85
# table(duplicated(df.crop85_b31_all$basename))
# 
# #put in the right set df.crop85_85fr
# df.crop85_85fr <- rbind(df.crop85_85fr, df.crop85_b31_all)
# 
# table(df.crop85_85fr$lc1)
# 
# #add b33
# df.crop85_b33 <- df.crop85[df.crop85$lc1.y == 'B33',]
# df.crop_b33 <- df.crop[df.crop$lc1 == 'B33',]
# df.crop_b33_extra <- df.crop_b33[sample(nrow(df.crop_b33), (85-nrow(df.crop85_b33)), replace = F),]
# #should be all F
# table(df.crop_b33_extra$basename %in% df.crop85_b33$basename)
# 
# #format to match
# df.crop85_b33 <- df.crop85_b33[,-c(grep('RAW',colnames(df.crop85_b33)))]
# colnames(df.crop85_b33)[grep('lc1', colnames(df.crop85_b33))] <- 'lc1'
# colnames(df.crop85_b33)[grep('cnn_labels', colnames(df.crop85_b33))] <- 'class_max'
# colnames(df.crop85_b33)[grep('cnn_values', colnames(df.crop85_b33))] <- 'prob_max'
# df.crop85_b33$code <- df.crop85_b33$lc1
# 
# df.crop85_b33 <- rbind(df.crop85_b33, df.crop_b33_extra)
# 
# #check 
# nrow(df.crop85_b33) == 85
# table(duplicated(df.crop85_b33$basename))
# 
# #put in the right set df.crop85_85fr
# df.crop85_85fr <- rbind(df.crop85_85fr, df.crop85_b33)
# 
# #check
# table(df.crop85_85fr$lc1)
# 
# write.csv(df.crop85_85fr, '/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/v2/inputs/lucasV_testSet_cap85.csv')

tab_country<-table(df.crop85_85fr$nuts0,df.crop85_85fr$lc1)
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
    tab_country.sum_df_per[i,j] <- round((tab_country.sum_df[i,j] / 85) * 100, 3)
  }
}

tab_country.sum_df_per$nuts0 <- trainSet_tab_country.sum_df$nuts0
tab_country.sum_df_per$`Total # records` <- NULL

write.csv(df.crop85_85fr, '/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/v2/inputs/lucasV_testSet_cap85.csv')

tab_country<-table(df.crop85_85fr$nuts0,df.crop85_85fr$lc1)
tab_country.sum<- cbind(tab_country,'Total # records'=margin.table(tab_country,margin=1))
tab_country.sums.1<- rbind(tab_country.sum,'Total # records'=margin.table(tab_country.sum,margin=2))
tab_country.sum.1<-rbind(tab_country.sum,'Total # records'=margin.table(tab_country.sum,margin=2))
