train <- read.csv('/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/inputs/lucas_2018c__sample_cleaned_train.csv')
head(train)
table(train$crop)

lucas <- read.csv('/data/work/Ispra/LUCAS/finishingWork/lucas_harmo.csv', stringsAsFactors = F)
table(lucas$year)

#take only 2018
lucas2018 <- lucas[lucas$year == 2018,]
head(lucas2018)

#take only crops we work on
lucas2018_vision <- lucas2018[lucas2018$lc1 %in% unique(as.character(train$crop)),]
unique(lucas2018_vision$lc1_spec_label)

#check cereals (B11, B12, B13) for spring/winter varieties
lucas2018_vision_b11 <- lucas2018_vision[lucas2018_vision$lc1 == 'B11',]
head(lucas2018_vision_b11)
unique(lucas2018_vision_b11$lc1_spec_label)

B11_SPRING <- c('Denmark', 'Estonia', 'Finland', 'Ireland', 'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Spain', 'UK')
B11_WINTER <- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'UK')
B11_DIFF <- B11_WINTER[! B11_WINTER %in% B11_SPRING] 
paste(B11_DIFF, collapse = ', ')

lucas2018_vision_b12 <- lucas2018_vision[lucas2018_vision$lc1 == 'B12',]
head(lucas2018_vision_b12)
unique(lucas2018_vision_b12$lc1_spec_label)

lucas2018_vision_b13 <- lucas2018_vision[lucas2018_vision$lc1 == 'B13',]
head(lucas2018_vision_b13)
unique(lucas2018_vision_b13$lc1_spec_label)

B13_SPRING <- c('Austria', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'Germany', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Poland', 'Slovakia', 'Slovenia', 'Spain', 'UK')
B13_WINTER <- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Finland', 'France', 'Germany', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'UK')
B13_DIFF <- B13_WINTER[! B13_WINTER %in% B13_SPRING] 
paste(B13_DIFF, collapse = ', ')

#check potato (B21) for early/late ware variety
lucas2018_vision_b21 <- lucas2018_vision[lucas2018_vision$lc1 == 'B21',]
head(lucas2018_vision_b21)
unique(lucas2018_vision_b21$lc1_spec_label)
B21_OVERLAP <- c('Bulgaria', 'Hungary', 'Lithuania', 'Poland', 'Romania')

#which countries work for all problem crops
countriesWork <- intersect(intersect(B11_DIFF,B13_DIFF),B21_OVERLAP)

#take only mature crops
ccdir <- '/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/cropcalendar'
ccs <- list.files(ccdir)

df_matureCrops <- data.frame(matrix(NA, nrow=length(ccs), ncol = 4))
colnames(df_matureCrops) <- c('CC', 'Bulgaria', 'Romania', 'Hungary')
for(i in 1:length(ccs)){
  cc <- read.csv(file.path(ccdir, ccs[i]), stringsAsFactors = F)
  cc3 <- cc[cc$Country %in% countriesWork,]
  
  #if clause to account for CCs where countriesWork are not present with values
  if(!all(is.na(cc3[,3:ncol(cc3)]))){
    df_matureCrops$CC[i] <- ccs[i]
    
    for(j in 1:nrow(cc3)){
      dfInt <- cc3[j,]
      dfintCountry <- dfInt$Country
      df_matureCrops[,dfintCountry][i] <- paste(colnames(dfInt[which(dfInt == 3)]), collapse = ', ')
      
    }
  }
}

df_matureCrops <- df_matureCrops[complete.cases(df_matureCrops), ]

#B11_B12 Split
df_matureCrops[nrow(df_matureCrops) +1,] <- df_matureCrops[1,]
df_matureCrops <- df_matureCrops[order(df_matureCrops$CC),]
write.csv(df_matureCrops, file.path('/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/df_matureCrops.csv'))
df_matureCrops <- read.csv('/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/df_matureCrops.csv', stringsAsFactors = F)
#common wheat == hard wheat

#preprocesses to be able to select training set
for(i in 1:nrow(df_matureCrops)){
  
  #extract LC1
  if(i == 1){
    df_matureCrops$LC1[i] <- strsplit(df_matureCrops$CC[i], '_')[[1]][1]
  }else if( i == 2){
    df_matureCrops$LC1[i] <- strsplit(df_matureCrops$CC[i], '_')[[1]][2]
  }else{
    df_matureCrops$LC1[i] <- strsplit(df_matureCrops$CC[i], '_')[[1]][1]
  }
  
  #extract month as numeral
  df_matureCrops$BulgariaNum[i] <- paste(unique(match(unlist(strsplit(gsub('[0-9,]+', '', df_matureCrops$Bulgaria[i]), split=' '))  ,month.abb)), collapse = ',')
  df_matureCrops$RomaniaNum[i] <- paste(unique(match(unlist(strsplit(gsub('[0-9,]+', '', df_matureCrops$Romania[i]), split=' '))  ,month.abb)), collapse = ',')
  df_matureCrops$HungaryNum[i] <- paste(unique(match(unlist(strsplit(gsub('[0-9,]+', '', df_matureCrops$Hungary[i]), split=' '))  ,month.abb)), collapse = ',')
}

df_matureCrops_w <- df_matureCrops[,c(6,7,8,9)]
write.csv(df_matureCrops_w, '/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/df_matureCrops_w.csv')
