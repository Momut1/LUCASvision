#compare the results csv for each run
bestmodels <- c(4,49,58,78,88)

#dirs with csvs
random_search_dir_allimg <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results-all-images'
best_model_dir_allimg <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results-all-images'
best_model_dir_85 <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/Results85'
best_model_2_dir_85 <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS_2/Results-85'

#get the csvs
random_searc_csvs_allimg <- file.path(random_search_dir_allimg, bestmodels, 'cnn_output_data_check.csv')
best_model_csvs_85 <- file.path(best_model_dir_85, bestmodels, 'cnn_output_data_check.csv')
best_model_2_csvs_85 <- file.path(best_model_2_dir_85, bestmodels, 'cnn_output_data_check.csv')

#loop through them 
for(i in 1:length(bestmodels)){
  print(bestmodels[i])
  
  #compare the two best model ones for a single run (4)
  best_model_csvs_85_i <- read.csv(best_model_csvs_85[i], stringsAsFactors = F)
  #head(best_model_csvs_85_4)
  
  best_model_2_csvs_85_i <- read.csv(best_model_2_csvs_85[i], stringsAsFactors = F)
  #head(best_model_2_csvs_85_4)
  
  #are they the same length of rows
  if( nrow(best_model_csvs_85_i) == nrow(best_model_2_csvs_85_i)){
    
    #are they ordered the same
    if(all(table(best_model_csvs_85_i$pointidyear == best_model_2_csvs_85_i$pointidyear)) == TRUE){
      
      #compare the top1 label prediction
      if(all(table(best_model_csvs_85_i$cnn_labels ==  best_model_2_csvs_85_i$cnn_labels)) == TRUE){
        print(paste0('best models for run number ', bestmodels[i], ' are identical'))
        print('-------------------------------------------------------------------------------------')
      }else{print(paste0('best models for run number ', bestmodels[i], ' are NOT identical'))}
      
      
    }else{print(paste0('best models for run number ', bestmodels[i], ' are not ordered in the same way for pointidyear'))}
    
  }else{print(paste0('best models for run number ', bestmodels[i], ' dont have the same nrow'))}
}

#check if the test85 on disk has the same pointidyears as the ones from the best model runs
testSet85 <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/lucasV_testSet_cap85.csv', stringsAsFactors = F)
head(testSet85)

#should be all T
table(testSet85$basename %in% best_model_2_csvs_85_i$pointidyear)

#loop them
for(i in 1:length(bestmodels)){
  print(bestmodels[i])
  
  #take out the 85/class from the allimages random search
  random_search_i <- read.csv(random_searc_csvs_allimg[i], stringsAsFactors = F)
  #head(random_search_i)
  
  #are all the pointidyears from the testSet85 in the random_search csv for the relevant run - should be T
  if(all(table(testSet85$basename %in% random_search_i$pointidyear)) == TRUE){
    
    #reduce to the 85
    random_search_i_85 <- random_search_i[random_search_i$pointidyear %in% testSet85$basename,]
    
    if(nrow(random_search_i_85) == 1020){
      
      #load in the resultscsv from the respective run of the best model runs
      best_model_2_csvs_85_i <- read.csv(best_model_2_csvs_85[i], stringsAsFactors = F)
      
      #are all the pointidyears in? should be all T
      if(all(table(random_search_i_85$pointidyear %in% best_model_2_csvs_85_i$pointidyear))== TRUE){
        
        # #is the order the same 
        # table(random_search_i_85$pointidyear == best_model_2_csvs_85_i$pointidyear)

        #order them in the same order
        random_search_i_85_ordered <- random_search_i_85[match(best_model_2_csvs_85_i$pointidyear, random_search_i_85$pointidyear),]
        
        #is the order the same NOW - Should be all T
        if(all(table(random_search_i_85_ordered$pointidyear == best_model_2_csvs_85_i$pointidyear))== TRUE){
          
          #convert b to B in cnn_labels
          best_model_2_csvs_85_i$cnn_labels <- toupper(best_model_2_csvs_85_i$cnn_labels)
          
          #is the output label the same?
          print(table(random_search_i_85_ordered$cnn_labels == best_model_2_csvs_85_i$cnn_labels))
          
        }else{print(paste0('random_search models for run ',bestmodels[i],' after ordering doesnt have the same order'))}
        
      }else{print(paste0('random_search models for run ',bestmodels[i],' doesnt have the same pointidyears as the respective csv from the best model 2 run'))}
      
    }else{print(paste0('random_search models for run ',bestmodels[i],' doesnt have nrow == 1200'))}
    
  }else{print(paste0('random_search models for run ',bestmodels[i],' dont have all the pointidyears from the testSet85'))}

}

###### THE OUTPUT CSVs of each run from the random search and from the best model (2) do not have the same predictions
####### ERGO - they should not have the same accuracy in the end


#the diff versions of this df85 are the same?
diskDir <- '/eos/jeodpp/data/projects/REFOCUS/data'

lucasV_testSet_cap85 <- read.csv(file.path(diskDir, '/LUCAS_C_vision/v2/inputs/lucasV_testSet_cap85.csv'), stringsAsFactors = F)
head(lucasV_testSet_cap85)

head(testSet85)

table(lucasV_testSet_cap85$basename %in% testSet85$basename)
###YES!