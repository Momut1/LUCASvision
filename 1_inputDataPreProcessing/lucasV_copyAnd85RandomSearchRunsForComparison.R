#compare the results csv for each run
bestmodels <- c(4,49,58,78,88)

#the test set of 85/class
testSet85 <- read.csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/lucasV_testSet_cap85.csv', stringsAsFactors = F)
head(testSet85)

#dirs with csvs
random_search_dir_allimg <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results-all-images'
random_searc_csvs_allimg <- file.path(random_search_dir_allimg, bestmodels, 'cnn_output_data_check.csv')

dirToCopy <- '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results-all-images_bestModels/'
#copy them to a new folder to work with
for(i in 1:length(random_searc_csvs_allimg)){
  sourcefile <- random_searc_csvs_allimg[i]
  runn <- strsplit(random_searc_csvs_allimg[i], '/')[[1]][13]
  
  if(! dir.exists(file.path(dirToCopy, runn))){
    dir.create(file.path(dirToCopy, runn))
  }
  
  destfile <- file.path(dirToCopy, runn, "cnn_output_data_check.csv")
  
  file.copy(sourcefile, destfile)
  
  #read in tthe csv and keep only the 85 per crop
  resultscsv_i <- read.csv(destfile, stringsAsFactors = F)
  
  resultscsv_i_85 <- resultscsv_i[resultscsv_i$pointidyear %in% testSet85$basename,]
  
  if(nrow(resultscsv_i_85) == 1020){
    write.csv(resultscsv_i_85, destfile)
  }else{print(random_searc_csvs_allimg[i])}
}

