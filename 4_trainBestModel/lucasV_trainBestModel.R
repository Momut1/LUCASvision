rm(list=ls())

eos_dir <- '/eos/jeodpp/data/projects/REFOCUS'
name_run <- file.path(eos_dir, 'data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS_2')
image_dir <- file.path(eos_dir, 'data/LUCAS_C_vision/v2/inputs/dataset_LUCAS')
output_graph_dir <- file.path(eos_dir, 'data/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/output_graph')
intermediate_output_graphs_dir <- file.path(name_run, 'intermediate_graph')
intermediate_store_frequency <- 500
epochs <- 3000
bottleneck_dir <- file.path(eos_dir, 'data/LUCAS_C_vision/v2/inputs/bottleneck_LUCAS')
tf_module <- 'https://tfhub.dev/google/imagenet/mobilenet_v2_140_224/classification/3'
saved_model_dir <- 'export_graph/'
checkpoint_path <- 'retrain_checkpoint/'

Results_lucas_random <- read.csv(file.path(eos_dir, '/data/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results.csv'), stringsAsFactors = F)
Results_lucas_random_best <- Results_lucas_random[Results_lucas_random$count %in% c(4, 49, 58, 78, 88),]

bestRuns <- Results_lucas_random_best$count

for(i in bestRuns){
  print(i)
  
  #get all the variables as from the table
  row_i <- Results_lucas_random_best[Results_lucas_random_best$count == i,]
  lr_i <- row_i$LR
  bs_i <- row_i$BS
  mom_i <- row_i$momentum
  
  #structure the retrain command
  command_i <- paste0('python3 /eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/scripts/retrain.py', 
         ' --image_dir ', image_dir,
         ' --output_graph ', file.path(output_graph_dir, paste0(i, '.pb')),
         ' --intermediate_output_graphs_dir ', file.path(intermediate_output_graphs_dir, i),
         ' --intermediate_store_frequency ', intermediate_store_frequency,
         ' --output_labels ', file.path(name_run, 'output_labels', paste0(i, '.txt')),
         ' --summaries_dir ', file.path(name_run, 'summaries_dir', i),
         ' --how_many_training_steps ', epochs,
         ' --learning_rate ', lr_i,
         ' --train_batch_size ', bs_i,
         ' --momentum ', mom_i,
         ' --bottleneck_dir ', bottleneck_dir,
         ' --tfhub_module ', tf_module,
         ' --saved_model_dir ', file.path(name_run, saved_model_dir, i),
         ' --checkpoint_path ', file.path(name_run, checkpoint_path, i, '/_retrain_checkpoint'),
         ' --Adam False',
         ' --flip_left_right True')
  
  #make directory tree
  if(!dir.exists(file.path(name_run, 'output_graph'))){
    dir.create(file.path(name_run, 'output_graph'))
  }
  
  if(!dir.exists(intermediate_output_graphs_dir)){
    dir.create(intermediate_output_graphs_dir)
  }
  
  if(!dir.exists(file.path(name_run, 'output_labels'))){
    dir.create(file.path(name_run, 'output_labels'))
  }
  
  if(!dir.exists(file.path(name_run, 'summaries_dir'))){
    dir.create(file.path(name_run, 'summaries_dir'))
  }
  
  if(!dir.exists(file.path(name_run, saved_model_dir))){
    dir.create(file.path(name_run, saved_model_dir))
  }
  
  if(!dir.exists(file.path(name_run, checkpoint_path))){
    dir.create(file.path(name_run, checkpoint_path))
  }
  
  if(!dir.exists(file.path(name_run, 'commands'))){
    dir.create(file.path(name_run, 'commands'))
  }
  
  #save command to folder
  writeLines(command_i, file.path(name_run, 'commands', paste0(i, '_infocommand.txt')))
  
  system(command_i)
}
