#!/bin/bash

foldertoanalyze='../outputs/Best_model_LUCAS/'
gt_path='/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/gt_LUCAS.csv'
name_files_test='test_set_bad_eos_str1.txt'

inputs='../inputs/'
outputs='../outputs/'
#find $foldertoanalyze -maxdepth 5 -type f -name "*_infocommand.txt" > $inputs"list_runs-bbch.txt"
list_commands=$inputs'list_runs-lucas-2.txt'
commands=`cat $list_commands`
subfolder='Results-str1-bad/'

#control flag for the inceptiion run
SUB="Inception"
declare -a modelsToRun=(78)




  for c in $commands
  do
    com=`cat $c`
    count="$(basename -- $c | cut -d_ -f1)"
    name_run="$(echo $c| cut -d/ -f3- |awk -F /commands '{print $1}')"
    m=`echo $com |grep -oP 'http.?://\S+'`
    hw=`echo $m | cut -d _ -f4 | cut -d / -f1`

    if [ ! -f $outputs$name_run'/'$subfolder$count'/cnn_output_data.csv' ]; then
    	if [[ " ${modelsToRun[@]} " =~ " ${count} " ]]; then
   	  #to handle the inception runs
  	  if [ "$com" == *"$SUB"* ]; then
  	  hw=299
  	  fi

  	  echo "**********************************************************************"	
  	  echo $count
  	  echo $name_run
  	  echo $hw $m

  	  mkdir -p $outputs$name_run'/'$subfolder$count'/'
  	  python3 label_image_list.py \
  		  --image_list $inputs$name_files_test \
  		  --graph $outputs$name_run'/output_graph/'$count'.pb' \
  		  --labels $outputs$name_run'/output_labels/'$count'.txt' \
  		  --input_height $hw \
  		  --input_width $hw \
  		  --output_dir $outputs$name_run'/'$subfolder$count'/'\
  		  --gt_files $gt_path 
                

  	  wait
      fi
    fi  
  done


python3 tensorboard_vals_LUCAS.py --input $foldertoanalyze'retrain_logs/' --output $foldertoanalyze
wait

#create the overall file of stats
find $foldertoanalyze -maxdepth 5 -type f -name "*cnn_output_data_check.csv" > $inputs"list_cnn_out_best.txt"
mkdir -p $outputs$name_run
python3 overallstats.py \
  --results_path $inputs"list_cnn_out_best-2.txt" \
  --output_fullpath $foldertoanalyze'/Results-all-images-85.csv'\
  --labels '../outputs/Best_model_LUCAS/output_labels/4.txt' \
  --gt_files $gt_path \
  --resultstrainval_path '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Best_model_LUCAS/resume-train.csv'
wait
