#!/bin/bash
input="../inputs/"
output="../outputs/"
#bottleneck_dir=$input'bottleneck_megaaugmented'
bottleneck_dir=$input'bottleneck'
#image_dir=$input'dataset_megaaugmented/'
image_dir=$input'dataset/'
output_graph='output_graph'
intermediate_output_graphs_dir='intermediate_graph/'
output_labels='output_labels'
summaries_dir='retrain_logs'
saved_model_dir='export_graph/'
checkpoint_path='retrain_checkpoint/'
name_run=$output'Best_model_LUCAS/'
fromwheremodel=$output'Random_search_LUCAS/'
intermediate_store_frequency=999
how_many_training_steps=3000


declare -a counts=(4 78 88 49 58)
#declare -a counts=(35 37 40)

#set the specific vaiables

for count in "${counts[@]}"
do
  string=`cat $fromwheremodel'commands/'$count'_infocommand.txt'`
  learning_rate=$(echo $string | sed "s/^.*--learning_rate\s*\(\S*\).*$/\1/")
  momentum=$(echo $string | sed "s/^.*--momentum\s*\(\S*\).*$/\1/")
  train_batch_size=$(echo $string | sed "s/^.*--train_batch_size\s*\(\S*\).*$/\1/")
  model=$(echo $string |grep -oP 'http.?://\S+')

  mkdir -p $name_run'commands/'

	echo python3.6 retrain.py \
		--image_dir $image_dir \
		--output_graph $name_run$output_graph'/'$count'.pb' \
		--intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
		--intermediate_store_frequency $intermediate_store_frequency \
		--output_labels $name_run$output_labels'/'$count'.txt' \
		--summaries_dir $name_run$summaries_dir'/'$count \
		--how_many_training_steps $how_many_training_steps \
		--learning_rate $learning_rate \
        --momentum $momentum \
		--train_batch_size $train_batch_size \
		--bottleneck_dir $bottleneck_dir \
		--tfhub_module $model \
		--saved_model_dir $name_run$saved_model_dir$count'/' \
		--checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
		--train_from_ckpt $fromwheremodel'retrain_checkpoint/'$count'/_retraincheckpoint' \
		--Adam 'True'\
		--random_brightness 5\
		--flip_left_right True >$name_run'commands/'$count'_infocommand.txt'

	#create all the folders needed for this run
	mkdir -p $name_run$output_graph'/'
	mkdir -p $name_run$intermediate_output_graphs_dir$count'/'
	mkdir -p $name_run$output_labels'/'
	mkdir -p $name_run$summaries_dir'/'
	mkdir -p $name_run$saved_model_dir
	mkdir -p $name_run$checkpoint_path$count'/'

	python3.6 retrain.py \
		--image_dir $image_dir \
		--output_graph $name_run$output_graph'/'$count'.pb' \
		--intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
		--intermediate_store_frequency $intermediate_store_frequency \
		--output_labels $name_run$output_labels'/'$count'.txt' \
		--summaries_dir $name_run$summaries_dir'/'$count \
		--how_many_training_steps $how_many_training_steps \
		--learning_rate $learning_rate \
		--momentum $momentum \
		--train_batch_size $train_batch_size \
		--bottleneck_dir $bottleneck_dir \
		--tfhub_module $model \
		--saved_model_dir $name_run$saved_model_dir$count'/' \
		--checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
		--train_from_ckpt $fromwheremodel'retrain_checkpoint/'$count'/_retraincheckpoint' \
		--Adam 'True'\
		--random_brightness 5\
		--flip_left_right True
	wait

done

