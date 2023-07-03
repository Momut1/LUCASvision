#!/bin/bash

name_run='../outputs/Random_search_LUCAS/'
intermediate_store_frequency=999
how_many_training_steps=3000
declare -a learning_rate=(0.0011002718452957356 0.009618115765022606 0.0003430204402546969 0.0003702030564246239 0.0007485658319553307 0.004720956513495432 0.0009826525828295317 0.0002168902353435396 0.002409849939202288 0.00046463294457832575 0.32676836115548213 0.6365902753690436 0.00014288610573906098 0.0625791990040654 0.019545194653809165 0.3908678950575244 0.04651574703021664 0.015536385903333375 0.06452232375450129 0.00351487593272056 0.05994459081146436 0.1992874728778628 0.007289034402507295 0.5782291742660336 0.1640438676255698 0.8579065823604026 0.00047703473161094824 0.012143391589715839 0.6054384520387095 0.039642405537496934 0.27754282213051296 0.7645044188747984 0.000552293586480187 0.0009385555885268284 0.3116018303105462 0.42180332608534715 0.10792200371718291 0.0024412373169742803 0.007147266896062517)
declare -a momentum=(0.96105 0.94963 0.94356 0.98704 0.91874 0.96877 0.97035 0.99252 0.90395 0.96471 0.96381 0.94617 0.90531 0.90386 0.91695 0.97945 0.91626 0.91387 0.93529 0.94845 0.96977 0.98012 0.92668 0.99095 0.90229 0.93751)
declare -a train_batch_size=(512 1024)
tfhub_module=('https://tfhub.dev/google/imagenet/mobilenet_v2_140_224/classification/3')
bottleneck_dir='../inputs/bottleneck_LUCAS'
image_dir='../inputs/dataset_LUCAS/'
output_graph='output_graph'
intermediate_output_graphs_dir='intermediate_graph/'
output_labels='output_labels/'
summaries_dir='retrain_logs/'
saved_model_dir='export_graph/'
checkpoint_path='retrain_checkpoint/'
mean_test=255
std_test=0

#---------------------NOTES
#tfhub_module='https://tfhub.dev/google/imagenet/mobilenet_v2_100_224/classification/3'
#try with the augmented images also
#try with the proper mean values, per image? per test set? I can only pass a value not per channel


mkdir -p $name_run
#counter
count=$((k++))
for i in `seq 0 $(( ${#learning_rate[@]} - 1 ))`
do 
    for l in "${train_batch_size[@]}"
    do
        if [ ! -f $name_run$output_graph'/'$count'.pb' ]; then
        echo "-----------------------------------------------------------------------------"
        echo $count
        #print the command and save it
        #With gradient
        mkdir -p $name_run'commands/'
        echo python3 retrain.py \
          --image_dir $image_dir \
          --output_graph $name_run$output_graph'/'$count'.pb' \
          --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
          --intermediate_store_frequency $intermediate_store_frequency \
          --output_labels $name_run$output_labels'/'$count'.txt' \
          --summaries_dir $name_run$summaries_dir'/'$count \
          --how_many_training_steps $how_many_training_steps \
          --learning_rate ${learning_rate[i]} \
          --train_batch_size $l \
          --momentum 0.000000 \
          --bottleneck_dir $bottleneck_dir \
          --tfhub_module $tfhub_module \
          --saved_model_dir $name_run$saved_model_dir$count'/' \
          --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
          --Adam 'False' > $name_run'commands/'$count'_infocommand.txt'
        
        #if [ $count -eq 180 ]
        #then 
            #create all the folders needed for this run
            mkdir -p $name_run$output_graph'/'
            mkdir -p $name_run$intermediate_output_graphs_dir$count'/'
            mkdir -p $name_run$output_labels'/'
            mkdir -p $name_run$summaries_dir'/'
            mkdir -p $name_run$saved_model_dir
            mkdir -p $name_run$checkpoint_path$count'/'

            python3 retrain.py \
                --image_dir $image_dir \
                --output_graph $name_run$output_graph'/'$count'.pb' \
                --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
                --intermediate_store_frequency $intermediate_store_frequency \
                --output_labels $name_run$output_labels'/'$count'.txt' \
                --summaries_dir $name_run$summaries_dir'/'$count \
                --how_many_training_steps $how_many_training_steps \
                --learning_rate ${learning_rate[i]} \
                --train_batch_size $l \
                --momentum 0.000000 \
                --bottleneck_dir $bottleneck_dir \
                --tfhub_module $tfhub_module \
                --saved_model_dir $name_run$saved_model_dir$count'/' \
                --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
                --Adam 'False'
            wait
        fi
        count=$((k++))
        if [ ! -f $name_run$output_graph'/'$count'.pb' ]; then

        echo "-----------------------------------------------------------------------------"
        echo $count
        
        
        #with adam
        mkdir -p $name_run'commands/'
        echo python3 retrain.py \
          --image_dir $image_dir \
          --output_graph $name_run$output_graph'/'$count'.pb' \
          --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
          --intermediate_store_frequency $intermediate_store_frequency \
          --output_labels $name_run$output_labels'/'$count'.txt' \
          --summaries_dir $name_run$summaries_dir'/'$count \
          --how_many_training_steps $how_many_training_steps \
          --learning_rate ${learning_rate[i]} \
          --train_batch_size $l \
          --momentum ${momentum[i]} \
          --bottleneck_dir $bottleneck_dir \
          --tfhub_module $tfhub_module \
          --saved_model_dir $name_run$saved_model_dir$count'/' \
          --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
          --Adam 'True' > $name_run'commands/'$count'_infocommand.txt'
        
        #if [ $count -eq 180 ]
        #then 
            #create all the folders needed for this run
            mkdir -p $name_run$output_graph'/'
            mkdir -p $name_run$intermediate_output_graphs_dir$count'/'
            mkdir -p $name_run$output_labels'/'
            mkdir -p $name_run$summaries_dir'/'
            mkdir -p $name_run$saved_model_dir
            mkdir -p $name_run$checkpoint_path$count'/'

            python3 retrain.py \
                --image_dir $image_dir \
                --output_graph $name_run$output_graph'/'$count'.pb' \
                --intermediate_output_graphs_dir $name_run$intermediate_output_graphs_dir$count'/' \
                --intermediate_store_frequency $intermediate_store_frequency \
                --output_labels $name_run$output_labels'/'$count'.txt' \
                --summaries_dir $name_run$summaries_dir'/'$count \
                --how_many_training_steps $how_many_training_steps \
                --learning_rate ${learning_rate[i]} \
                --train_batch_size $l \
                --momentum ${momentum[i]} \
                --bottleneck_dir $bottleneck_dir \
                --tfhub_module $tfhub_module \
                --saved_model_dir $name_run$saved_model_dir$count'/' \
                --checkpoint_path $name_run$checkpoint_path$count'/_retraincheckpoint' \
                --Adam 'True'
            wait
        fi
        count=$((k++))


	done
done

