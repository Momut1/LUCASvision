from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
import pandas as pd
from sklearn.metrics import accuracy_score
import argparse
import numpy as np
import csv


def readcsv_panda(path):

    return pd.read_csv(path)


def extract_metrics(run, tranval):
    count = run[0].split("/")[4]
    #root_path = run[0].split("Results")[0]
    # take the val missclassifications
    aux= tranval[tranval['run'] == int(count)]
    val_acc = aux[aux['set'] =='validation'].Acc.values[0]
    train_acc = aux[aux['set'] == 'train'].Acc.values[0]

    #handle the test results
    test_results = pd.read_csv(run[0], dtype={"cnn_labels": str, "cnn_values": str, "pointidyear": int, "lc1": str}, low_memory = False)
    #test_results['basename'] = test_results["files"].apply(lambda x: x[x.find('/NL'):].strip('\']'))
    # drop the information that is not needed
    test_results.drop(test_results.columns.difference(['cnn_labels', 'cnn_values', 'pointidyear']), 1, inplace=True)
    
    #stuff for capping at 85
    test_results['pointidyear'] = test_results['pointidyear'].apply(str) 
    
    #cap at the 85 images/class - delete this whole block if re-using code
    cappedImgList = pd.read_csv('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/test_set_eos_lucas85.csv')
    cappedImgList = extractPointIDYearFromFilepath(cappedImgList)
    
    if(all(item in list(test_results['pointidyear']) for item in list(cappedImgList['pointidyear'])) == True):
        test_results = test_results[test_results.pointidyear.isin(list(cappedImgList.pointidyear))]
    else:
        print('not all images from cappedImgList are in the test_results!')

    ####### CHANGE 2 - extract cnn_labels_top and cnn_values_top at index 0
    #test_results['cnn_labels_top'] = ""
    #test_results['cnn_values_top'] = ""

    #for roww in range(0,test_results.shape[0]):
     #   test_results['cnn_labels_top'][roww] = test_results["cnn_labels"].apply(eval)[roww][0]
      #  test_results['cnn_values_top'][roww] = test_results["cnn_values"].apply(eval)[roww][0]

    #test_results['cnn_labels_top'] = test_results['cnn_labels_top'].str.upper()
    # join them by basename of results
    gt['pointidyear'] = gt['pointidyear'].apply(str) 
    df_join = pd.merge(test_results, gt, how='left', on="pointidyear")
    
    #control for upper/lower case - handle this in pre-processing and delete lines
    df_join['cnn_labels'] = df_join['cnn_labels'].str.upper()
    
    #print('****************')
    #print(df_join.head())
    #print(df_join['cnn_labels'])
    #print('****************')
    
    # extract confusion matrix
    gt_aux = df_join['lc1'].to_numpy()
    p = df_join['cnn_labels'].to_numpy()
    test_acc = accuracy_score(gt_aux, p)
    #do the metrics for the aggregation values
    return val_acc, train_acc, test_acc

def extract_hyperparames(run):
    count = run[0].split("/")[4]
    root_path = run[0].split("Results")[0]
    # take the params
    params = open(root_path + "commands/" + count + "_infocommand.txt", 'r')
    params = params.read()

    # Augmentation flag
    if (params.find("flip") > 0):
        augment = "Augmetations"
    else:
        augment = "No augmetations"

    LR = params.split('--learning_rate ')[1].split(' ')[0]
    BS = params.split('--train_batch_size ')[1].split(' ')[0]

    # optimizer flag
    momentum = params.split('--momentum ')[1].split(' ')[0]
    if momentum == '0.000000':
        optimizer = "Gradient Descent"
    else:
        optimizer = "Adam"

    if "mobilenet" in params.split('--tfhub_module https://tfhub.dev/google/imagenet/')[1].split('_'):
        model = "mobilenet"
        n_layers = params.split('--tfhub_module https://tfhub.dev/google/imagenet/')[1].split('_')[2]
        input_s = params.split('--tfhub_module https://tfhub.dev/google/imagenet/')[1].split('_')[3].split("/")[0]
    else:
        model = "Inception"
        input_s = 299
        n_layers = 0

    return augment, LR, BS, momentum, optimizer

def overall_stats(results_path, resultstrainval_path, output_full_path):
    runs = csv.reader(open(results_path))
    tranval =  readcsv_panda(resultstrainval_path)
    col_names = ['count', 'augment', 'LR', 'BS', 'momentum', 'optimizer', 'val_acc', 'train_acc', 'test_acc']
    my_df = pd.DataFrame(columns=col_names)
    for run in runs:
        count = run[0].split("/")[4]
        #return [augment, LR. BS, momentum, optimizer]
        augment, LR, BS, momentum, optimizer = extract_hyperparames(run)

        #return val_acc, train_acc, test_acc, trash_count, bso_count
        val_acc, train_acc, test_acc = extract_metrics(run, tranval)
        add_row_stats(my_df,count, augment, LR, BS, momentum, optimizer, val_acc, train_acc, test_acc)

    my_df.to_csv(output_full_path)



def add_row_stats(my_df,count,augment, LR, BS, momentum, optimizer, val_acc, train_acc, test_acc):
    new_row = [count, augment, LR, BS, momentum, optimizer, val_acc, train_acc, test_acc]
    my_df.loc[len(my_df)] = new_row
    
def extractPointIDYearFromFilepath(df):
    df['pointidyear'] = ''
    for i in range(0, df.shape[0]):
        #extract the filepath of a single image
        filepatheos_i = df.iloc[i, 0]

        #extract the filename and poitnID
        filename_i = filepatheos_i.split('/')[-1]
        pointID = filename_i.split('_')[1]

        #extract the year
        year = filename_i.split('_')[0].replace('LUCAS', '')

        #contatenate both
        df['pointidyear'][i] = pointID + year
        
    #df['pointidyear'].apply(int)  
    return(df)



if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("--results_path", help="Path to txt to the image list to be processed")
    parser.add_argument("--resultstrainval_path", help="Path to txt to the image list to be processed")
    parser.add_argument("--output_fullpath", help="Path to save the data")
    parser.add_argument("--save", help="save the parallel plot?", default='/data/results')
    parser.add_argument("--gt_files", help="csv files with the gt of the files to process the GT has to have code as a key")
    args = parser.parse_args()
    if args.results_path:
        results_path = args.results_path
    if args.output_fullpath:
        output_fullpath = args.output_fullpath
    if args.save:
        save = args.save
    if args.gt_files:
        gt_path = args.gt_files
    if args.resultstrainval_path:
        resultstrainval_path = args.resultstrainval_path

    #parser not working - change this line to parsed argument (gt_path)
    gt = readcsv_panda('/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/inputs/gt_LUCAS.csv')

    # create a column with the basename of the files
    #gt['basename'] = gt["name"].apply(lambda x: x[x.find('/NL'):])

    # drop the information that is not needed
    gt.drop(gt.columns.difference(['lc1', 'pointidyear']), 1, inplace=True)
    
    #why isnt the parser working? delete this line
    resultstrainval_path = '/eos/jeodpp/data/projects/REFOCUS/data/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/resume-train.csv'

    overall_stats(results_path, resultstrainval_path, output_fullpath)


