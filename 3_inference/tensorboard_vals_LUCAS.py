import tensorflow as tf
import glob
import pandas as pd	
import os
from tensorboard.backend.event_processing.event_accumulator import EventAccumulator

def extract_stats_trainval(inpath, outpath):
    # This example supposes that the events file contains summaries with a
    # summary value tag 'loss'.  These could have been added by calling
    # `add_summary()`, passing the output of a scalar summary op created with
    # with: `tf.scalar_summary(['loss'], loss_tensor)`.
    events = [f for f in glob.glob(inpath + "**/**/event*", recursive=False)]
    COLUMN_NAMES = ['run', 'set', 'CE', 'Acc', 'LR', 'momentum', 'BS', 'step']
    df = pd.DataFrame(columns=COLUMN_NAMES)
    for ev in events:
        print(ev)
        run = ev.split('/')[4]
        set = ev.split('/')[5]
        #find the LR and BS of this run
        aux_path = os.path.dirname(os.path.dirname(inpath))+'/commands/' + run + '_infocommand.txt'
        params = open(aux_path, 'r')
        params = params.read()
        LR = params.split('--learning_rate ')[1].split(' ')[0]
        BS = params.split('--train_batch_size ')[1].split(' ')[0]
        momentum = params.split('--momentum ')[1].split(' ')[0]
        event_acc = EventAccumulator(ev)
        event_acc.Reload()
        CE = event_acc.Scalars('cross_entropy_1')[-1].value
        Acc = event_acc.Scalars('accuracy_1')[-1].value
        step = event_acc.Scalars('accuracy_1')[-1].step
        df.loc[len(df)] = [run, set, CE, Acc, LR,momentum, BS, step]


    df.to_csv(outpath + 'resume-train.csv')

inp = '../outputs/Random_search_LUCAS/retrain_logs/'
out = '../outputs/Random_search_LUCAS/'

extract_stats_trainval(inp, out)
