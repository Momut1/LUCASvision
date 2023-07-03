from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
import pandas as pd
import matplotlib.pyplot as plt
import itertools
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
import numpy as np
import argparse


def save_conf_matrix(y_true, y_pred, rpt,
                     classes,
                     name,
                     normalize=False,
                     title='Confusion matrix',
                     cmap=plt.get_cmap('RdYlGn')):
    """
    Mostly stolen from: http://scikit-learn.org/stable/auto_examples/model_selection/plot_confusion_matrix.html#sphx-glr-auto-examples-model-selection-plot-confusion-matrix-py

    Normalization changed, classification_report stats added below plot
    """

    cm = confusion_matrix(y_true, y_pred)
    print(cm)

    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title, fontsize=14)
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)
    plt.ylabel('True label', fontsize=8)
    plt.xlabel('Predicted label', fontsize=8)

    # Calculate normalized values (so all cells sum to 1) if desired
    if normalize:
        cm = np.round(cm.astype('float') / cm.sum(), 2)

    # Place Numbers as Text on Confusion Matrix Plot
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, cm[i, j],
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black",
                 fontsize=8)

    # Add Precision, Recall, F-1 Score as Captions Below Plot
    rpt = rpt.replace('avg / total', '      avg')
    rpt = rpt.replace('support', 'N Obs')

    plt.annotate(rpt,
                 xy=(0, 0),
                 xytext=(-50, -700),
                 xycoords='axes fraction', textcoords='offset points',
                 fontsize=8, ha='left')

    # Plot
    plt.tight_layout()
    plt.savefig(name + '.png')


def readcsv_panda(path):
    return pd.read_csv(path)


if __name__ == "__main__":
    # results_path = '/data/Flevo_v2/cnn_output_data.csv'
    # output_fullpath = '/data/Flevo_v2/cm_cnn_output_data'
    parser = argparse.ArgumentParser()
    parser.add_argument("--results_path", help="image list to be processed")
    parser.add_argument("--output_fullpath", help="graph/model to be executed")
    parser.add_argument("--gt_files", help="csv files with the gt of the files to process the GT has to have code as a key")
    parser.add_argument("--BBCH", help="name of file with all the GT infoIts BBCH classification, binary")
    args = parser.parse_args()
    if args.results_path:
        results_path = args.results_path
    if args.output_fullpath:
        output_fullpath = args.output_fullpath
    if args.gt_files:
        gt_path = args.gt_files
    if args.gt_files:
        BBCH = args.BBCH

    gt = readcsv_panda(gt_path)
    results = readcsv_panda(results_path)
    # create a column with the basename of the files
    gt['basename'] = gt["name"].apply(lambda x: x[x.find('/NL'):])

    # drop the information that is not needed
    #diffeent code for BBCH and crop
    gt.drop(gt.columns.difference(['code', 'basename']), 1, inplace=True)
    results.drop(results.columns.difference(['cnn_labels', 'cnn_values', 'basename']), 1, inplace=True)
    if BBCH == '1':
        #erase empty code columns
        gt['code'].replace('', np.nan, inplace=True)
        gt.dropna(subset=['code'], inplace=True)
        trash_count = len(results[results.cnn_labels == 'tsh0'])
        bso_count = len(results[results.cnn_labels == 'bso0'])
        results = results[results.cnn_labels != 'tsh0']
        results = results[results.cnn_labels != 'bso0']
        results['cnn_labels'] = results['cnn_labels'].str.upper()
    else:
        trash_count = len(results[results.cnn_labels == 'tsh'])
        bso_count = len(results[results.cnn_labels == 'bso'])
        results = results[results.cnn_labels != 'tsh']
        results = results[results.cnn_labels != 'bso']
        results['cnn_labels'] = results['cnn_labels'].str.upper()

    # join them by basename of results
    df_join = pd.merge(results, gt, how='left', on="basename")
    index = df_join.loc[pd.isna(df_join["code"]), :].index

    df_join = df_join.drop(index)
    # extract confusion matrix
    gt = df_join['code'].to_numpy()
    p = df_join['cnn_labels'].to_numpy()
    lab = df_join['code'].unique()
    rpt = classification_report(gt, p)
    save_conf_matrix(gt, p, rpt, lab, output_fullpath)
    # df_confusion = pd.crosstab(df_join['code'], df_join['cnn_labels'], rownames=['Actual'], colnames=['Predicted'], margins=True)
