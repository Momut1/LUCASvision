import pandas as pd
#from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import plotly.express as px

out = '../outputs/Random_search_LUCAS/'
df = pd.read_csv('../outputs/Random_search_LUCAS/Results.csv')

#df = df[df['set'] == 'validation']

#
fig, axs = plt.subplots()
# fig.subplots_adjust(hspace=0.5)
# fig.suptitle('Validation set')

#Adam
adam = df[df['momentum'] != 0]
y_points = adam['momentum']
x_points = adam['LR']
markers_points = adam['BS']
fig = px.scatter(adam, x='LR', y='momentum', color='train_acc', size='BS')
fig.update_layout(xaxis_type="log")
fig.show()

#Adam
adam = df[df['momentum'] != 0]
y_points = adam['momentum']
x_points = adam['LR']
markers_points = adam['BS']
fig = px.scatter(adam, x='LR', y='momentum', color='val_acc', size='BS')
fig.update_layout(xaxis_type="log")
fig.show()

#Adam
adam = df[df['momentum'] != 0]
y_points = adam['momentum']
x_points = adam['LR']
markers_points = adam['BS']
fig = px.scatter(adam, x='LR', y='momentum', color='test_acc', size='BS')
fig.update_layout(xaxis_type="log")
fig.show()

#GD
gd = df[df['momentum'] == 0]
s = axs[1].scatter(gd.BS, gd.LR, c=gd.Acc, cmap = 'hsv', vmin = 0.5, vmax = 1)
cb = plt.colorbar(s)
cb.set_label('Accuracy')
axs[1].set_ylim([0.0001,1])
axs[1].set_title('Gradient Descent')
axs[1].set_xlabel('BS')
axs[1].set_ylabel('Learning rate')
axs[1].set_xscale('log')

plt.show()
for i in df.BS.unique():
    aux = df[df.BS == i]
    title = "Validation set, Batch size: {}".format(i)
    fig = plt.figure()
    ax = fig.add_subplot(111)
    s = ax.scatter(df.momentum, df.LR, c=df.Acc, cmap = 'plasma', vmin = 0.5, vmax = 1)
    cb = plt.colorbar(s)
    cb.set_label('Accuracy')
    ax.set_ylim([0.0001,1])
    ax.set_title(title)
    ax.set_xlabel('Momentum')
    ax.set_ylabel('Learning rate')

    ax.set_xscale('log')
    outname = "{}BS-{}-validation_randomgrid.png".format(out, i)
    fig.savefig(outname)  
    
    
df = pd.read_csv('../outputs/Random_search_LUCAS/resume-train.csv')

df = df[df['set'] == 'train']
    
for i in df.BS.unique():
    aux = df[df.BS == i]
    title = "Train set, Batch size: {}".format(i)
    fig = plt.figure()
    ax = fig.add_subplot(111)
    s = ax.scatter(df.momentum, df.LR, c=df.Acc, cmap = 'plasma', vmin = 0.5, vmax = 1)
    cb = plt.colorbar(s)
    cb.set_label('Accuracy')
    ax.set_ylim([0.0001,1])
    ax.set_title(title)
    ax.set_xlabel('Momentum')
    ax.set_ylabel('Learning rate')
    ax.set_xscale('log')
    outname = "{}BS-{}-train_randomgrid.png".format(out, i)
    fig.savefig(outname)    
    
    

    
