import pandas as pd
import plotly.express as px
import glob
inpath = '../outputs/Best_model_LUCAS/retrain_logs/'
missfiles = [f for f in glob.glob(inpath + "**.csv", recursive=False)]

col_names = ['image', 'class_pred', 'gt', 'count']
my_df = pd.DataFrame(columns=col_names)
for miss in missfiles:
    print(miss)
    aux = miss.split("/")[4]
    count = aux.split("missclass")[0]
    df = pd.read_csv(miss, index_col=0)
    df['count'] = count
    my_df = my_df.append(df)
#map the ints to class names
mymap = {0: 'bso0', 1:'car1', 2:'car4', 3:'gma2', 4:'gra1', 5:'mai1', 6:'mai3', 7:'mai7', 8:'oni1', 9:'oni4', 10:'oni48', 11:'pot1', 12:'pot6', 13:'pot8', 14:'pot9', 15:'sbt14', 16:'sbt39', 17:'scr2',
         18:'scr3', 19:'tsh0', 20:'veg1', 21:'wwh2', 22:'wwh3', 23:'wwh7', 24:'wwh9'}
my_df['color'] = my_df["gt"]
my_df.replace({"gt": mymap}, inplace=True)
my_df.replace({"class_pred": mymap}, inplace=True)
my_df.to_csv('../outputs/Random_search_BBCH/missclass.csv')

fig = px.parallel_categories(my_df, color='color', dimensions = ['gt', 'class_pred'], color_continuous_scale=px.colors.qualitative.Plotly)
fig.show()
