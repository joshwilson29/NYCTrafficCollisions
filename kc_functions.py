import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
import os
from scipy.spatial import ConvexHull
from scipy.spatial import Delaunay
from scipy.spatial.qhull import QhullError
from scipy.stats import ttest_ind
import hdbscan
from bokeh.sampledata import us_states
import bokeh.models as bmo
from bokeh.palettes import d3, viridis
from bokeh.plotting import figure,output_notebook,ColumnDataSource,show
from bokeh.layouts import column,row,gridplot
import re

LOCS=['longitud','latitude']

# Data Clean up 

def cleanLocs(dftmp):
    return dftmp.loc[(dftmp.latitude<=77) & (dftmp.longitud<=999),:]

def getalldata(filename='accident.csv'):
    years=range(2015,2018)
    dftmp=pd.read_csv('data/2014/'+filename,encoding='latin-1')
    dftmp['id']=dftmp.ST_CASE.apply(lambda x: "2014."+str(int(x)))
    for i in years:
        dfz=pd.read_csv('data/'+str(i)+'/'+filename,encoding='latin-1')
        dfz['id']=dfz.ST_CASE.apply(lambda x: str(i) + "." + str(int(x)))
        dftmp=dftmp.append(dfz,sort=False) 
    dftmp.columns=[ i.lower() for i in dftmp.columns.tolist()]
    # make sure all the indices are unique
    dftmp=dftmp.reset_index().drop('index',axis=1)
    return dftmp

# Please ignore this function for now - need to be edited to figure out how we'll deal with multiple columns
def mergedata(dfmain,filename='distract.csv',columns=[]):
    years=range(2015,2018)
    dftmp=pd.read_csv('data/2014/'+filename.lower())
    dftmp.columns=[i.lower for i in dftmp.columns.tolist()]
    dftmp['id']=dftmp.ST_CASE.apply(lambda x: "2014."+str(int(x)))
    for i in years:
        dfz=pd.read_csv('data/'+str(i)+'/'+filename)
        dfz['id']=dfz.ST_CASE.apply(lambda x: str(i) + "." + str(int(x)))
        dftmp=dftmp.append(dfz)
    dfmain=dfmain.set_index('id')
    dftmp=dftmp.set_index('id')
    return dfmain.merge(dftmp)

def cyclic(dftmp,columns=['ARR_HOUR'],periods=[24]):
    for c,p in zip(columns,periods):
        tgt=dftmp[c]
        if min(tgt)!=0:
            tgt=tgt-min(tgt)
        dftmp[c+'_sin_cycle']=np.sin(2*np.pi*tgt/p)
        dftmp[c+'_cos_cycle']=np.cos(2*np.pi*tgt/p)
        
def removeNoHourAndMinutes(dforig):
    dftmp=dforig.copy()
    crit=(dftmp.hour<24) & (dftmp.minute<60)
    dftmp=dftmp.loc[crit,:]
    return dftmp

def createTimestamp(dforig):
    dftmp=dforig.copy()
    dftmp['tstamp']=[pd.Timestamp(year=v.year,month=v.month,day=v.day,hour=v.hour,minute=v.minute) for i,v in dftmp.iterrows()]
    return dftmp
    
def removeUneededColumns(dforig,filename='accident.csv'):
    '''
    Removes uneeded columns from the dataframe accoding to a specified file. The function used to get the columns is getVarlist. filename should be the data filename to filter.
    See getVarlist() to see how it gets the list of variables for each data file from the data specification file.

    Returns dataframe with the specified variables
    '''
    dftmp=dforig.copy()
    key=filename.split('.')[0]
    v=getVarlist()
    return dftmp.loc[:,v[key]]

def calculateTopNCatPct(dforig,N=5):
    '''
    calculates top N categories
    '''
    l=[]
    DO_NOT_CALC=['id','tstamp','longitud','latitude','day','month','year','day_week','hour','minute']
    for i in dforig.columns.tolist():
        if i not in DO_NOT_CALC:
            l.append([i,(dforig[i].value_counts()/dforig.shape[0])[0:N].sum()])
    return pd.DataFrame(l,columns=['variable','percent'])

def binarizeVariables(dforig,variable_list=['peds','route'],TopN=5):
    '''
    binarizes variables
    '''
    dftmp=dforig.copy()
    for col in variable_list:
        for value in dftmp[col].value_counts().index[0:TopN].tolist():
            dftmp[col+'_'+str(value)]= dftmp[col]==value
    return dftmp

# Clustering Functions

class convexhull(ConvexHull):
    def in_hull(self,p):
        """
        Test if points in `p` are in `hull`

        `p` should be a `NxK` coordinates of `N` points in `K` dimensions
        `hull` is either a scipy.spatial.Delaunay object or the `MxK` array of the 
        coordinates of `M` points in `K`dimensions for which Delaunay triangulation
        will be computed
        """
        hull=Delaunay(self.points)

        return hull.find_simplex(p)>=0

def cluster_all_points(dfsrc,filter_rows,LongitudeLatitude=LOCS,hdbscan_params={'min_cluster_size':30,'gen_min_span_tree':True, 'metric':'manhattan','min_samples':30}):
    '''
    This function takes a table with longitude, latitude, clusters them using hdbscan, then generate the cluster boundaries
    using a convex hull and label all points in the convex hull to the appropriate cluster. Note that cluster = -1 indicates that it is not
    in a cluster. The returned dataframe will have 1 additonal field - 'cluster', indicating the cluster it is in.
    
    dfsrc - the pandas datframe will all the information
    filter_rows- a boolean list that should have the same number of data points indicating which rows are considered "Active"
                (ie. if in the dataframe of dfsrc, if a row is considered "on", then it should be True, else False)
    LongitudeLatitude - list of [longitude, latitude] parameter
    hdbscan - the list of parametrs for hdbscan
    
    Example - dfret=cluster_all_points(df,df.WK_ZONE==1)
    '''
    import hdbscan
    dftmp=dfsrc.copy()
    dftgt=dftmp.loc[filter_rows,LongitudeLatitude]
    clusterer = hdbscan.HDBSCAN(**hdbscan_params)
    clusterer.fit(dftgt)
    dftgt['cluster']=clusterer.labels_+1
    dftmp['cluster']=0
    dftmp.loc[list(dftgt.index),'cluster']=dftgt['cluster']
    # Note that at this point, only the points found to be in a cluster and the filter_row == TRUE have an active cluster number, not we need to find
    # clusters for points which have filter_row == FALSE but is inside the cluster. We do this using a convex hull.
    for i in dftmp['cluster'].unique():
        if i!=0:
            dftgt=dftmp.loc[dftmp['cluster']==i,LongitudeLatitude]
            try:
                hull=convexhull(dftgt)
            except Exception as Err:
                if type(Err) == QhullError:
                    # Can't form a hull because there's less than 3 points
                    if len(dftgt.longitud.unique()) < 3 and len(dftgt.latitude.unique()) < 3:
                        for lon,lat in zip (dftgt.longitud.unique(),dftgt.latitude.unique()):
                            dftmp.loc[(dftmp.longitud==lon) & (dftmp.latitude==lat),'cluster']=i
                    else:
                        print(f'Cluster {i}: QHULL ERROR NOT ONLY 1 point - returning dataframe')
                        return(dftgt)
                else:
                    print(f'Cluster {i}: Error - Returning dataframe. Error {Err.args}')
                    return(dftgt)
            hull.close()
            dftmp.loc[hull.in_hull(dftmp.loc[:,LongitudeLatitude].values),'cluster']=i
    # Now, all the points of the dataframe are labeled
    return dftmp

def ttest(dfsrc,label,clusterfield='cluster',notinclusternumber=0):
    '''
    This function takes a pandas dataframe with at least 1 field - clusterfield (the field indicating the cluster which the
    data point is in). The it performs a ttest for significance for each cluster against data in non-clusters and returns
    the tstat and pvalue for the test in addtional fields. For each cluster, it will have the same tstat and pvalue. Note that
    the cluster is only tested again points not in cluster (ie. cluster = -1). The worries about validity of the t-test if some
    data points are int the significance range should be assuaged by the argument that if a given cluster is deemed insignificant,
    then the cluster shouldn't be different mean the non-clustered; therefore, the test against a sample of the total population
    is still valid (similar to bootstrapping...)

    dfsrc - pandas dataframe
    label - list of booleans - should be the same length as dfsrc - it labels the data points to be true or not true
    clusterfield - the field parameter to use if the cluster field is not cluster
    notinclusternumber - the cluster number to use to indicate in the clusterfield that it's not in a cluster
    
    Example: ttest(df,df.WRK_ZONE==1)
    '''
    dftmp=dfsrc.copy()
    dftmp['label']=label
    dfnotincluster=dftmp.loc[dftmp[clusterfield]==notinclusternumber,'label']
    dftmp['tstat']=-1
    dftmp['pvalue']=-1
    for i in dftmp[clusterfield].unique():
        if i!=notinclusternumber:
            dfcluster=dftmp.loc[dftmp[clusterfield]==i,'label']
            tstat,pvalue=ttest_ind(dfcluster,dfnotincluster)
            dftmp.loc[dftmp[clusterfield]==i,['tstat']]=tstat
            dftmp.loc[dftmp[clusterfield]==i,['pvalue']]=pvalue
    return dftmp

def signficant_clusters(dfsrc,adjust_clusters=True, alpha=0.05):
    '''
    The function returns a dataframe with the field 'pvalue'. It returns a dataframe with a boolean field 'b_sig' indicating if it's significant for not.
    The function adjusts for bonferroni correction.
    
    dfsrc - the dataframe
    alpha - alpha level
    
    '''
    dftmp=dfsrc.copy()
    dftmp['sig']=dftmp['pvalue']<=alpha
    if adjust_clusters:
        dftmp['cluster']=dftmp['cluster']*dftmp['sig']
    return dftmp

# Functions for merging

def UniqueCases(dforig,varlist=['st_case'],column='id'):
    '''
    returns the number of unique cases
    '''
    return len(dforig[column].unique())==dforig.shape[0],max(dforig.groupby(column)[varlist].count().max())
# plotting map

def plot_map(dfsrc,color='navy',ON_points=False,title="US Scatter Map"):
    '''
    plots the accident points according to 'longitud' and 'latitude' into a map
    If color parameter is not a list, it's just 1 color, otherwise, it should inidicate clusters for each data point.
    ON_points is a list of points where the a circular outline will be drawn regardless of the clusters - used for indicating "ON" points
    '''
   
    dftmp=dfsrc.copy()
   
    if type(color) is not str:
        dftmp['cluster']=[str(i) for i in color]
        

    us_st = us_states.data.copy()

    # del us_states["HI"]
    # del us_states["AK"]

    # separate latitude and longitude points for the borders
    #   of the states.
    state_xs = [us_st[code]["lons"] for code in us_st]
    state_ys = [us_st[code]["lats"] for code in us_st]

    # init figure
    p = figure(title=title, 
            toolbar_location="left", plot_width=1100, plot_height=700, x_range=(-130,-65),y_range=(22,53)) #, tooltips=[('Long','@longitud'),('Lat','@latitude'),('Status','@ON')])

    # Draw state lines
    p.patches(state_xs, state_ys, fill_alpha=0.0,
        line_color="#884444", line_width=1.5)

    params=dict(x='longitud',y='latitude',legend='cluster',alpha=0.3)
    if len(dftmp['cluster'].unique())==1:
        color='navy'
    else:
        dftmp['alpha']=np.where(dftmp.cluster==0,0.2,1)
        params['alpha']='alpha'
 
    if type(ON_points) is not bool:
        dftmp['ON']=ON_points
        params['line_color']='ON'
        params['line_alpha']='ON'

    source = ColumnDataSource(dftmp)

    params['source']=source

    # The scatter markers
    if type(color) is not str:
        if len(dftmp['cluster'].unique())<20:
            palette = d3['Category20'][len(dftmp['cluster'].unique())]
        else:
            palette = viridis(len(dftmp['cluster'].unique()))
        color_map = bmo.CategoricalColorMapper(factors=dftmp['cluster'].unique(), palette=palette)
        p.circle(**params,color={'field':'cluster','transform':color_map})
    else:
        p.circle(**params,color=color)
    show(p)
    
# EDA Functions
def getVarlist(filename='ListofVariables.xlsx',columns='Variables'):
    '''
    reads the list of data files to load from the filename and loads the variables wanted as indicated from filename into a dictionary

    Returns - dictionary with filenames as the key ('accident','cevent',etc) and the list of variables wanted from that dat file
    '''
    dftmp=pd.read_excel('ListofVariables.xlsx', sheet_name='Variables')
    varlist={}
    for i in dftmp['CSV Name'].unique():
        varlist[i.lower()]=[i.lower() for i in dftmp.loc[dftmp['CSV Name']==i,'Variable'].tolist()]
        varlist[i.lower()].append('id')
    return varlist

def createHistograms(filename='accident.csv'):
    '''
    Creates histogram plots for the file according the the variables pulled from the getVarlist() function

    Returns - nothing
    '''
    dftmp=getalldata(filename)
    v=getVarlist()
    dftmp=dftmp.loc[:,v[filename.split('.')[0]]]
    l=[]
    for i in dftmp.columns.tolist():
        if (i!='latitude') and (i!='longitud') and (i!='id') and (i!='veh_no') and (i!='per_no') and (i!='evennum'):
            dfcounts=dftmp[i].value_counts().reset_index()
            dfcounts['pct']=(dfcounts[i]/dfcounts[i].sum())
            src=ColumnDataSource(dfcounts)
            p=figure(tooltips=[(i,'@index'),('count','@'+i),('%','@pct{0.00%}')], title=i)
            p.vbar(x='index',top=i,width=0.8,source=src)
            p.yaxis.axis_label='Count'
            p.xaxis.axis_label=i
            l.append(p)
    show(column(l))
    return None

