#cd c:/spark/spark-1.6.1-bin-hadoop2.6/bin
#pyspark --driver-memory 2g --executor-memory 2g c:\spark\spark-1.6.1-bin-hadoop2.6\data\bach\app\BachApp_K-means.py <k>



######################################################### IMPORT USEFULL MODULES
from __future__ import print_function
import sys
import pandas
import matplotlib.pyplot as plt
from pyspark.sql.functions import col
from sklearn.decomposition import PCA as sklearnPCA
from pyspark.sql import SQLContext
from pyspark.sql.types import *
import numpy as np
from pyspark import SparkContext
from pyspark.mllib.clustering import KMeans
from time import time

######################################################### TOOLBOX OF FUNCTIONS
#Binaze Data
def filterOut(row):
    t=row[2:14]
    for i in range(0,len(t)):
        if t[i]=="YES" :
            t[i]=1
        else:
            t[i]=0
    return np.append(t,row[15])

#Function for calculating Intertie
def error(point):
    center = model.clusterCenters[
    model.predict(point)]
    return sum([x**2 for x in (point - center)])

######################################################### MAIN
if __name__ == "__main__":
    #ERREUR SI K NON DEFINI
    if len(sys.argv) != 2:
        print("Usage: k means <k> : number of cluster")
        exit(-1)

    #Define Spark Context
    sc = SparkContext(appName="KMeans")
    #Define SQL Context
    sqlContext = SQLContext(sc)

    #Import data
    donnee = sc.textFile("../data/bach/input/jsbach_chorals_harmony.txt")
    #Select only numericals data
    data = donnee.map(lambda line: np.array([x for x in line.split(',')]))
    #binarise
    data2=data.map(filterOut)
    #to integer
    data3=data2.map(lambda line: np.array([int(x) for x in line]))

    #define number k of cluster
    k = int(sys.argv[1])

    #Lunch k-means 
    t0 = time()
    model = KMeans.train(data2, k, maxIterations=100, runs=10, initializationMode="random")
    tt = time() - t0

    #Find cluster
    clust=model.predict(data2)

  

    #Compute inertie
    Inert = data3.map(lambda point:error(point)).reduce(lambda x, y: x + y)
    #Compute var Intra
    varIntra=Inert/data3.count()
    
    #Save summary file
    f = open("../data/bach/output/K_means.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("Number of cluster =  %s \n" % k)
    f.writelines("Variance intraclasse =  %s \n" % str(varIntra))
    f.writelines("Inertie = %s \n"  % str(Inert))
    f.close()

    #Concat data with defined clust
    concat=data3.zip(clust)
    #Create Df based on schema
    #Find header & create SQL Schema
    t= sc.textFile("../data/bach/input/header.csv")
    header = t.first()
    schemaString = header.replace('"','')  # get rid of the double-quotes
    fields = [StructField(field_name, IntegerType(), True) for field_name in schemaString.split(',')]
    schema = StructType(fields)
    df= concat.map(lambda p: (int(p[0][0]), int(p[0][1]), int(p[0][2]), int(p[0][3]), int(p[0][4]) , int(p[0][5]),
     int(p[0][6]) , int(p[0][7]), int(p[0][8]), int(p[0][9]), int(p[0][10]), int(p[0][11]), int(p[0][12]),int(p[1]))).toDF(schema)
    #to Pandas DF
    df2=df.toPandas()
    #Save cluster
    df2.to_csv("../data/bach/output/BachChoralsClusters.csv")

    #catch only label column
    label=df2["clust"]
    #the DF without labels
    without=df2.drop("clust",1)
    #PCA on 2 dim
    pca = sklearnPCA(n_components=2)
    #2dimensions Data
    data2D = pca.fit_transform(without)
    
    #Plotting 2D data colored by clust  : if k>5 problem !
    colors = ['#581845','#900C3F','#C70039','#FF5733','#FFC300']
    col_map=dict(zip(set(label),colors[0:k]))
    label_color = [col_map[l] for l in label]

    fig, ax = plt.subplots()
    ax.scatter(data2D[:,0], data2D[:,1], c=label_color)
    plt.show()

    sc.stop()
