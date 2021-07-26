#cd c:/spark/spark-1.6.1-bin-hadoop2.6/bin
#pyspark --driver-memory 2g --executor-memory 2g 



######################################################### IMPORT USEFULL MODULES
import pandas
import csv
from pyspark.mllib.util import MLUtils
from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext
from pyspark.ml.feature import VectorAssembler
from pyspark.sql.types import *
from pyspark.mllib.regression import LabeledPoint
from pyspark.sql.functions import col
from time import time
from pyspark.mllib.tree import GradientBoostedTrees, GradientBoostedTreesModel

######################################################### TOOLBOX OF FUNCTIONS
#For export in csv format
def Savecsv(LabeledPoint,yourpath):
    df1 = sqlContext.createDataFrame(LabeledPoint)
    df2 = df1.toPandas()
    df2.to_csv(yourpath)

#For export in json format
def Savejson(LabeledPoint,yourpath):
    df1 = sqlContext.createDataFrame(LabeledPoint)
    df2 = df1.toPandas()
    df2.to_json(yourpath)

#Transform rdd to SQL dataframe
def toDataframe(data,schema):
    #CATCH HEADER
    df= data.map(lambda k: k.split(",")).map(lambda p: (int(p[0]), 
        int(p[1]), int(p[2]), int(p[3]), int(p[4]) , int(p[5]), int(p[6]) , int(p[7]), int(p[8]),
         int(p[9]), int(p[10]))).toDF(schema)
    return df

#Transform Sql DF to LabeledPoint format
def toLabeledPoint(df):
    features= df.map(lambda row: row[0:10])
    lab= df.map(lambda row: row[10])
    transformedData = lab.zip(features)
    Lp = transformedData.map(lambda row: LabeledPoint(row[0],row[1]))
    return Lp

#Binarize Y
def filterOut(lab):
    if lab >=1 :
            lab=1
    else:
            lab=0
    return int(lab)

#Transform Sql DF to LabeledPoint format and binarize
def toLabeledPoint2(df):
    features= df.map(lambda row: row[0:10])
    lab= df.map(lambda row: row[10])
    lab2=lab.map(filterOut)
    transformedData = lab2.zip(features)
    Lp=transformedData.map(lambda row: LabeledPoint(row[0],row[1]))
    return Lp

######################################################### MAIN

if __name__ == "__main__":
    print("########################################################################### BEGIN")
    #Define Context
    sc = SparkContext("local","PokerApp_GradientBoosting")
    sqlContext = SQLContext(sc)

    #Import header and create SQL schema
    t= sc.textFile("../data/poker/input/header.csv")
    header = t.first()
    schemaString = header.replace('"','')  # get rid of the double-quotes
    #FORMATING DATA
    fields = [StructField(field_name, IntegerType(), True) for field_name in schemaString.split(',')]
    #ifyou wanna change type : fields[10].dataType = FloatType()
    schema = StructType(fields)  

    print("########################################################################### IMPORT DATA")
    #import train and test data from csv
    train=sc.textFile("../data/poker/input/poker-train.txt")
    test= sc.textFile("../data/poker/input/poker-test.txt")
    #Tranform data to SQL DF and print schema
    train_df= toDataframe(train,schema)
    train_df.printSchema()
    test_df= toDataframe(test,schema)
    test_df.printSchema()

    #Transform to Labeled Points and Binarize data
    train=toLabeledPoint2(train_df)
    test=toLabeledPoint2(test_df)

    #Transform to Labeled Points for forecast 0-9
    train2=toLabeledPoint(train_df)
    test2=toLabeledPoint(test_df)

    print("###########################################################################BINARY STUMP GRADIENT BOOSTING")
    #First model : binarized data on stump tree
    t0 = time()
    GT1 = GradientBoostedTrees.trainClassifier(train,categoricalFeaturesInfo={},numIterations=150,maxBins=32,maxDepth=1)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  GT1.predict(test.map(lambda x: x.features))
    labelsAndPredictions = test.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/BinaryStumpGradientBoostingPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/BinaryStumpGradientBoosting.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("----------TREE-------- \n")
    f.writelines("Num nodes total = %s \n" % GT1.totalNumNodes())
    f.writelines("NumTrees = %s \n" %  GT1.numTrees())
    f.writelines("----------------------\n")
    f.writelines(GT1.toDebugString())
    f.close()

    print("###########################################################################BINARY 8Depth GRADIENT BOOSTING")
    #Second model : binarized data on trees
    t0 = time()
    GT2 = GradientBoostedTrees.trainClassifier(train,categoricalFeaturesInfo={},numIterations=150,maxBins=32,maxDepth=8)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  GT2.predict(test.map(lambda x: x.features))
    labelsAndPredictions = test.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/BinaryDepth8GradientBoostingPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/BinaryDepth8GradientBoosting.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("----------TREE-------- \n")
    f.writelines("Num nodes total = %s \n" % GT2.totalNumNodes())
    f.writelines("NumTrees = %s \n" %  GT2.numTrees())
    f.writelines("----------------------\n")
    f.writelines(GT2.toDebugString())
    f.close()
    
    print("###########################################################################0-9 STUMP GRADIENT BOOSTING")
    #Second model : 0-9 data 
    t0 = time()
    GT3 = GradientBoostedTrees.trainClassifier(train2,categoricalFeaturesInfo={},numIterations=150,maxBins=32,maxDepth=1)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  GT3.predict(test2.map(lambda x: x.features))
    labelsAndPredictions = test2.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/0-9_Stump_GradientBoostingPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test2.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/0-9_Stump_GradientBoosting.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("----------TREE-------- \n")
    f.writelines("Num nodes total = %s \n" % GT3.totalNumNodes())
    f.writelines("NumTrees = %s \n" %  GT3.numTrees())
    f.writelines("----------------------\n")
    f.writelines(GT3.toDebugString())
    f.close()

    print("###########################################################################0-9 8Depth GRADIENT BOOSTING")
    #Second model : 0-9 data 
    t0 = time()
    GT4 = GradientBoostedTrees.trainClassifier(train2,categoricalFeaturesInfo={},numIterations=150,maxBins=32,maxDepth=8)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  GT4.predict(test2.map(lambda x: x.features))
    labelsAndPredictions = test2.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/0-9_8Depth_GradientBoostingPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test2.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/0-9_8Depth_GradientBoosting.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("----------TREE-------- \n")
    f.writelines("Num nodes total = %s \n" % GT4.totalNumNodes())
    f.writelines("NumTrees = %s \n" %  GT4.numTrees())
    f.writelines("----------------------\n")
    f.writelines(GT4.toDebugString())
    f.close()
    
    sc.stop()