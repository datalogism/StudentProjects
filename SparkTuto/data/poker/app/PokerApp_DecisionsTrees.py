#cd c:/spark/spark-1.6.1-bin-hadoop2.6/bin
#pyspark --driver-memory 2g --executor-memory 2g c:\spark\spark-1.6.1-bin-hadoop2.6\data\poker\app\PokerApp_DecisionsTrees.py

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
from pyspark.mllib.tree import DecisionTree, DecisionTreeModel

#For filter only if hand have a value
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
    sc = SparkContext("local","PythonDecisionTreeClassificationExample")
    print("########################################################################### BEGIN")
    sqlContext = SQLContext(sc)

    t= sc.textFile("../data/poker/input/header.csv")
    header = t.first()
    schemaString = header.replace('"','')  # get rid of the double-quotes
    #FORMATING DATA
    fields = [StructField(field_name, IntegerType(), True) for field_name in schemaString.split(',')]
    #ifyou wanna change type : fields[10].dataType = FloatType()
    schema = StructType(fields)  

    print("########################################################################### IMPORT")

    train=sc.textFile("../data/poker/input/poker-train.txt")
    test= sc.textFile("../data/poker/input/poker-test.txt")

    train_df= toDataframe(train,schema)
    train_df.printSchema()
    test_df= toDataframe(test,schema)
    test_df.printSchema()

    train=toLabeledPoint2(train_df)
    test=toLabeledPoint2(test_df)
    train2=toLabeledPoint(train_df)
    test2=toLabeledPoint(test_df)

    print("###########################################################################BINARY DECISION TREE")

    t0 = time()
    model = DecisionTree.trainClassifier(train,categoricalFeaturesInfo={},impurity='gini',numClasses=2, maxDepth=8, maxBins=32)
    tt = time() - t0

    predictions =  model.predict(test.map(lambda x: x.features))
    labelsAndPredictions = test.map(lambda lp: lp.label).zip(predictions)

    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test.count())

    print(testErr)
    Savecsv(labelsAndPredictions,"../data/poker/output/BinaryDecisionTreePredictions.csv")

    f = open("../data/poker/output/BinaryDecisionTree.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("----------TREE-------- \n")
    f.writelines("Num nodes = %s \n" % model.numNodes())
    f.writelines("Depht= %s \n" %  model.depth())
    f.writelines("----------------------\n")
    f.writelines(model.toDebugString())
    f.close()

    print("###########################################################################0-9 DECISION TREE")
    
    t0 = time()
    model = DecisionTree.trainClassifier(train2,categoricalFeaturesInfo={},impurity='gini',numClasses=10, maxDepth=8, maxBins=32)
    tt = time() - t0

    predictions =  model.predict(test2.map(lambda x: x.features))
    labelsAndPredictions = test2.map(lambda lp: lp.label).zip(predictions)

    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test2.count())

    print(testErr)
    Savecsv(labelsAndPredictions,"../data/poker/output/0-9DecisionTreePredictions.csv")

    f = open("../data/poker/output/0-9DecisionTree.txt", 'a')
    f.writelines("Model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("----------TREE-------- \n")
    f.writelines("Num nodes = %s \n" % model.numNodes())
    f.writelines("Depht= %s \n" %  model.depth())
    f.writelines("----------------------\n")
    f.writelines(model.toDebugString())
    f.close()
    
    sc.stop()