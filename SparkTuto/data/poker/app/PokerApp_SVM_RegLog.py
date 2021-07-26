#cd c:/spark/spark-1.6.1-bin-hadoop2.6/bin
#pyspark --driver-memory 2g --executor-memory 2g c:\spark\spark-1.6.1-bin-hadoop2.6\data\app\PokerApp_SVM_RegLog.py



######################################################### IMPORT USEFULL MODULES
import pandas
import csv
from pyspark.mllib.util import MLUtils
from pyspark import SparkContext, SparkConf
from pyspark.mllib.classification import SVMWithSGD, SVMModel
from pyspark.sql import SQLContext
from pyspark.ml.feature import VectorAssembler
from pyspark.sql.types import *
from pyspark.mllib.regression import LabeledPoint
from pyspark.sql.functions import col
from pyspark.mllib.classification import LogisticRegressionWithSGD
from time import time

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
    sc = SparkContext("local","PokerApp_SVM&RegLog")
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

    print("########################################################################### BINARY LOGISTIC REGRESSION")
    t0 = time()
    RL1 = LogisticRegressionWithSGD.train(train, 100)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  RL1.predict(test.map(lambda x: x.features))
    labelsAndPredictions = test.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/BinaryRegLogPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/BinaryRegLog.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("Final weights: " + str(RL1.weights))
    f.writelines("Final intercept: " + str(RL1.intercept))
    f.close()

    print("########################################################################### 0-9 LOGISTIC REGRESSION")
    t0 = time()
    RL2 = LogisticRegressionWithSGD.train(train2, 100)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  RL1.predict(test2.map(lambda x: x.features))
    labelsAndPredictions = test2.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/0-9_RegLogPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test2.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/0-9_RegLog.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("----------METRICS--------\n")
    f.writelines("MSE = %s \n" % testErr)
    f.writelines("Final weights: " + str(RL2.weights))
    f.writelines("Final intercept: " + str(RL2.intercept))
    f.close()
 
    print("########################################################################### BINARY SVM")
    t0 = time()
    SVM1 = SVMWithSGD.train(train, iterations=100)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  RL1.predict(test.map(lambda x: x.features))
    labelsAndPredictions = test.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/BinarySVMPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/BinarySVM.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("MSE = %s \n" % testErr)
    f.close()
    
    print("########################################################################### BINARY SVM")
    t0 = time()
    SVM2 = SVMWithSGD.train(train2, iterations=100)
    tt = time() - t0
    
    #Predict via model and bind result with expected labels
    predictions =  SVM2.predict(test2.map(lambda x: x.features))
    labelsAndPredictions = test2.map(lambda lp: lp.label).zip(predictions)
    # Export data to csv
    Savecsv(labelsAndPredictions,"../data/poker/output/0-9_SVMPredictions.csv")

    #Compute Error Rate by parallelized calculation
    a=labelsAndPredictions.filter(lambda x: x[0]!= x[1])
    FalsePred=sc.parallelize(a.collect(),4)
    testErr = FalsePred.count() / float(test2.count())

    #Export Tree architecture and Metrics in txt file
    f = open("../data/poker/output/0-9_SVM.txt", 'a')
    f.writelines("model trained in %s seconds \n" %round(tt,3))
    f.writelines("MSE = %s \n" % testErr)
    f.close()
    sc.stop()