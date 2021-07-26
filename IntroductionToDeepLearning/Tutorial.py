# -*- coding: utf-8 -*-
"""
Created on Mon May 16 20:41:04 2016

@author: celian
"""
############################################## IMPORT USEFUL LIBRARIES
import time
import numpy as np
import pandas as pd
from keras.layers.core import Dense, Dropout
from keras.layers.normalization import BatchNormalization
from keras.callbacks import Callback, EarlyStopping
from keras.models import Sequential
from keras.layers.recurrent import LSTM
from pandas.tools.plotting import autocorrelation_plot
import matplotlib.pyplot as plt

############################################## DEFINING FUNCTIONS

##################################### 1. DATA FUNCTIONS
#### LOADING DATA
## Input :
# > data : vector of values
# > n_prev : length of the window
## Output :
# > formated data vectors

def _load_data(data, n_prev =7):  
    """
    data should be pd.DataFrame()
    """

    docX, docY = [], []
    for i in range(len(data)-n_prev):
        docX.append(data.iloc[i:i+n_prev].as_matrix())
        docY.append(data.iloc[i+n_prev].as_matrix())
    alsX = np.array(docX)
    alsY = np.array(docY)

    return alsX, alsY
    
#### TRAIN AND TEST SPLIT
## Input :
# > df : dataframe formated 
# > test_size : rate of the data used for testing
## Output :
# > Splitted data set
def train_test_split(df, test_size=0.1):  
    """
    This just splits data to training and testing parts
    """
    ntrn = int(len(df) * (1 - test_size))

    X_train, y_train = _load_data(df.iloc[0:ntrn])
    X_test, y_test = _load_data(df.iloc[ntrn:])

    return (X_train, y_train), (X_test, y_test)


##################################### 2. MODELS FUNCTIONS

    
def make_LSTM_network1(in_out_neurons,hidden_neurons):
    model = Sequential()
    model.add(LSTM(hidden_neurons, input_dim=in_out_neurons, forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=False) )   
    model.add(Dense(in_out_neurons, input_dim=hidden_neurons))
    return model

def make_LSTM_network2(in_out_neurons,hidden_neurons1,hidden_neurons2):
    model = Sequential()
    model.add(LSTM(hidden_neurons1, input_dim=in_out_neurons, forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=True) )
    #model.add(Dropout(0.25)) 
    model.add(LSTM(hidden_neurons2, input_dim=hidden_neurons1, forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=False) )
    model.add(Dense(in_out_neurons, input_dim=hidden_neurons2))
    return model
    
def make_LSTM_network3(in_out_neurons,hidden_neurons,batch1,batch2):
    model = Sequential()
    model.add(LSTM(hidden_neurons, input_shape=(batch1, in_out_neurons), forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=True) )
    model.add(Dropout(0.25))    
    model.add(LSTM(hidden_neurons, input_shape=(batch2, hidden_neurons), forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=False) )
    model.add(Dense(in_out_neurons, input_dim=hidden_neurons))
    return model


def make_LSTM_network4(in_out_neurons,hidden_neurons1,hidden_neurons2,hidden_neurons3):
    model = Sequential()
    model.add(BatchNormalization())
    model.add(LSTM(hidden_neurons1, input_dim=in_out_neurons, forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=True) )
    model.add(Dropout(0.25))    
    model.add(BatchNormalization())
    model.add(LSTM(hidden_neurons2, input_dim=hidden_neurons1, forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=False) )
    model.add(Dropout(0.25))    
    model.add(BatchNormalization())
    model.add(LSTM(hidden_neurons2, input_dim=hidden_neurons1, forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=False) )
    model.add(Dense(in_out_neurons, input_dim=hidden_neurons2))
    return model


##################################### 3. PLOTTING THE RESULTS

def draw_results(predicted,y):
   pd.concat([pd.DataFrame(predicted),pd.DataFrame(y)], axis=1).plot()
   
def draw_loss(history):
    plt.figure(figsize=(6, 3))
    plt.plot(history.losses)
    plt.ylabel('error')
    plt.xlabel('iteration')
    plt.title('training error')
    plt.show() 


 ##################################### 4. TRAINING FUNCTION
def train_model(model, X_train, Y_train, batch_size, nb_epoch,early_stop):
 
    #DEFINING OPTIMISER CRITERAS
    model.compile(loss="mean_squared_error", optimizer="adam")  
    
    #FORCE THE MODULE TO GET LOSS HISTORY
    class TrainingHistory(Callback):
        def on_train_begin(self, logs={}):
            self.losses = []
            self.predictions = []
            self.i = 0
            self.save_every = 50
    
        def on_batch_end(self, batch, logs={}):
            self.losses.append(logs.get('loss'))
            self.i += 1        
            if self.i % self.save_every == 0:        
                pred = model.predict(X_train)
                self.predictions.append(pred)
    #Create a new instance of history                
    history = TrainingHistory()
    
    #IF we want to early stop ...
    if early_stop==True:
        earlyStopping=EarlyStopping(monitor='val_loss', patience=50, verbose=0, mode='auto')
        model.fit(X_train, y_train, batch_size=batch_size, nb_epoch=nb_epoch, validation_split=0.05,callbacks=[history,earlyStopping])
    # or not ...
    else:
        model.fit(X_train, y_train, batch_size=batch_size, nb_epoch=nb_epoch, validation_split=0.05,callbacks=[history])
   
    return (history)


############################################" MAIN CODE ###################################

#Load the data   
df=pd.DataFrame(pd.read_csv('/home/celian/Bureau/Meteo/neige.csv', sep=','))
#Plot the time series  
df.dropna().plot()
#Delete NA
df2=df.dropna()
#Visualise the correlaction trough time
autocorrelation_plot(df2)
#Split data-set
(X_train, y_train), (X_test, y_test) = train_test_split(df2)

########################################## MODEL 0 : SIMPLE LSTM 
#Define parameters
in_out_neurons=1
hidden_neurons = 30
batch_size=100
nb_epoch=200
model0= make_LSTM_network1(in_out_neurons,hidden_neurons)

####### TRAINING STEP
t0 = time.clock()
history=train_model(model0, X_train, y_train, batch_size, nb_epoch,early_stop=False)
print ('TIME :'+str(time.clock()-t0))
#######

##### PREDICT VALUES
predicted = model0.predict(X_test)

#####  COMPUTE RSME           
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))               
print('RSME :' +str(rmse))

### PLOTING RESULTS
pd.DataFrame(y_test).plot()
draw_results(predicted,y_test)
draw_loss(history)


########################################## MODEL 1 : LSTM 2 LAYERS
#Define parameters
batch_size=100
nb_epoch=200
in_out_neurons=1
hidden_neurons1 = 10
hidden_neurons2 = 30
#Create model
model1=make_LSTM_network2(in_out_neurons,hidden_neurons1,hidden_neurons2)

####### TRAINING STEP
t0 = time.clock()
history=train_model(model1, X_train, y_train,batch_size, nb_epoch,early_stop=False)
print ('TIME :'+str(time.clock()-t0))
#######

##### PREDICT VALUES
predicted = model1.predict(X_test)

#####  VALIDATION STEP
model1.evaluate(X_test, y_test,
                  batch_size=100, verbose=1)
 
#####  COMPUTE RSME           
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))               
print('RSME :' +str(rmse))

### PLOTING RESULTS
pd.DataFrame(y_test).plot()
draw_results(predicted,y_test)
draw_loss(history)



########################################## MODEL 2 : LSTM 2 LAYERS WITH DROPOUT ET MINI BATCHES
batch_size=100
nb_epoch=200
in_out_neurons=1
hidden_neurons = 100
batch1=10
batch2=50

model2= make_LSTM_network3(in_out_neurons,hidden_neurons, batch1,batch2)

weights = model2.layers[0].get_weights()
w0_0 = weights[0][0][0]
w1_0 = weights[1][0]

t0 = time.clock()
history=train_model(model2, X_train, y_train,batch_size, nb_epoch,early_stop=True)
print ('TIME :'+str(time.clock()-t0))

predicted = model2.predict(X_test)

model2.evaluate(X_test, y_test,
                  batch_size=100, verbose=1, show_accuracy=True)
                  
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))               
print('RSME :' +str(rmse))

draw_results(predicted,y_test)
draw_loss(history)

weights_1 = model2.layers[0].get_weights()
w0_1 = weights_1[0][0][0]
w1_1 = weights_1[1][0]

print(w0_1)
print(w0_0)

print(w1_1)
print(w1_0)

########################################## MODEL 3 : LSTM 2 LAYERS WITH DROPOUT ET MINI BATCHES
batch_size=100
nb_epoch=200
in_out_neurons=1
hidden_neurons1 = 10
hidden_neurons2 = 20
hidden_neurons3 = 30

model3= make_LSTM_network3(in_out_neurons,hidden_neurons1,hidden_neurons2,hidden_neurons3)

weights = model3.layers[0].get_weights()
w0_0 = weights[0][0][0]
w1_0 = weights[1][0]

t0 = time.clock()
history=train_model(model3, X_train, y_train,batch_size, nb_epoch,early_stop=True)
print ('TIME :'+str(time.clock()-t0))

predicted = model3.predict(X_test)

model2.evaluate(X_test, y_test,
                  batch_size=100, verbose=1, show_accuracy=True)
                  
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))               
print('RSME :' +str(rmse))

draw_results(predicted,y_test)
draw_loss(history)

weights_1 = model3.layers[0].get_weights()
w0_1 = weights_1[0][0][0]
w1_1 = weights_1[1][0]

print(w0_1)
print(w0_0)

print(w1_1)
print(w1_0)
