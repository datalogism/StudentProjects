# -*- coding: utf-8 -*-
"""
Created on Tue May 24 21:15:47 2016

@author: celian
"""

# -*- coding: utf-8 -*-
"""
Created on Mon May 16 20:41:04 2016

@author: celian
"""

import numpy as np
import pandas as pd
from keras.layers.core import Dense, Dropout, Activation
from keras.optimizers import Adam, RMSprop
from keras.callbacks import Callback
from keras.models import Sequential
from keras.layers.core import Dense, Activation, Dropout,TimeDistributedDense
from keras.layers.recurrent import LSTM, GRU
from pandas.tools.plotting import autocorrelation_plot
import matplotlib.pyplot as plt

input_meteo_0=pd.DataFrame(pd.read_csv('/home/celian/Bureau/Meteo/input.csv', sep=';'))
date=input_meteo_0.iloc[:,0]
output_meteo_0=pd.DataFrame(pd.read_csv('/home/celian/Bureau/Meteo/output.csv', sep=';'))
output_meteo_0.shape
#all_meteo=pd.concat([input_meteo_0,output_meteo_0], axis=1).drop('Vit. raf. max. ', 1)
#df=all_meteo

#for i in range(3,all_meteo.shape[1]):
#    df.iloc[:,i]=all_meteo.iloc[:,i].interpolate('quadratic')
output_meteo_0.iloc[output_meteo_0.shape[0]-7]
t=output_meteo_0.iloc[output_meteo_0.shape[0]-20:output_meteo_0.shape[0]-13]
#######################################


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

def train_test_split(df, test_size=0.1):  
    """
    This just splits data to training and testing parts
    """
    ntrn = int(len(df) * (1 - test_size))

    X_train, y_train = _load_data(df.iloc[0:ntrn])
    X_test, y_test = _load_data(df.iloc[ntrn:])

    return (X_train, y_train), (X_test, y_test)

#X_train, X_test, y_train, y_test = train_test_split(input_meteo_1,output_meteo_1,
 #       test_size=0.33, random_state=42)

#def train_test_split(df, test_size=0.3):  
#    """
#    This just splits data to training and testing parts
#    """
#    ntrn = int(len(df) * (1 - test_size))
#
#    X_train, y_train = _load_data(df2.iloc[0:ntrn])
#    X_test, y_test = _load_data(df2.iloc[ntrn:])
#
#    return (X_train, y_train), (X_test, y_test)
#    
    



def make_LSTM_network1(in_out_neurons,hidden_neurons):
    model = Sequential()
    model.add(LSTM(hidden_neurons, input_dim=in_out_neurons, forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=False) )
    model.add(Dropout(0.25))    
    model.add(Dense(in_out_neurons, input_dim=hidden_neurons))
    
    
    return model

def make_LSTM_network2(in_out_neurons,hidden_neurons,batch1,batch2):
    model = Sequential()
    model.add(LSTM(100, input_shape=(7, 1), forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=True) )
    model.add(Dropout(0.25))    
    model.add(LSTM(100, input_shape=(28, 100), forget_bias_init='one', activation='tanh', inner_activation='sigmoid', return_sequences=False) )
    model.add(Dense(1, input_dim=100))
   
    return model
     
def draw_results(predicted,y):
   pd.concat([pd.DataFrame(predicted),pd.DataFrame(y)], axis=1).plot()
def draw_loss(history):
    plt.figure(figsize=(6, 3))
    plt.plot(history.losses)
    plt.ylabel('error')
    plt.xlabel('iteration')
    plt.title('training error')
    plt.show() 

def draw_animation(x,y):
    import matplotlib.animation as animation

    fig = plt.figure(figsize=(5, 2.5))
    plt.plot(x, y,  label='data')
    line, = plt.plot(x, history.predictions[0],  label='prediction')
    plt.legend(loc='upper left')
    
    def update_line(num):
        plt.title('iteration: {0}'.format((history.save_every * (num + 1))))
        line.set_xdata(x)
        line.set_ydata(history.predictions[num])
        return []
    
    ani = animation.FuncAnimation(fig, update_line, len(history.predictions),
                                       interval=50, blit=True)
    ani.save('../../videos/2016q1/neuron.mp4', fps=30, extra_args=['-vcodec', 'libx264', '-pix_fmt','yuv420p'])
    plt.close()
    
    plt.figure(figsize=(5, 2.5))
    plt.plot(x, y, label='data')
    plt.plot(x, history.predictions[0], label='prediction')
    plt.legend(loc='upper left')
    plt.title('iteration: 0')
    plt.savefig('../../images/2016q1/neuron_start.png')
    plt.close()

 
def train_model(model, X_train, Y_train, X_test, Y_test):
 
    #sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
    #model.compile(loss='categorical_crossentropy', optimizer=sgd)
    model.compile(loss="mean_squared_error", optimizer="adam")  
    #model.fit(X_train, Y_train, nb_epoch=nb_epoch, batch_size=batch_size,
    #          validation_split=0.1, show_accuracy=True, verbose=1)
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
                
    history = TrainingHistory()
        
    model.fit(X_train, y_train, batch_size=100, nb_epoch=200, validation_split=0.05,callbacks=[history])
   
    return (history)

#def save_model(model,name):
# 
#   model_json = model.to_json()
#   open( str(name)+'.json', 'w').write(model_json)
#   model.save_weights(str(name)+'.h5', overwrite=True)
#
#from keras.models import model_from_json
#def load_model(model_def_fname, model_weight_fname):
#   model = model_from_json(open(model_def_fname).read())
#   model.load_weights(model_weight_fname)
#   return model
#   
   
############################################" MAIN CODE ###################################
   
df=pd.DataFrame(pd.read_csv('/home/celian/Bureau/Meteo/output.csv', sep=','))
df.dropna().plot()
df2=df.dropna()
np.array(t)
autocorrelation_plot(df2)
(X_train, y_train), (X_test, y_test) = train_test_split(df2)




###### LSTM
### 30
in_out_neurons=1
hidden_neurons = 30
model1=make_LSTM_network1(in_out_neurons,hidden_neurons)

history=train_model(model1, X_train, y_train, X_test, y_test)
predicted = model1.predict(X_test)


model1.evaluate(X_test, y_test,
                  batch_size=100, verbose=1, show_accuracy=True)
                  
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))               
print('RSME :' +str(rmse))
   

    
draw_results(predicted,y_test)
draw_loss(history)

weights = model1.layers[0].get_weights()
w0 = weights[0][0][0]
w1 = weights[1][0]
###### LSTM
### 100
in_out_neurons=1
hidden_neurons = 100

model1=make_LSTM_network1(in_out_neurons,hidden_neurons)
train_model(model1, X_train, y_train, X_test, y_test)
predicted = model1.predict(X_test)
draw_result(predicted,y_test)
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))

#save_model(model1,"poidsmodele_lstm_100")
#model = load_model('cifar10_architecture.json', 'cifar10_weights.h5')

###### LSTM
### 350
in_out_neurons=1
hidden_neurons = 350
model2=make_LSTM_network2()
train_model(model2, X_train, y_train, X_test, y_test)
predicted = model1.predict(X_test)
draw_result(predicted,y_test)
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))
print('RSME :' +str(rmse))


###### LSTM
### 100*350
in_out_neurons=1
hidden_neurons1=100
hidden_neurons2=350
model2= make_LSTM_network2(in_out_neurons,hidden_neurons1,hidden_neurons2)
h=train_model(model2, X_train, y_train, X_test, y_test)
predicted = model2.predict(X_test)
draw_result(predicted,y_test)
rmse = np.sqrt(((predicted - y_test) ** 2).mean(axis=0))
print('RSME :' +str(rmse))
#save_model(model2,"poidsmodele_gru_100")

#reuniwatt=pd.DataFrame(pd.read_csv('/home/celian/Bureau/Meteo/DataReuniwatt.csv', sep=';'))
#reuniwatt.shape
#reuniwatt.dropna().iloc[:,267].plot()
#reuniwatt=reuniwatt.dropna()

