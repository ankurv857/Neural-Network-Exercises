
#Python Codes - Image Process.py
import theano
import keras
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os
from keras.utils.np_utils import to_categorical

print os.getcwd()

train = pd.read_csv('fer2013.csv')
#print train.head(10)

train['pixels_s'] = [x.split(" ") for x in train['pixels']]

#print train.head(10)
#print len(train['pixels_s'][12])

train['matrix'] = [np.reshape(x,(48,48)) for x in train['pixels_s']]
train['matrix'] = [x.astype(np.int) for x in train['matrix']]
#train.head(10)

plt.imshow(train['matrix'][0])
plt.show()

train_X = train[train['Usage'] == 'Training']
test_X = train[train['Usage'] != 'Training']

X_train = np.array([[sample] for sample in X_train])

from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation, Flatten
from keras.layers import Convolution2D, MaxPooling2D, Convolution1D
from keras.optimizers import SGD

model = Sequential()
# input: 100x100 images with 3 channels -> (3, 100, 100) tensors.
# this applies 32 convolution filters of size 3x3 each.
model.add(Convolution2D(32, 3, 3, border_mode='valid', input_shape=(1,48,48)))
model.add(Activation('relu'))
model.add(Convolution2D(32, 3, 3))
model.add(Activation('relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))

model.add(Convolution2D(64, 3, 3, border_mode='valid'))
model.add(Activation('relu'))
model.add(Convolution2D(64, 3, 3))
model.add(Activation('relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))

model.add(Flatten())
# Note: Keras does automatic shape inference.
model.add(Dense(256))
model.add(Activation('relu'))
model.add(Dropout(0.5))

model.add(Dense(7))
model.add(Activation('softmax'))

sgd = SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True)
model.compile(loss='categorical_crossentropy', optimizer=sgd)

model.fit(X_train, Y_train, batch_size=32, nb_epoch=1)
