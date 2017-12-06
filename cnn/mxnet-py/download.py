import wget
import os

def download_file(url):
    """
    Downloads a file from a url if the file does not exist in the current folder
    :param url: Url to the file
    """
    local_filename = url.split('/')[-1]
    if os.path.isfile(local_filename):
        print("The file %s already exist in the current directory" % local_filename)
    else:
        print('downloading data: %s' % url)
        response = wget.download(url)
        print('saved data')

import sys

url_train = 'https://mxnetstorage.blob.core.windows.net/public/mnist/mnist_train.csv'
url_test = 'https://mxnetstorage.blob.core.windows.net/public/mnist/mnist_test.csv'

print("Downloading file %s" % url_train)
download_file(url_train)

print("Downloading file %s" % url_test)
download_file(url_test)

#http://yann.lecun.com/exdb/mnist/
# %matplotlib inline
import numpy as np
import pandas as pd
import mxnet as mx
import time
import math
# import matplotlib.pyplot as plt
import logging

BATCH_SIZE = 100
DATA_SHAPE = (BATCH_SIZE, 1, 28, 28)
EPOCHS = 10
LR  = 0.1
MOM = 0.9
WD = 0.00001

# logging
logger = logging.getLogger()
fhandler = logging.FileHandler(filename='lenet.log', mode='a')
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
fhandler.setFormatter(formatter)
logger.addHandler(fhandler)
logger.setLevel(logging.DEBUG)


# gather data
train = pd.read_csv('mnist_train.csv', header=None)
train_y = train[[0]].values.ravel()
train_x = train.iloc[:,1:].values

# modify data
train_x = np.array(train_x, dtype='float32').reshape((-1, 1, 28, 28))
#print(train_x.shape)  # (60000, 1, 28, 28)
# normalise (between 0 and 1)
train_x[:] /= 255.0

# iterator to feed mini_batch at a time
# returns <mxnet.io.DataBatch object at 0x000001AA996B38D0>
# type <class 'mxnet.io.DataBatch'>
train_iter = mx.io.NDArrayIter(train_x, train_y, batch_size=BATCH_SIZE, shuffle=True)


def create_lenet():
    # create symbolic representation
    data = mx.symbol.Variable('data')
    input_y = mx.sym.Variable('softmax_label')  # placeholder for output

    conv1 = mx.symbol.Convolution(
        data=data, kernel=(5,5), num_filter=20)
    tanh1 = mx.symbol.Activation(
        data=conv1, act_type="tanh")
    pool1 = mx.symbol.Pooling(
        data=tanh1, pool_type="max", kernel=(2,2), stride=(2,2))

    conv2 = mx.symbol.Convolution(
        data=pool1, kernel=(5,5), num_filter=50)
    tanh2 = mx.symbol.Activation(
        data=conv2, act_type="tanh")
    pool2 = mx.symbol.Pooling(
        data=tanh2, pool_type="max", kernel=(2,2), stride=(2,2))

    flatten = mx.symbol.Flatten(
        data=pool2)

    fc1 = mx.symbol.FullyConnected(
        data=flatten, num_hidden=500)
    tanh3 = mx.symbol.Activation(
        data=fc1, act_type="tanh")

    fc2 = mx.symbol.FullyConnected(
        data=tanh3, num_hidden=10)

    lenet = mx.symbol.SoftmaxOutput(
        data=fc2, label=input_y, name="softmax")
    return lenet

# train the NN
ctx = mx.cpu()
cnn = create_lenet()

# Visualise symbol (for crepe)
# mx.viz.plot_network(cnn)

cnn.list_arguments()


# Setup model
model = mx.model.FeedForward(
    ctx = ctx,
    symbol = cnn,
    num_epoch = EPOCHS,  # number of training rounds
    learning_rate = LR,  # learning rate
    momentum = MOM,   # momentum for sgd
    wd = WD  # weight decay for reg
)


# Train
# Log accuracy to file every batch
# Save parameters at every epoch
tic = time.time()

model.fit(
    X = train_iter,
    eval_metric=['accuracy'],
    batch_end_callback=mx.callback.Speedometer(BATCH_SIZE),
    epoch_end_callback=mx.callback.do_checkpoint("lenet_checkp_")
)

print("Finished training in %.0f seconds" % (time.time() - tic))
# Finished training in 577 seconds

# prediction of test set
test = pd.read_csv('mnist_test.csv', header=None)
test_y = test[[0]].values.ravel()
test_x = test.iloc[:,1:].values

test_x = np.array(test_x, dtype='float32').reshape((-1, 1, 28, 28))
test_x[:] /= 255.0

test_iter = mx.io.NDArrayIter(test_x, test_y, batch_size=100)

# most likely will be last element after sorting
pred = np.argsort(model.predict(X = test_iter))[:,-1]

# accuracy
print(sum(pred==test_y)/len(test_y))
# 0.9901

# save
np.savetxt('predicted_images.csv', np.c_[pred, test_y], delimiter=',', fmt='%d')
