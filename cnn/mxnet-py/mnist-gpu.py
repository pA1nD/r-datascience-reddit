import wget
import os
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
# ################################################

# create own iterator (that can optionally apply transform function)
def manual_iterator(infile, batch_size=100, shuffle=True):

    """ Accepts 'infile' location to a .csv and then yields numpy arrays
    wrapped as NDarrays using mx.nd.array(np.array(data, dtype='float32'))
    reshaped for the CNN symbol i.e. .reshape((LENGTH, 1, FEAT, DIM))"""

    # load in data
    df = pd.read_csv(infile, header=None)

    # shuffle
    if shuffle:
        df = df.sample(frac=1).reset_index(drop=True)

    train_x, train_y = df.iloc[:,1:].values, df[[0]].values.ravel()

    # modify data
    train_x = np.array(train_x, dtype='float32').reshape((-1, 1, 28, 28))

    # transformation
    train_x[:] /= 255.0

    # yield mini-batches as NDArray
    X_split = np.zeros(DATA_SHAPE, dtype='float32')
    for ti, tx in enumerate(train_x):

        X_split[ti%batch_size][0] = tx

        if (ti + 1) % batch_size == 0:
            yield mx.nd.array(X_split), mx.nd.array(train_y[ti+1-batch_size:ti+1])
            X_split = np.zeros(DATA_SHAPE, dtype='float32')

def example(infile='mnist_train.csv'):

    mbatch = 3
    df = pd.read_csv(infile, header=None)
    train_y = df[[0]].values.ravel()
    print("actual: ", train_y[:mbatch*4])

    counter = 0
    for batchX, batchY in manual_iterator(infile, batch_size=mbatch, shuffle=False):
        print("batch: ", batchY.asnumpy().astype('int32'))
        counter += 1
        if counter == 4:
            break   


# #################################################

from mxnet.io import DataBatch

GPUS = 4
ctx = [mx.gpu(int(i)) for i in range(GPUS)]
BATCH_SIZE = 100 * GPUS
DATA_SHAPE = (BATCH_SIZE, 1, 28, 28)
EPOCHS = 10
LR  = 0.1
MOM = 0.9
WD = 0.00001

print(ctx)

cnn = create_lenet()
mod = mx.mod.Module(cnn, context=ctx)

mod.bind(data_shapes=[('data', DATA_SHAPE)],
         label_shapes=[('softmax_label', (BATCH_SIZE,))])

mod.init_params(mx.init.Uniform(scale=0.01))
mod.init_optimizer(kvstore='device',
                   optimizer='sgd',
                   optimizer_params={
                       "learning_rate": LR,
                       "momentum": MOM,
                       "wd": WD,
                       "rescale_grad": 1.0/BATCH_SIZE
                   })



tic = time.time()

# Evaluation metric:
metric = mx.metric.Accuracy()

# Train EPOCHS
for epoch in range(EPOCHS):
    t = 0
    metric.reset()
    for batchX, batchY in manual_iterator('mnist_train.csv',
                                          batch_size=BATCH_SIZE, shuffle=True):
        # Wrap X and y in DataBatch (need to be in a list)
        batch = DataBatch(data=[batchX], label=[batchY])
        # Push data forwards and update metric
        # For training + testing
        mod.forward(batch, is_train=True)
        mod.update_metric(metric, batch.label)
        # Get weights and update
        # For training only
        mod.backward()
        mod.update()
        # log every batch
        t += 1
        if t % BATCH_SIZE == 0:
            toc = time.time()
            train_t = toc - tic
            metric_m, metric_v = metric.get()
            print("epoch: %d iter: %d metric(%s): %.4f dur: %.0f" % (epoch, t, metric_m, metric_v, train_t))

print("Finished training in %.0f seconds" % (time.time() - tic))

# On CPU we had 577 seconds, with (one) GPU we get 351 seconds

# Test
metric = mx.metric.Accuracy()

for batchX, batchY in manual_iterator('mnist_test.csv',
                                      batch_size=BATCH_SIZE):
    # Wrap X and y in DataBatch (need to be in a list)
    batch = DataBatch(data=[batchX], label=[batchY])
    # Push data forwards and update metric
    # For training + testing
    mod.forward(batch, is_train=False)
    mod.update_metric(metric, batch.label)

metric_m, metric_v = metric.get()
print("metric(%s): %.4f" % (metric_m, metric_v))
