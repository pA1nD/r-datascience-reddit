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
