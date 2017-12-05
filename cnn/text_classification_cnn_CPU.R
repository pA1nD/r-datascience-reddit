#
# Text classification using Convolutional Neural Networks at character level
#

# init --------------------------------------------------------------------
library(mxnet)

alphabet <- c("abcdefghijklmnopqrstuvwxyz0123456789-,;.!?:'\"/\\|_@#$%^&*~`+ =<>()[]{}")
vocab.size <- nchar(alphabet)
feature.len <- 1014
args <- data.frame(
  network = 'crepe', # The cnn to use: 'crepe', 'vdcnn','vdcnn_residual'
  gpus = NULL, # the gpus will be used, e.g "0,1,2,3"
  "model_prefix" = '', # the prefix of the model to load/save
  "load_epoch" = '', # load the model on an epoch using the model-prefix
  "num_examples" = '', # the number of training examples
  "log_file" = '', # the name of log file
  "data_dir" = '../data', # the input data directory
  "train_dataset" = "train.csv", # train dataset name
  "val_dataset" = "val.csv", # validation dataset name
  "data_shape" = 3, # number of fields in the input csv, default: (label, text1, text2)
  "batch_size" = 128, # the batch size
  lr = .01, # the initial learning rate
  "lr_factor" = 1, # times the lr with a factor for every lr-factor-epoch epoch
  "lr_factor_epoch" = 1, # the number of epoch to factor the lr, could be .5
  mom = .9, # momentum for sgd
  wd = .00001, # weight decay for sgd
  "num_round" = 10, # the number of iterations over training data to train the model
  "kv_store" = 'device', # the kvstore type
  "num_classes" = 2, # the number of classes
  "log_dir" = "/tmp/", # directory of the log file
  depth = 9  # the depth for resnet, it can be a value among 9, 17, 29, 49
)

# network
source("R/crepe_model.R")
network <- get_symbol(vocab.size = vocab.size, num.output.classes = args$num_classes)



# Create an intermediate file with a dictionary -----------------------------------------------------
train.file.input <- file.path(args$data_dir, args$train_dataset)
train.filename <- strsplit(basename(train.file.input), "\\.")[[1]]
train.file.output <- file.path(args$data_dir, paste0(train.filename[1], "_encoded.", train.filename[2]))
test.file.input <- file.path(args$data_dir, args$val_dataset)
test.filename <- strsplit(basename(test.file.input), "\\.")[[1]]
test.file.output <- file.path(args$data_dir, paste0(test.filename[1], "_encoded.", test.filename[2]))

source("text_encoder.R")
if (!file.exists(train.file.output)) {
  text.encoder.csv(
    input.file = train.file.input,
    output.file = train.file.output,
    alphabet = alphabet,
    max.text.lenght = feature.len,
    shuffle = TRUE
  )
}
if (!file.exists(test.file.output)) {
  text.encoder.csv(
    input.file = test.file.input,
    output.file = test.file.output,
    alphabet = alphabet,
    max.text.lenght = feature.len,
    shuffle = FALSE
  )
}

# Custom CSVIter --------------------------------------------------------------------
CustomCSVIter <- setRefClass(
  "CustomCSVIter",
  fields = c("iter", "data.csv", "batch.size",
             "alphabet", "feature.len"),
  contains = "Rcpp_MXArrayDataIter",
  methods = list(
    initialize = function(iter,
                          data.csv,
                          batch.size,
                          alphabet,
                          feature.len) {
      csv_iter <- mx.io.CSVIter(data.csv = data.csv,
                                data.shape =
                                  feature.len + 1,
                                #=features + label
                                batch.size =
                                  batch.size)
      .self$iter <- csv_iter
      .self$data.csv <- data.csv
      .self$batch.size <- batch.size
      .self$alphabet <- alphabet
      .self$feature.len <- feature.len
      .self
    },
    value = function() {
      val <- as.array(.self$iter$value()$data)
      val.y <- val[1, ]
      val.x <- val[-1, ]
      val.x <- dict.decoder(
        data = val.x,
        alphabet = .self$alphabet,
        feature.len = .self$feature.len,
        batch.size = .self$batch.size
      )
      val.x <- mx.nd.array(val.x)
      val.y <- mx.nd.array(val.y)
      list(data = val.x, label = val.y)
    },
    iter.next = function() {
      .self$iter$iter.next()
    },
    reset = function() {
      .self$iter$reset()
    },
    num.pad = function() {
      .self$iter$num.pad()
    },
    finalize = function() {
      .self$iter$finalize()
    }
  )
)
train.iter <- CustomCSVIter$new(
  iter = NULL,
  data.csv = train.file.output,
  batch.size = args$batch_size,
  alphabet = alphabet,
  feature.len = feature.len
)
test.iter <- CustomCSVIter$new(
  iter = NULL,
  data.csv = test.file.output,
  batch.size = args$batch_size,
  alphabet = alphabet,
  feature.len = feature.len
)

# Train -------------------------------------------------------------------

# devices
# if (is.null(args$gpus)) {
  devices <- mx.cpu()
# } else {
#   devices <- lapply(unlist(strsplit(args$gpus, ",")), function(i) {
#     mx.gpu(as.integer(i))
#   })
# }

# # save model
# if (is.null(args$model_prefix)) {
  checkpoint <- NULL
# } else {
#   checkpoint <- mx.callback.save.checkpoint(args$model_prefix)
# }
#
# # load pretrained model
# if (!is.null(args$load_epoch)) {
#   if (is.null(args$model_prefix))
#     stop("model_prefix should not be empty")
#   begin.round <- args$load_epoch
#   model <- mx.model.load(args$model_prefix, iteration = begin.round)
#   network <- model$symbol
#   arg.params <- model$arg.params
#   aux.params <- model$aux.params
# } else{
#   arg.params <- NULL
#   aux.params <- NULL
#   begin.round <- 1
# }

# learning rate scheduler
# if (args$lr_factor < 1) {
#   if (is.null(args$num_examples))
#     stop("When using the learning rate scheduler, you have to set the number of training examples")
#   epoch_size <-
#     as.integer(max(args$num_examples / args$batch_size), 1)
#   lr.scheduler <- mx.lr_scheduler.FactorScheduler(step = as.integer(max(epoch_size * args$lr_factor_epoch, 1)),
#                                                   factor_val = args$lr_factor)
# } else{
  lr.scheduler = NULL
# }

time_init <- Sys.time()
model <- mx.model.FeedForward.create(
  symbol = network,
  X = train.iter,
  eval.data = test.iter,
  ctx = devices,
  begin.round = begin.round,
  num.round = args$num_round,
  array.batch.size = args$batch_size,
  learning.rate = args$lr,
  momentum = args$mom,
  eval.metric = mx.metric.accuracy,
  wd = args$wd,
  kvstore = args$kv_store,
  initializer = mx.init.normal(sd = 0.05),
  arg.params = arg.params,
  aux.params = aux.params,
  lr_scheduler = lr.scheduler,
  optimizer = "sgd",
  epoch.end.callback = checkpoint,
  batch.end.callback = mx.callback.log.speedometer(args$batch_size, frequency = 100)
)
time_end <- Sys.time()
difftime(time_end, time_init, units = "hours")
cat(paste0("Training finished\n"))
