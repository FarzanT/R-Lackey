#=====================MLPs in H2O====================
library(h2o)
h2o.init(nthreads = -1, min_mem_size = "512M")
??h2o
train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"

train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)

y <- "C785" # response column: digits 0-9
x <- setdiff(names(train), y) #vector predictor column names

# Since the response is encoded as integers, we need to tell H2O that
# the response is in fact a categorical/factor column.  Otherwise, it 
# will train a regression model instead of multiclass classification.

train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

# First we will train a basic DL model with mostly default parameters. To execute the example more quickly, we will use a smaller hidden layer, hidden = c(20,20), than the default, which is c(200,200). What this means is that we will use two hidden layers, each of 20 neurons, rather than 200 neurons. This will cause a loss of accuracy of the model, at the price of speed. To get a more accurate model, I’d recommend using the default architecture for hidden, or something even a bit more complex.

dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit1",
                            hidden = c(300,300))

# First we will train a basic DL model with mostly default parameters. To execute the example more quickly, we will use a smaller hidden layer, hidden = c(20,20), than the default, which is c(200,200). What this means is that we will use two hidden layers, each of 20 neurons, rather than 200 neurons. This will cause a loss of accuracy of the model, at the price of speed. To get a more accurate model, I’d recommend using the default architecture for hidden, or something even a bit more complex.
# 
# A few notes:
#   
#   The DL model will infer the response distribution from the response encoding if it is not specified explicitly through the distribution argument.
# H2O’s DL will not be reproducible if it is run on more than a single core, so in this example, the performance metrics below may vary slightly from what you see on your machine. The implementation uses a technique called “Hogwild!” which increases the speed of training at the cost of reproducibility on multiple cores.
# Early stopping (stop training before the specified number of epochs is completed to prevent overfitting) is enabled by default. If a validation frame is given, or if cross-validation is used (nfolds > 1), it will use validation error to determine the early stopping point. If just a training frame is given (and no CV), it will use the training set to perform early stopping. More on that below.

# Note: The warning about “Dropping constant columns” occurs because MNIST is a sparse dataset, and there exist entire columns where every value is zero. Constant columns do not add any value to the model, so they are automatically removed.
# 
# In the next model, we will increase the number of epochs used in the DNN by setting epochs=50 (the default is 10). Increasing the number of epochs in a deep neural net may increase performance of the model, however, you have to be careful not to overfit your model to your training data. To automatically find the optimal number of epochs, you must use H2O’s early stopping functionality. Unlike the rest of the H2O algorithms, H2O’s DL will use early stopping by default, so for comparison we will first turn off early stopping. We do this in the next example by setting stopping_rounds=0.

dl_fit2 <- h2o.deeplearning(x = x, y = y, training_frame = train, model_id = "dl_fit2", epochs = 50, hidden = c(20,20), stopping_rounds = 0, seed = 1)

# Train a DNN with early stopping:
#   
#   This example will use the same model parameters as dl_fit2. However, this time, we will turn on early stopping and specify the stopping criterion. We will use cross-validation (nfolds=3 to determine the optimal number of epochs. Alternatively, we could pass validation set to the validation_frame argument (note: the validation set must be different than the test set!).

dl_fit3 <- h2o.deeplearning(x = x, y = y, training_frame = train, model_id = "dl_fit3", epochs = 50, hidden = c(20, 20), nfolds = 3, score_interval = 1, stopping_rounds = 5, stopping_metric = "misclassification", stopping_tolerance = 1e-3, seed = 1)

# Let's compare the performance of the three DL models.
dl_perf1 <- h2o.performance(model = dl_fit1, newdata = test)
dl_perf2 <- h2o.performance(model = dl_fit2, newdata = test)
dl_perf3 <- h2o.performance(model = dl_fit3, newdata = test)

# Retrieve test set MSE
h2o.mse(dl_perf1)
h2o.mse(dl_perf2)
h2o.mse(dl_perf3)

# There are a number of utility functions that allow us to inspect the model. For example, h2o.scoreHistory() or h2o.confusionMatrix()
h2o.scoreHistory(dl_fit3)
h2o.confusionMatrix(dl_fit3)

# We cal also plot a model, which will graph the performance of some metric over the training process.

plot(dl_fit3, timestep = "epochs", metric = "classification_error")

# However, it may be more interesting to plot one (or all) of the CV models, as they will show the training error along with the validation error – a more informative plot with respect to evaluating overfitting.

# Get the CV models from the dl_fit3 object
cv_models <- sapply(dl_fit3@model$cross_validation_models,
                    function(i) h2o.getModel(i$name))
# Plot the scoring history over time
plot(cv_models[[1]], timestep = "epochs", metric = "classification_error")

#The “tick” at the right side of the plot is there because overwrite_with_best_model is set to TRUE by default. So what you are seeing at the end of the plot is the validation and training error for the final (best) model.


#===============Deep Learning Grid Search==================

# As an alternative to manual tuning, or “hand tuning”, we can use the h2o.grid() function to perform either a Cartesian or Randon Grid Search (RGS). Random Grid Search is usually a quicker way to find a good model, so we will provide a example of how to use H2O’s Random Grid Search on a DNN.
# 
# One handy feature of RGS is that you can specify how long you would like to execute the grid for – this can be based on a time, number of models, or a performance-metric-based stopping criterion. In the example below, we will train the DNN grid for 600 seconds (10 minutes).
# 
# First define a grid of Deep Learning hyperparamters and specify the search_criteria.

activation_opt <- c("Rectifier", "Maxout", "Tanh")
l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)

hyper_params <- list(activation = activation_opt, l1 = l1_opt, l2 = l2_opt)
search_criteria <- list(strategy = "RandomDiscrete", max_runtime_secs = 600)

# Rather than comparing models by using cross-validation (which is “better” but takes longer), we will simply partition our training set into two pieces – one for training and one for validiation.
# 
# This will split the train frame into an 80% and 20% partition of the rows.

splits <- h2o.splitFrame(train, ratios = 0.8, seed = 1)

# Train the random grid. Fixed non-default parameters such as hidden=c(20,20) can be passed directly to the h2o.grid() function.

dl_grid <- h2o.grid("deeplearning", x = x, y = y, grid_id = "dl_grid"
                    , training_frame = splits[[1]]
                    , validation_frame = splits[[2]]
                    , seed = 0, hidden = c(20,20)
                    , hyper_params = hyper_params
                    , search_criteria = search_criteria)

# Once we have trained the grid, we can collect the results and sort by our model performance metric of choice.

dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", sort_by = "accuracy"
                           , decreasing = TRUE)
print(dl_gridperf)

# Note that that these results are not reproducible since we are not using a single core H2O cluster (H2O’s DL requires a single core to be used in order to get reproducible results).

# Grab the model_id for the top DL model, chosen by validation error.

best_dl_model_id <- dl_gridperf@model_ids[[1]]
best_dl <- h2o.getModel(best_dl_model_id)

# Now let’s evaluate the model performance on a test set so we get an honest estimate of top model performance.
best_dl_perf <- h2o.performance(model = best_dl, newdata = test)
h2o.mse(best_dl_perf)

#====================Deep Learning Autoencoders==================

# Deep Learning Autoencoders can be used for both unsupervised pre-training of a supervised deep neural network or for anomaly detection. We will demonstrate these applications using the h2o package below.
# 
# From Statistical Learning with Sparsity (Hastie, Tibshirani, Wainwright, 2015) Section 8.2.5: “In the neural network literature, an autoencoder generalizes the idea of principal components.”
# 
# Autoencoders for Unsupervised Pre-Training
# On sparse autoencoders (although this can be said of autoencoders in general):
#   
#   “One important use of the sparse autoencoder is for pretraining. When fitting a supervised neural network to labelled data, it is often advantageous to first fit an autoencoder to the data without the labels and then use the resulting weights as starting values for fitting the supervised neural network (Erhan et al. 2010). Because the neural-network objective function is nonconvex, these starting weights can significantly improve the quality of the final solution. Furthermore, if there is additional data available without labels, the autoencoder can make use of these data in the pretraining phase.”
# 
# Additional Resources
# 
# JMLR Article: “Why Does Unsupervised Pre-training Help Deep Learning?”" by Dumitru Erhan, Aaron Courville, Yoshua Bengio, Pascal Vincent (2010).
# 
# There is a whole chapter about autoencoders at deeplearningbook.org, which I recommend reading for further study.
# 
# Andrew Ng also has a good course notes on sparse autoencoders, specifically.
# 
# Autoencoders in H2O
# Start up a local H2O cluster:

library(h2o)
h2o.init(nthreads = -1)
h2o.no_progress()

# Load the MNIST handwritten digits dataset, convert to an “H2O Frame” and train an unsupervised deep learning autoencoder. Note that we do not need to provide the response variable to the function, since this is an unsupervised model.

train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"
train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)
y <- "C785" # response column: digits 0-9
x <- setdiff(names(train), y) # vector of predictor column names
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

# Split the training data into two pieces: one that will be used for unsupervised pre-training and the other that will be used for supervised training. Note that this step is simply for demonstratation purposes – you would typically want to make use of all your labeled training data for the supervised learning problem.

splits <- h2o.splitFrame(train, 0.5, seed = 1)

# first part of the data, without labels for unsupervised learning
train_unsupervised <- splits[[1]]

# second part of the data, with labels for supervised learning
train_supervised <- splits[[2]]

?dim
dim(train_supervised)
dim(train_unsupervised)

#Let’s choose an archictecture that compresses the 784 inputs down to 64 at the smallest point. There are many good options for this parameter, this is just an example.

hidden <- c(128, 64, 128)

# Train the deep learning autoencoder model.

ae_model <- h2o.deeplearning(x = x
                             , training_frame = train_unsupervised
                             , model_id = "mnist_autoencoder"
                             , ignore_const_cols = FALSE
                             , activation = "Tanh" # Tanh is good for
                                                  # autoencoding
                             , hidden = hidden
                             , autoencoder = TRUE) 
# Next, we can use this pre-trained autoencoder model as a starting point for a supervised deep neural network (DNN).
fit1 <- h2o.deeplearning(x = x, y = y
                         , training_frame = train_supervised
                         , ignore_const_cols = FALSE
                         , hidden = hidden
                         , pretrained_autoencoder = "mnist_autoencoder")
perf1 <- h2o.performance(fit1, newdata = test)
h2o.mse(perf1)

# For comparison, let's train a DNN without using the weights from the autoencoder.

fit2 <- h2o.deeplearning(x = x, y = y
                         , training_frame = train_supervised
                         , ignore_const_cols = FALSE
                         , hidden = hidden)
perf2 <- h2o.performance(fit2, newdata = test)
h2o.mse(perf2)

#==================Deep Features==================

# Another application of DNNs is dimension reduction – specifically, the projection of the feature space into a non-linear transformation of that space. In H2O, we can use the h2o.deepfeatures() function to extract non-linear features from an H2O data set using an H2O deep learning model. Here, we grab the features from the first hidden layer.

# Convert train_supervised with autoencoder model to lower-dimensional space
train_reduced_x <- h2o.deepfeatures(ae_model, train_supervised, layer = 1)
dim(train_reduced_x)
# We can use this reduced feature set to train another mode. Let's train a Random Forest.
# First we need to add the response column back to the training frame.
train_reduced <- h2o.cbind(train_reduced_x, train_supervised[,y])

rf1 <- h2o.randomForest(x = names(train_reduced_x), y = y
                        , training_frame = train_reduced
                        , ntrees = 100, seed = 1)
# To evaluate the performace on the test set, we also need to project the test set in to the reduced feature space.
test_reduced_x <- h2o.deepfeatures(ae_model, test, layer = 1)
test_reduced <- h2o.cbind(test_reduced_x, test[,y])

rf_perf <- h2o.performance(rf1, newdata = test_reduced)
h2o.mse(rf_perf)
# In this case, the performance is not better than using the full feature space. However, there are examples where a reducing the feature space would be beneficial. Alternatively, we could simply add these non-linear transformations of the original feature space to the original training (and test) set to obtain an expanded set of features.

#==============Anomaly Detection==================

# We can also use a deep learning autoencoder to identify outliers in a dataset. The h2o.anomaly() function computes the per-row reconstruction error for the test data set (passing it through the autoencoder model and computing mean square error (MSE) for each row).

test_rec_error <- as.data.frame(h2o.anomaly(ae_model, test))
# Convert the test data into its autoencoded representation
test_recon <- predict(ae_model, test)
# Since we will continue to use the MNIST data for our example, we can actually visualize the digits that are considered to be "outliers".

# helper functions for display of handwritten digits
# adapted from http://www.r-bloggers.com/the-essence-of-a-handwritten-digit/

plotDigit <- function(mydata, rec_error) {
  len <- nrow(mydata)
  N <- ceiling(sqrt(len))
  par(mfrow = c(N,N), pty = 's', mar = c(1,1,1,1), xaxt = 'n', yaxt = 'n')
  for (i in 1:nrow(mydata)) {
    colors <- c('white','black')
    cus_col <- colorRampPalette(colors = colors)
    z <- array(mydata[i,], dim = c(28,28))
    z <- z[,28:1]
    class(z) <- "numeric"
    image(1:28, 1:28, z, main = paste0("rec_error: ", round(rec_error[i],4)), col = cus_col(256))
  }
}
plotDigits <- function(data, rec_error, rows) {
  row_idx <- sort(order(rec_error[,1],decreasing=F)[rows])
  my_rec_error <- rec_error[row_idx,]
  my_data <- as.matrix(as.data.frame(data[row_idx,]))
  plotDigit(my_data, my_rec_error)
}
# Let's plot the 6 digits with the lowest reconstruction error. First we plot the reconstruction, then the original scanned images.
plotDigits(test_recon, test_rec_error, c(1:6)) # autoencoded representation
plotDigits(test, test_rec_error, c(1:6))

# Now we plot the 6 digits with the highest reconstruction error – these are the biggest outliers.

plotDigits(test_recon, test_rec_error, c(9995:10000))
plotDigits(test, test_rec_error, c(9995:10000))



