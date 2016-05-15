import math
from keras.models import Sequential
from keras.layers.core import Dense
from keras.regularizers import l1, l2, l1l2
from DataPreprocessor import *
from keras.optimizers import sgd


def autoencoder_binary(train, tests, valid, ws, compression_rate, max_epochs, activation, sparcity, optimizer):
    # Compute the layer sizes.
    layer1 = int(math.floor(ws * compression_rate))
    layer2 = int(math.floor(layer1 * compression_rate))

    # Print out the shape of the autoencoder
    print("____Architecture: ",
          str(ws), "->",
          # str(layer1), "->",
          str(layer2), "->",
          # str(layer1), "->",
          str(ws))

    # Construct a Keras model.
    model = Sequential()
    regular = None
    if sparcity == 'l1':
        regular = l1()
    elif sparcity == 'l2':
        regular = l2()
    elif sparcity == 'l1l2':
        regular = l1l2()

    # Add the first set of connections to the network
    model.add(Dense(layer2, input_dim=ws, activation=activation, W_regularizer=regular))
    # model.add(Dense(layer2, input_dim=layer1, activation=activation, W_regularizer=regular))
    # model.add(Dense(layer1, input_dim=layer2, activation=activation, W_regularizer=regular))
    model.add(Dense(ws, input_dim=layer2, activation=activation, W_regularizer=regular))

    if optimizer == "sgd-momentum":
        optimizer = sgd(nesterov=True)

    # Compile the model using binary crossentropy and rmsprop optimization.
    model.compile(loss='categorical_crossentropy', optimizer=optimizer, metrics=['accuracy'])

    # Train the model using early stopping.
    best_loss = math.pow(ws, 2)
    keep_training = True
    epochs_done = 0
    while keep_training:
        epochs_done += 5
        print("____Epochs done = " + str(epochs_done) +
              "; best loss = " + str(best_loss))
        # Fit the model using 64 additional epochs.
        model.fit(train, train, batch_size=32, nb_epoch=5, verbose=0)
        loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=32, verbose=0)
        # If the accuracy deteriorates or the epochs done exceeds the max epochs stop training.
        # if loss_tests > best_loss or epochs_done >= max_epochs:
        if epochs_done >= max_epochs:
            keep_training = False
        else:
            best_loss = loss_tests

    # Compute the performance metrics and then compute their significance.
    loss_train, accuracy_train = model.evaluate(train, train, batch_size=32, verbose=0)
    loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=32, verbose=0)
    loss_valid, accuracy_valid = model.evaluate(valid, valid, batch_size=32, verbose=0)

    # Print out some intermediary values for inspection during runtime.
    print("____" + str(loss_train) + ";" + str(loss_tests) + ";" + str(loss_valid))
    print("____" + str(accuracy_train) + ";" + str(accuracy_tests) + ";" + str(accuracy_valid))
    return loss_train, loss_tests, loss_valid, accuracy_train, accuracy_tests, accuracy_valid


def compression_test(sims, quandl_codes, window_sizes, decimation_levels, compression_rate,
                     activation, sparcity, optimizer, prefix_name, max_epochs=2000):
    accs_function_results = []
    loss_function_results = []

    names = ['window_meta', 'decimation_meta']
    for code in quandl_codes:
        names.extend([code + "_train",
                      code + "_tests",
                      code + "_valid"])

    for r in range(0, sims):
        names.extend(["sim_" + str(r) + "_train",
                      "sim_" + str(r) + "_tests",
                      "sim_" + str(r) + "_valid"])

    len_bindata = 0
    for ws in window_sizes:
        for dc in decimation_levels:
            print("Testing window size " + str(ws))
            print("Testing decimation level " + str(dc))
            this_result_loss = [ws, dc]
            this_result_accs = [ws, dc]

            # Analyze the market data.
            for code in quandl_codes:
                print("__Compressing Market " + code)
                # Generate the data we need to run the tests.
                bindata, train, tests, valid = get_nn_data_binary(code, dc, ws)
                len_bindata = len(bindata)
                # Compute the results.
                loss_train, loss_tests, loss_valid, accuracy_train, accuracy_tests, accuracy_valid = \
                    autoencoder_binary(train, tests, valid, ws, compression_rate,
                                       max_epochs, activation, sparcity, optimizer)
                # Add the results to the lists of results.
                this_result_loss.extend([loss_train, loss_tests, loss_valid])
                this_result_accs.extend([accuracy_train, accuracy_tests, accuracy_valid])

            # Analyze the benchmark random data.
            for r in range(0, sims):
                print("__Compression Random Benchmark " + str(r))
                bindata, train, tests, valid = get_nn_benchmark_data_binary(len_bindata, ws)
                loss_train, loss_tests, loss_valid, accuracy_train, accuracy_tests, accuracy_valid = \
                    autoencoder_binary(train, tests, valid, ws, compression_rate,
                                       max_epochs, activation, sparcity, optimizer)
                # Add the results to the lists of results.
                this_result_loss.extend([loss_train, loss_tests, loss_valid])
                this_result_accs.extend([accuracy_train, accuracy_tests, accuracy_valid])

            loss_function_results.append(this_result_loss)
            accs_function_results.append(this_result_accs)

            # Write out the loss function results at this point in time.
            loss_df = pandas.DataFrame(loss_function_results)
            loss_df.columns = names
            loss_df[[col for col in loss_df.columns if
                     "train" in col or "meta" in col]].to_csv("loss_" + prefix_name + "_train.csv")
            loss_df[[col for col in loss_df.columns if
                     "tests" in col or "meta" in col]].to_csv("loss_" + prefix_name + "_tests.csv")
            loss_df[[col for col in loss_df.columns if
                     "valid" in col or "meta" in col]].to_csv("loss_" + prefix_name + "_valid.csv")

            # Write out the accuracy function results at this point in time.
            accs_df = pandas.DataFrame(accs_function_results)
            accs_df.columns = names
            accs_df[[col for col in accs_df.columns if
                     "train" in col or "meta" in col]].to_csv("accs_" + prefix_name + "_train.csv")
            accs_df[[col for col in accs_df.columns if
                     "tests" in col or "meta" in col]].to_csv("accs_" + prefix_name + "_tests.csv")
            accs_df[[col for col in accs_df.columns if
                     "valid" in col or "meta" in col]].to_csv("accs_" + prefix_name + "_valid.csv")

