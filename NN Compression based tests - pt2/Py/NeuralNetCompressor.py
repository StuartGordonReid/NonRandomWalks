from keras.models import Sequential
from keras.layers.core import Dense
from keras.regularizers import l1, l2, l1l2
from DataPreprocessor import *


def autoencoder(train, tests, valid, ws, compression_rate, max_epochs, activation, sparcity):
    # Compute the layer sizes.
    layer1 = int(ws * compression_rate)
    layer2 = int(layer1 * compression_rate)

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
    model.add(Dense(layer1, input_dim=ws, activation=activation, W_regularizer=regular))
    model.add(Dense(layer2, input_dim=layer1, activation=activation, W_regularizer=regular))
    model.add(Dense(layer1, input_dim=layer2, activation=activation, W_regularizer=regular))
    model.add(Dense(ws, input_dim=layer1, activation=activation, W_regularizer=regular))

    # Compile the model using binary crossentropy and rmsprop optimization.
    model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])

    # Train the model using early stopping.
    best_accuracy = -1.0
    keep_training = True
    epochs_done = 0
    while keep_training:
        epochs_done += 5
        print("____Epochs done = " + str(epochs_done))
        # Fit the model using 64 additional epochs.
        model.fit(train, train, batch_size=1, nb_epoch=5, verbose=0)
        loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=1, verbose=0)
        # If the accuracy deteriorates or the epochs done exceeds the max epochs stop training.
        if accuracy_tests <= best_accuracy or epochs_done >= max_epochs:
            keep_training = False
        else:
            best_accuracy = accuracy_tests

    # Compute the performance metrics and then compute their significance.
    loss_train, accuracy_train = model.evaluate(train, train, batch_size=1, verbose=0)
    loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=1, verbose=0)
    loss_valid, accuracy_valid = model.evaluate(valid, valid, batch_size=1, verbose=0)
    return loss_train, loss_tests, loss_valid


def compression_test(sims, quandl_codes, window_sizes, decimation_levels, compression_rate,
                     activation, sparcity, prefix_name, max_epochs=2000, seed=1987):
    # Seed the results for replication ability.
    numpy.random.seed(seed=seed)
    loss_function_results = []
    names = ['window_meta', 'decimation_meta', 'seed_meta']
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
            this_result_loss = [ws, dc, seed]

            # Analyze the market data.
            for code in quandl_codes:
                print("__Compressing Market " + code)
                # Generate the data we need to run the tests.
                bindata, train, tests, valid = get_nn_data(code, dc, ws)
                len_bindata = len(bindata)
                # Compute the results.
                loss_train, loss_tests, loss_valid = autoencoder(train, tests, valid, ws, compression_rate,
                                                                 max_epochs, activation, sparcity)
                # Add the results to the lists of results.
                this_result_loss.extend([loss_train, loss_tests, loss_valid])

            # Analyze the benchmark random data.
            for r in range(0, sims):
                print("__Compression Random Benchmark " + str(r))
                bindata, train, tests, valid = get_nn_benchmark_data(len_bindata, ws)
                loss_train, loss_tests, loss_valid = autoencoder(train, tests, valid, ws, compression_rate,
                                                                 max_epochs, activation, sparcity)
                # Add the results to the lists of results.
                this_result_loss.extend([loss_train, loss_tests, loss_valid])

            loss_function_results.append(this_result_loss)

            # Write out the loss function results at this point in time.
            loss_df = pandas.DataFrame(loss_function_results)
            loss_df.columns = names
            loss_df[[col for col in loss_df.columns if
                     "train" in col or "meta" in col]].to_csv(prefix_name + "_train.csv")
            loss_df[[col for col in loss_df.columns if
                     "tests" in col or "meta" in col]].to_csv(prefix_name + "_tests.csv")
            loss_df[[col for col in loss_df.columns if
                     "valid" in col or "meta" in col]].to_csv(prefix_name + "_valid.csv")

