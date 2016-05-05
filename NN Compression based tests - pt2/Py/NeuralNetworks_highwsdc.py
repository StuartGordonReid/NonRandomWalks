from keras.models import Sequential
from keras.layers.core import Dense
from DataPreprocessor import *


def autoencoder(train, tests, valid, ws, max_epochs, activation, layers):
    # Compute the layer sizes.
    l1 = int(ws * 0.9)
    l2 = int(l1 * 0.9)

    # Construct a Keras model.
    model = Sequential()

    # Add the layers to the network.
    model.add(Dense(l1, input_dim=ws, activation=activation))
    if layers == 4:
        model.add(Dense(l2, input_dim=l1, activation=activation))
        model.add(Dense(l1, input_dim=l2, activation=activation))
    model.add(Dense(ws, input_dim=l1, activation=activation))

    # Compile the model using binary crossentropy and rmsprop optimization.
    model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])

    # Train the model using early stopping.
    best_accuracy = -1.0
    keep_training = True
    epochs_done = 0
    while keep_training:
        epochs_done += 32
        print("____Epochs done = " + str(epochs_done))
        # Fit the model using 64 additional epochs.
        model.fit(train, train, batch_size=8, nb_epoch=32, verbose=0)
        loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=16, verbose=0)
        # If the accuracy deteriorates or the epochs done exceeds the max epochs stop training.
        if accuracy_tests <= best_accuracy or epochs_done >= max_epochs:
            keep_training = False
        else:
            best_accuracy = accuracy_tests

    # Compute the performance metrics and then compute their significance.
    loss_train, accuracy_train = model.evaluate(train, train, batch_size=8, verbose=0)
    loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=8, verbose=0)
    loss_valid, accuracy_valid = model.evaluate(valid, valid, batch_size=8, verbose=0)
    return loss_train, accuracy_train, loss_tests, accuracy_tests, loss_valid, accuracy_valid


def compression_test(sims, quandl_codes, window_sizes, decimation_levels,
                     max_epochs=4196, activation='sigmoid', seed=1987, layers=4):
    # Seed the results for replication ability.
    numpy.random.seed(seed=seed)

    loss_function_results = []
    accuracy_results = []

    names = ['window_meta', 'decimation_meta',
             'layers_meta', 'seed_meta']
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
        print("Testing window size " + str(ws))
        for dc in decimation_levels:
            print("Testing decimation level " + str(dc))
            this_result_loss = [ws, dc, layers, seed]
            this_result_accs = [ws, dc, layers, seed]

            # Analyze the market data.
            for code in quandl_codes:
                print("__Compressing Market " + code)
                # Generate the data we need to run the tests.
                bindata, train, tests, valid = get_nn_data(code, dc, ws)
                len_bindata = len(bindata)
                # Compute the results.
                loss_train, accuracy_train, loss_tests, accuracy_tests, loss_valid, accuracy_valid = \
                    autoencoder(train, tests, valid, ws, max_epochs, activation, layers)
                # Add the results to the lists of results.
                this_result_loss.extend([loss_train, loss_tests, loss_valid])
                this_result_accs.extend([accuracy_train, accuracy_tests, accuracy_valid])

            # Analyze the benchmark random data.
            for r in range(0, sims):
                print("__Compression Random Benchmark " + str(r))
                bindata, train, tests, valid = get_nn_benchmark_data(len_bindata, ws)
                loss_train, accuracy_train, loss_tests, accuracy_tests, loss_valid, accuracy_valid = \
                    autoencoder(train, tests, valid, ws, max_epochs, activation, layers)
                # Add the results to the lists of results.
                this_result_loss.extend([loss_train, loss_tests, loss_valid])
                this_result_accs.extend([accuracy_train, accuracy_tests, accuracy_valid])

            loss_function_results.append(this_result_loss)
            accuracy_results.append(this_result_accs)

            # Write out the loss function results at this point in time.
            loss_df = pandas.DataFrame(loss_function_results)
            loss_df.columns = names
            loss_df.to_csv("Loss_hwsdc.csv")
            loss_df[[col for col in loss_df.columns if "train" in col or "meta" in col]].to_csv("Loss_Train_hwsdc.csv")
            loss_df[[col for col in loss_df.columns if "tests" in col or "meta" in col]].to_csv("Loss_Tests_hwsdc.csv")
            loss_df[[col for col in loss_df.columns if "valid" in col or "meta" in col]].to_csv("Loss_Valid_hwsdc.csv")

            # Write out the accuracy results at this point in time.
            accs_df = pandas.DataFrame(accuracy_results)
            accs_df.columns = names
            accs_df.to_csv("Accuracy_hwsdc.csv")
            accs_df[[col for col in accs_df.columns if "train" in col or "meta" in col]].to_csv("Accuracy_Train_hwsdc.csv")
            accs_df[[col for col in accs_df.columns if "tests" in col or "meta" in col]].to_csv("Accuracy_Tests_hwsdc.csv")
            accs_df[[col for col in accs_df.columns if "valid" in col or "meta" in col]].to_csv("Accuracy_Valid_hwsdc.csv")


ex_windows = list(range(20, 260, 10))
ex_decimations = list(range(5, 11, 1))
ex_codes = ["YAHOO/INDEX_NYA", "YAHOO/INDEX_GSPTSE", "YAHOO/INDEX_HSI", "YAHOO/INDEX_MID",
            "YAHOO/INDEX_XOI", "YAHOO/INDEX_N225", "YAHOO/INDEX_AORD"]

compression_test(150, ex_codes, ex_windows, ex_decimations)
