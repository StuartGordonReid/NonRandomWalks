from keras.models import Sequential
from keras.layers.core import Dense
from DataPreprocessor import *
import math


def autoencoder(code, ws, dc, sims, max_epochs, activation, seed, layers):
    print("__Testing real data")

    # Seed the generator so that we have comparable results.
    numpy.random.seed(seed=seed)

    # Download, preprocess, and split the market data.
    x = preprocess_data(code, dc, ws)
    train = numpy.asarray(x[1:int(len(x) * 0.8)])
    valid = numpy.asarray(x[int(len(x) * 0.9):len(x)])
    tests = numpy.asarray(x[int(len(x) * 0.8):int(len(x) * 0.9)])

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

    # Compute the same variables on randomly generated data and return everything to the calling function.
    mean_train, mean_tests, mean_valid, stdev_train, stdev_tests, stdev_valid, accs_train, accs_tests, accs_valid = \
        autoencoder_significance(x, ws, sims, max_epochs, activation, seed, layers)
    return accuracy_train, accuracy_tests, accuracy_valid, mean_train, mean_tests, mean_valid, stdev_train, stdev_tests, stdev_valid, accs_train, accs_tests, accs_valid


def autoencoder_significance(x, ws, sims, max_epochs, activation, seed, layers):
    print("__Testing random data.")

    # Compute the layer sizes.
    l1 = math.floor(ws * 0.9)
    l2 = math.floor(l1 * 0.9)

    # Track the accuracies of each sim.
    accs_train = []
    accs_tests = []
    accs_valid = []

    # For number of simulations.
    for j in range(0, sims):
        print("____Simulation " + str(j))

        # Seed the generator so that our benchmark is different random data for each sim.
        numpy.random.seed(seed=(seed*(j + 1)))

        # Generate a benchmark data set and split it.
        x = benchmark_data(x.size, ws)
        train = numpy.asarray(x[1:int(len(x) * 0.8)])
        valid = numpy.asarray(x[int(len(x) * 0.9):len(x)])
        tests = numpy.asarray(x[int(len(x) * 0.8):int(len(x) * 0.9)])

        # Seed the generator again so that we have comparable results.
        numpy.random.seed(seed=seed)

        # Construct a model.
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
            print("______Epochs done = " + str(epochs_done))
            model.fit(train, train, batch_size=8, nb_epoch=32, verbose=0)
            loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=16, verbose=0)
            if accuracy_tests <= best_accuracy or epochs_done >= max_epochs:
                keep_training = False
            else:
                best_accuracy = accuracy_tests

        # Compute the performance metrics on the training set and append them to the lists.
        loss_train, accuracy_train = model.evaluate(train, train, batch_size=8, verbose=0)
        accs_train.append(float(accuracy_train))

        # Compute the performance metrics on the testing set and append them to the list.
        loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=8, verbose=0)
        accs_tests.append(float(accuracy_tests))

        # Compute the performance metrics on the validation set and append them to the list.
        loss_valid, accuracy_valid = model.evaluate(valid, valid, batch_size=8, verbose=0)
        accs_valid.append(float(accuracy_valid))

    # Convert the lists into numpy arrays.
    accs_train = numpy.array(accs_train)
    accs_tests = numpy.array(accs_tests)
    accs_valid = numpy.array(accs_valid)

    # Compute the averages for significance.
    mean_train = numpy.mean(accs_train)
    mean_tests = numpy.mean(accs_tests)
    mean_valid = numpy.mean(accs_valid)

    # Compute the variance for significance.
    stdev_train = numpy.std(accs_train)
    stdev_tests = numpy.std(accs_tests)
    stdev_valid = numpy.std(accs_valid)

    # Return all of the values to the calling function.
    return mean_train, mean_tests, mean_valid, stdev_train, stdev_tests, stdev_valid, accs_train, accs_tests, accs_valid


def run_analysis(code, sims, epochs):
    # Set up the tuning parameters for the randomness test.
    window_sizes = [4, 6, 8, 10, 12, 14, 16, 32]
    decims = [1, 2, 3, 4, 5]
    levels = [2, 4]
    results = []
    for ws in window_sizes:
        for dc in decims:
            for lv in levels:
                print(code, "; Window =", str(ws), " Decimation =", str(dc), " Levels =", str(lv))

                # Compute all of the relevant test statistics.
                acc_train, acc_tests, acc_valid, mean_train, mean_tests, mean_valid, stdev_train, stdev_tests, stdev_valid, accs_train, accs_tests, accs_valid \
                    = autoencoder(code, ws, dc, sims, epochs, 'sigmoid', 1987, lv)
                this_result = [ws, dc, lv, acc_train, acc_tests, acc_valid, mean_train,
                               mean_tests, mean_valid, stdev_train, stdev_tests, stdev_valid]

                # Add each training simulation result to the results vector.
                for sim_result in accs_train:
                    this_result.append(sim_result)

                # Add each testing simulation result to the results vector.
                for sim_result in accs_tests:
                    this_result.append(sim_result)

                # Add each validation simulation result to the results vector.
                for sim_result in accs_valid:
                    this_result.append(sim_result)

                # Append the results vector to the set of all results.
                results.append(this_result)
                print(this_result)

        # Write out a temporary file with the results but no headings. Just in case ...
        results_temp = pandas.DataFrame(results)
        results_temp.to_csv(code.replace("YAHOO/", "") +
                            "_temp_" + str(ws) + ".csv")

    # Print all the information out at the end.
    results_df = pandas.DataFrame(results)
    names = ['window size', 'decimation', 'levels',
             'market accuracy (train)', 'market accuracy (test)', 'market accuracy (valid)',
             'random accuracy (train)', 'random accuracy (test)', 'random accuracy (valid)',
             'random variance (train)', 'random variance (test)', 'random variance (valid)']

    # Add the training results for each simulation.
    for sim_name in numpy.repeat("Sim Train", sims):
        names.append(str(sim_name))

    # Add the testing results for each simulation.
    for sim_name in numpy.repeat("Sim Tests", sims):
        names.append(str(sim_name))

    # Add the validation results for each simulation.
    for sim_name in numpy.repeat("Sim Valid", sims):
        names.append(str(sim_name))

    results_df.columns = names
    return results_df


# "YAHOO/INDEX_GSPC",
quandl_codes = ["YAHOO/INDEX_AORD",
                "YAHOO/INDEX_N225",
                "YAHOO/INDEX_XOI",
                "YAHOO/INDEX_MID"]


for qcode in quandl_codes:
    print("-----------------------------------------------------------------")
    code_results_name = qcode.replace("YAHOO/", "") + "_results.csv"
    code_results = run_analysis(qcode, 60, 4096)
    code_results.to_csv(code_results_name)


# # Run the analysis for the Dow Jones Industrial Average.
# dow = run_analysis('YAHOO/INDEX_DJI', 150, 1000)
# dow.to_csv("results djia.csv")


# # Run the analysis for the S&P 500 stocks index.
# sp500 = run_analysis('YAHOO/INDEX_GSPC', 150, 1000)
# sp500.to_csv("results sp500.csv")


