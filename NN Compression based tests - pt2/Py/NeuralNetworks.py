from keras.models import Sequential
from keras.layers.core import Dense
from DataPreprocessor import *
import math


def autoencoder(code, ws, dc, sims, epochs, activation, seed, layers):
    # Seed the generator so that we have comparable results.
    numpy.random.seed(seed=seed)
    # Download and preprocess the training data.
    x = preprocess_data(code, dc, ws)
    train = numpy.asarray(x[1:int(len(x) * 0.8)])
    tests = numpy.asarray(x[int(len(x) * 0.8):len(x)])
    # Compute the sizes of the layers.
    l1 = int(ws * 0.9)
    l2 = int(l1 * 0.9)
    # Construct a model.
    model = Sequential()
    # Add the layers to the network.
    model.add(Dense(l1, input_dim=ws, activation=activation))
    if layers == 4:
        model.add(Dense(l2, input_dim=l1, activation=activation))
        model.add(Dense(l1, input_dim=l2, activation=activation))
    model.add(Dense(ws, input_dim=l1, activation=activation))
    # Compile and train the model with the specified optimization algorithm and metrics.
    model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
    model.fit(train, train, batch_size=32, nb_epoch=epochs, verbose=0)
    # Compute the performance metrics and then compute their significance.
    loss_train, accuracy_train = model.evaluate(train, train, batch_size=128, verbose=0)
    loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=128, verbose=0)
    mean_train, mean_tests, stdev_train, stdev_tests, accs_train, accs_tests = \
        autoencoder_significance(x, ws, sims, epochs, activation, seed, layers)
    return accuracy_train, accuracy_tests, mean_train, mean_tests, stdev_train, stdev_tests, accs_train, accs_tests


def autoencoder_significance(x, ws, sims, epochs, activation, seed, layers):
    # Compute the sizes of the layers.
    l1 = math.floor(ws * 0.9)
    l2 = math.floor(l1 * 0.9)
    accs_train = []
    accs_tests = []
    for j in range(0, sims):
        # Seed the generator so that our benchmark is different random data for each sim.
        numpy.random.seed(seed=(seed*(j + 1)))
        # Generate a benchmark data set.
        x = benchmark_data(x.size, ws)
        train = numpy.asarray(x[1:int(len(x) * 0.8)])
        tests = numpy.asarray(x[int(len(x) * 0.8):len(x)])
        # Seed the generator so that we have comparable results.
        numpy.random.seed(seed=seed)
        # Construct a model.
        model = Sequential()
        # Add the layers to the network.
        model.add(Dense(l1, input_dim=ws, activation=activation))
        if layers == 4:
            model.add(Dense(l2, input_dim=l1, activation=activation))
            model.add(Dense(l1, input_dim=l2, activation=activation))
        model.add(Dense(ws, input_dim=l1, activation=activation))
        # Compile and train the model with the specified optimization algorithm and metrics.
        model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
        model.fit(train, train, batch_size=32, nb_epoch=epochs, verbose=0)
        # Compute the performance metrics and append them to the lists.
        loss_train, accuracy_train = model.evaluate(train, train, batch_size=128, verbose=0)
        loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=128, verbose=0)
        accs_train.append(float(accuracy_train))
        accs_tests.append(float(accuracy_tests))
    # Compute the averages for testing the significance.
    accs_train = numpy.array(accs_train)
    accs_tests = numpy.array(accs_tests)
    mean_train = numpy.mean(accs_train)
    mean_tests = numpy.mean(accs_tests)
    stdev_train = numpy.std(accs_train)
    stdev_tests = numpy.std(accs_tests)
    return mean_train, mean_tests, stdev_train, stdev_tests, accs_train, accs_tests


def run_analysis(code, sims, epochs):
    # Set up the tuning parameters for the randomness test.
    window_sizes = [4, 8, 12, 16, 32, 64]
    decims = [1, 2, 3, 4, 5]
    levels = [2, 4]
    results = []
    for ws in window_sizes:
        for dc in decims:
            for lv in levels:
                print("Window =", str(ws), " Decimation =", str(dc), " Levels =", str(lv))
                # Compute all of the relevant test statistics.
                acc_train, acc_tests, mean_train, mean_tests, stdev_train, stdev_tests, accs_train, accs_tests \
                    = autoencoder(code, ws, dc, sims, epochs, 'sigmoid', 1987, lv)
                this_result = [ws, dc, lv, acc_train, acc_tests, mean_train, mean_tests, stdev_train, stdev_tests]
                # Add each training simulation result to the results vector.
                for sim_result in accs_train:
                    this_result.append(sim_result)
                # Add each testing simulation result to the results vector.
                for sim_result in accs_tests:
                    this_result.append(sim_result)
                # Append the results vector to the set of all results.
                results.append(this_result)
                print(this_result)

    # Print all the information out at the end.
    results_df = pandas.DataFrame(results)
    names = ['window size', 'decimation', 'levels',
             'market accuracy (train)', 'market accuracy (test)',
             'random accuracy (train)', 'random accuracy (test)',
             'random variance (train)', 'random variance (test)']
    # Add the training results for each simulation.
    for sim_name in numpy.repeat("Sim Train", sims):
        names.append(str(sim_name))
    # Add the testing results for each simulation.
    for sim_name in numpy.repeat("Sim Tests", sims):
        names.append(str(sim_name))
    results_df.columns = names
    return results_df


# Run the analysis for the Dow Jones Industrial Average.
dow = run_analysis('YAHOO/INDEX_DJI', 150, 1000)
dow.to_csv("results djia.csv")


# Run the analysis for the S&P 500 stocks index.
sp500 = run_analysis('YAHOO/INDEX_GSPC', 150, 1000)
sp500.to_csv("results sp500.csv")
