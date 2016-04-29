from keras.models import Sequential
from keras.layers.core import Dense
from DataPreprocessor import *
import math


def autoencoder(code, ws, dc, sims, epochs, activation, seed, layers):
    # Seed the generator so that we have comparable results.
    numpy.random.seed(seed=seed)
    # Download and preprocess the training data.
    x = preprocess_data(code, dc, ws)
    train = numpy.asarray(x)
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
    loss, accuracy = model.evaluate(train, train, batch_size=20, verbose=0)
    mean, stdev = autoencoder_significance(x, ws, sims, epochs, activation, seed, layers)
    return accuracy, mean, stdev


def autoencoder_significance(x, ws, sims, epochs, activation, seed, layers):
    # Compute the sizes of the layers.
    l1 = math.floor(ws * 0.9)
    l2 = math.floor(l1 * 0.9)
    accuracies = []
    for j in range(0, sims):
        # Seed the generator so that our benchmark is different random data for each sim.
        numpy.random.seed(seed=(seed*(j + 1)))
        # Generate a benchmark data set.
        x = benchmark_data(x.size, ws)
        train = numpy.asarray(x)
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
        loss, accuracy = model.evaluate(train, train, batch_size=20, verbose=0)
        accuracies.append(float(accuracy))
    # Compute the averages for testing the significance.
    accuracies = numpy.array(accuracies)
    mean_accuracy = numpy.mean(accuracies)
    stdev_accuracy = numpy.std(accuracies)
    return mean_accuracy, stdev_accuracy


def run_analysis(sims, epochs):
    window_sizes = [4, 8, 16, 32, 64]
    decims = [1, 2, 3, 4]
    levels = [2, 4]
    results = []
    for ws in window_sizes:
        for dc in decims:
            for lv in levels:
                print("Window =", str(ws), " Decimation =", str(dc), " Levels =", str(lv))
                dji_mean, rand_mean, rand_std = autoencoder('YAHOO/INDEX_DJI', ws, dc,
                                                            sims, epochs, 'sigmoid', 1987, lv)
                results.append([ws, dc, lv, dji_mean, rand_mean, rand_std])
                print([ws, dc, lv, dji_mean, rand_mean, rand_std])

    # Print all the information out at the end.
    results_df = pandas.DataFrame(results)
    results_df.columns = ['window size', 'decimation', 'levels',
                          'dow mean', 'rand mean', 'rand stdev']
    return results_df


compression_test = run_analysis(300, 2000)
compression_test.to_csv("results.csv")
print(compression_test)
