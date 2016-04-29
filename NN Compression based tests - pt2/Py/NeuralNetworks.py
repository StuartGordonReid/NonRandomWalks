from keras.models import Sequential
from keras.layers.core import Dense
from DataPreprocessor import *


def autoencoder_4(code, ws, dc, sims, epochs, activation, seed):
    numpy.random.seed(seed=seed)

    x = preprocess_data(code, dc, ws)
    l1 = int(ws * 0.9)
    l2 = int(l1 * 0.9)

    train = numpy.asarray(x)
    model = Sequential()
    model.add(Dense(l1, input_dim=ws, activation=activation))
    model.add(Dense(l2, input_dim=l1, activation=activation))
    model.add(Dense(l1, input_dim=l2, activation=activation))
    model.add(Dense(ws, input_dim=l1, activation=activation))

    model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
    model.fit(train, train, batch_size=32, nb_epoch=epochs, verbose=4)
    loss, accuracy = model.evaluate(train, train, batch_size=20)
    mean, stdev = autoencoder_4sig(x, ws, sims, epochs, activation, seed)
    return accuracy, mean, stdev


def autoencoder_4sig(x, ws, sims, epochs, activation, seed):
    l1 = int(ws * 0.9)
    l2 = int(l1 * 0.9)
    accuracies = []
    for j in range(0, sims):
        numpy.random.seed(seed=(seed*(j + 1)))
        x = benchmark_data(x.size, ws)
        train = numpy.asarray(x)

        numpy.random.seed(seed=seed)
        model = Sequential()
        model.add(Dense(l1, input_dim=ws, activation=activation))
        model.add(Dense(l2, input_dim=l1, activation=activation))
        model.add(Dense(l1, input_dim=l2, activation=activation))
        model.add(Dense(ws, input_dim=l1, activation=activation))
        model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
        model.fit(train, train, batch_size=32, nb_epoch=epochs, verbose=4)
        loss, accuracy = model.evaluate(train, train, batch_size=20)
        accuracies.append(float(accuracy))

    accuracies = numpy.array(accuracies)
    mean_accuracy = numpy.mean(accuracies)
    stdev_accuracy = numpy.std(accuracies)
    return mean_accuracy, stdev_accuracy


dji_mean, rand_mean, rand_std = autoencoder_4('YAHOO/INDEX_DJI', 32, 2, 55, 1000, 'sigmoid', 1987)
print("Market score " + str(dji_mean) +
      "; Random score = " + str(rand_mean) +
      "; Random stdev = " + str(rand_std))
