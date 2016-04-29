from keras.models import Sequential
from keras.layers.core import Dense
from DataPreprocessor import *


def autoencoder_4(real, code, ws, dc, sims, epochs, activation):
    x = preprocess_data(code, dc, ws)
    l1 = int(ws * 0.9)
    l2 = int(l1 * 0.9)
    print(str(ws) +
          ":" + str(l1) +
          ":" + str(l2))

    accuracies = []
    for j in range(0, sims):
        print("Running simulation " + str(j))
        if not real:
            x = benchmark_data(x.size, ws)
        train = numpy.asarray(x)

        model = Sequential()
        model.add(Dense(l1, input_dim=ws, activation=activation))
        model.add(Dense(l2, input_dim=l1, activation=activation))
        model.add(Dense(l1, input_dim=l2, activation=activation))
        model.add(Dense(ws, input_dim=l1, activation=activation))

        model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
        model.fit(train, train, batch_size=32, nb_epoch=epochs, verbose=4)

        loss, accuracy = model.evaluate(train, train, batch_size=500)
        accuracies.append(float(accuracy))

    accuracies = numpy.array(accuracies)
    mean_accuracy = numpy.mean(accuracies)
    stdev_accuracy = numpy.std(accuracies)
    return mean_accuracy, stdev_accuracy


dji_mean, dji_std = autoencoder_4(True, 'YAHOO/INDEX_DJI', 40, 1, 30, 500, 'sigmoid')
dji_b_mean, dji_b_std = autoencoder_4(False, 'YAHOO/INDEX_DJI', 40, 1, 30, 500, 'sigmoid')


print("---------------------------------------")
print("Results: ")
print("---------------------------------------")
print(str(dji_mean), "+-", str(dji_std))
print(str(dji_b_mean), "+-", str(dji_b_std))

