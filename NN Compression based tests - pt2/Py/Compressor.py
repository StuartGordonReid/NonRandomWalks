from Fetcher import *

import numpy
import pandas
import math
import random
from keras.layers import Input, Dense
from keras.models import Model
from keras import regularizers
import matplotlib.pyplot as plt
from random import shuffle


def logrets_to_prices(returns):
    prices = [1.0]
    for t in range(0, len(returns)):
        prices.append(prices[t] * math.exp(returns[t]))
    return numpy.array(prices)


def prices_to_logrets(data, long_ws=120):
    data_lagged = pandas.DataFrame.shift(data, periods=1, freq=None, axis=0)
    logrets = numpy.squeeze(numpy.array(numpy.log(data.ix[2:] / data_lagged.ix[2:])))
    return logrets[0:(len(logrets) - (len(logrets) % long_ws))]


def shuffle_logrets(logrets, long_ws=120):
    shuffled_logrets = []
    for t in range(long_ws, len(logrets) + 1, long_ws):
        subsequence = logrets[(t - long_ws):t]
        shuffle(subsequence)
        shuffled_logrets.extend(subsequence)
    return numpy.array(shuffled_logrets)


def logrets_to_inputs(logrets):
    logrets = -logrets
    logrets *= 10
    inputs = numpy.exp(logrets)
    inputs += 1
    return numpy.array(1 / inputs)


def inputs_to_logrets(inputs):
    logrets = 1 / inputs
    logrets -= 1
    logrets = numpy.log(logrets)
    logrets /= 10
    return numpy.array(-logrets)


def get_inputs(code, start_date, end_date, collapse, shuffle, input_dimension):
    logrets = prices_to_logrets(get_raw_data(code, start_date, end_date, collapse))
    logrets = shuffle_logrets(logrets) if shuffle else logrets
    inputs = logrets_to_inputs(logrets)
    return inputs[0:(len(inputs) - (len(inputs) % input_dimension))]


def split_inputs(code, start_date, end_date, collapse, shuffle, input_dimension):
    keep = int(720 / input_dimension) if collapse == "daily" else int(144 / input_dimension)
    inputs = get_inputs(code, start_date, end_date, collapse, shuffle, input_dimension)
    # Split inputs into the training set and
    train = inputs[0:len(inputs) - (keep * input_dimension)]
    train = train.reshape(int(len(train) / input_dimension), input_dimension)
    # Split the inputs into a out of sample validation set.
    valid = inputs[len(inputs) - (keep * input_dimension):len(inputs)]
    valid = valid.reshape(int(len(valid) / input_dimension), input_dimension)
    return train, valid


def tf_autoencoder(code, start_date, end_date, collapse, do_shuffle, input_dimension,
                   latent_dimension, epochs, batch, loss, optimizer):
    train, valid = split_inputs(code, start_date, end_date, collapse, do_shuffle, input_dimension)
    the_fed = regularizers.activity_l1(10e-5)
    input_seq = Input(shape=(input_dimension, ))
    encoded = Dense(latent_dimension, activation='relu', activity_regularizer=the_fed)(input_seq)
    decoded = Dense(input_dimension, activation='sigmoid')(encoded)
    autoencoder = Model(input=input_seq, output=decoded)
    autoencoder.compile(optimizer=optimizer, loss=loss)
    autoencoder.fit(train, train, nb_epoch=epochs, batch_size=batch, verbose=0)
    train_reconstructed = autoencoder.predict(train)
    valid_reconstructed = autoencoder.predict(valid)
    return train, train_reconstructed, valid, valid_reconstructed


def get_result(model, code, start_date, end_date, collapse, shuffle, input_dimension,
               latent_dimension, epochs, batch, loss, optimizer):
    train, train_predicted, valid, valid_predicted = model(code, start_date, end_date, collapse, shuffle,
                                                           input_dimension, latent_dimension,
                                                           epochs, batch, loss, optimizer)
    stats = get_stats(train, train_predicted, valid, valid_predicted)
    sim = "Actual" if not shuffle else "Simulation"
    return [code, sim, loss, epochs, optimizer] + stats


def get_stats(train, train_reconstructed, valid, valid_reconstructed, show_plots=False):
    # Construct the reconstructed equity curve by the NN.
    train_reconstructed_1d = inputs_to_logrets(train_reconstructed.reshape((train_reconstructed.size, )))
    valid_reconstructed_1d = inputs_to_logrets(valid_reconstructed.reshape((valid_reconstructed.size, )))
    reconstructed = numpy.array(list(train_reconstructed_1d) + list(valid_reconstructed_1d))
    reconstructed_equity = logrets_to_prices(reconstructed)
    # Construct the true equity curve.
    train_1d = inputs_to_logrets(train.reshape((train.size, )))
    valid_1d = inputs_to_logrets(valid.reshape((valid.size, )))
    actuals = numpy.array(list(train_1d) + list(valid_1d))
    actuals_equity = logrets_to_prices(actuals)
    # Compute the test stats on the equity curves.
    diff = numpy.array(valid_1d - valid_reconstructed_1d)
    sse = numpy.sum(diff ** 2)
    sae = numpy.sum(numpy.abs(diff))
    mse = numpy.mean(diff ** 2)
    mae = numpy.mean(numpy.abs(diff))
    # Plot the two
    if show_plots:
        plt.plot(reconstructed_equity)
        plt.plot(actuals_equity)
        plt.show()

        plt.plot(logrets_to_prices(valid_reconstructed_1d))
        plt.plot(logrets_to_prices(valid_1d))
        plt.show()
    # Return the relevant stats.
    return [sse, sae, mse, mae]


def seed_and_discard(discards=4095):
    numpy.random.seed(0)
    random.seed(0)
    for i in range(0, discards):
        random.uniform(0, 1)
        numpy.random.uniform(size=1)


def write_results(outs, file_out):
    outs_df = pandas.DataFrame(outs)
    outs_df.columns = ["Index", "Type", "Objective", "Epochs",
                       "Optimizer", "SSE", "SAE", "MSE", "MAE"]
    outs_df.to_csv(file_out)
    print(outs_df)


def run_compression(codes, simulations, file_out):
    outs = []
    for code in codes:
        seed_and_discard()
        for sim in range(0, simulations):
            do_shuffle = True if sim > 0 else False
            outs.append(get_result(tf_autoencoder, code, "1995-01-01", "2016-01-01",
                                   "daily", do_shuffle, 20, 10, 12000, 128, "mse", "adam"))
            write_results(outs, file_out)

