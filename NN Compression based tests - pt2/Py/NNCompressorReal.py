import math
from keras.models import Sequential
from keras.layers.core import Dense
from keras.regularizers import l1, l2, l1l2
from DataPreprocessor import *
from keras.optimizers import sgd
from keras import backend as K


def tanh(x):
    return K.tanh(x)


def autoencoder_real(train, tests, valid, ws, compression_rate, max_epochs, sparcity, optimizer):
    # Compute the layer sizes.
    layer1 = int(math.floor(ws * compression_rate))
    layer2 = int(math.floor(layer1 * compression_rate))

    # Print out the shape of the autoencoder
    print("____Architecture: ",
          str(ws), "->",
          str(layer1), "->",
          str(layer2), "->",
          str(layer1), "->",
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
    model.add(Dense(layer1, input_dim=ws, activation=tanh, W_regularizer=regular))
    model.add(Dense(layer2, input_dim=layer1, activation=tanh, W_regularizer=regular))
    model.add(Dense(layer1, input_dim=layer2, activation=tanh, W_regularizer=regular))
    model.add(Dense(ws, input_dim=layer1, activation=tanh, W_regularizer=regular))

    if optimizer == "sgd-momentum":
        optimizer = sgd(nesterov=True)

    # Compile the model using binary crossentropy and rmsprop optimization.
    model.compile(loss='mean_squared_error', optimizer=optimizer, metrics=['accuracy'])

    # Train the model using early stopping.
    best_loss = math.pow(ws, 2)
    keep_training = True
    epochs_done = 0
    while keep_training:
        epochs_done += 50
        # Fit the model using 64 additional epochs.
        model.fit(train, train, batch_size=32, nb_epoch=50, verbose=0)
        loss, accuracy = model.evaluate(train, train, batch_size=32, verbose=0)
        print("____EPOCHS = " + str(epochs_done) + "; LOSS = " + str(loss))
        loss, accuracy = model.evaluate(tests, tests, batch_size=32, verbose=0)
        # If the accuracy deteriorates or the epochs done exceeds the max epochs stop training.
        if loss > best_loss or epochs_done >= max_epochs:
            keep_training = False
        else:
            best_loss = loss

    # Compute the performance metrics and then compute their significance.
    loss_train, accuracy_train = model.evaluate(train, train, batch_size=32, verbose=0)
    loss_tests, accuracy_tests = model.evaluate(tests, tests, batch_size=32, verbose=0)
    loss_valid, accuracy_valid = model.evaluate(valid, valid, batch_size=32, verbose=0)

    # Print out some intermediary values for inspection during runtime.
    print("____" + str(loss_train) + ";" + str(loss_tests) + ";" + str(loss_valid))
    print("____" + str(accuracy_train) + ";" + str(accuracy_tests) + ";" + str(accuracy_valid))
    return loss_valid, accuracy_valid


def compression_test_real(sims, code, window_sizes, compression_rate, sparcity, optimizer, max_epochs,
                          start_date, end_date, collapse, file_out, in_loss, in_accs):
    # Column names for the output data frames.
    names = ["code", "compression", "sparcity", "optimizer",
             "starting", "ending", "frequency", "window", "ACTUAL"]
    for sim in range(0, sims):
        names.append("SIM" + str(sim))

    all_losses = in_loss
    all_accuracies = in_accs
    for ws in window_sizes:
        print("Testing window size " + str(ws))
        print("__Compressing " + str(code))
        realdata, train, tests, valid = get_nn_data_real(code, ws, start_date, end_date, collapse)
        loss_valid, accuracy_valid = autoencoder_real(train, tests, valid, ws, compression_rate,
                                                      max_epochs, sparcity, optimizer)
        # Store the results with relevant experiment details.
        losses = [code, compression_rate, sparcity, optimizer,
                  start_date, end_date, collapse, ws, loss_valid]
        # Store the results with relevant experiment details.
        accuracies = [code, compression_rate, sparcity, optimizer,
                      start_date, end_date, collapse, ws, accuracy_valid]

        for sim in range(0, sims):
            print("__Running simulation " + str(sim))
            realdata, train, tests, valid = get_nn_benchmark_data_real(code, ws, start_date, end_date, collapse)
            loss_valid, accuracy_valid = autoencoder_real(train, tests, valid, ws, compression_rate,
                                                          max_epochs, sparcity, optimizer)
            losses.append(loss_valid)
            accuracies.append(accuracy_valid)

        all_losses.append(losses)
        all_accuracies.append(accuracies)

        # Write out the results to the csv file.
        all_losses_df = pandas.DataFrame(all_losses)
        all_losses_df.columns = names
        all_losses_df.to_csv(file_out + "_loss.csv")

        # Write out the results to the csv file.
        all_accuracies_df = pandas.DataFrame(all_accuracies)
        all_accuracies_df.columns = names
        all_accuracies_df.to_csv(file_out + "_accuracy.csv")
    return all_losses, all_accuracies

