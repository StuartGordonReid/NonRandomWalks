import numpy
import random
import quandl
import pandas


def get_raw_data(code, start_date="1985-01-01", end_date="2016-01-01"):
    # Get the raw data from Quandl.
    try:
        quandl.ApiConfig.api_key = "t6Rn1d5N1W6Qt4jJq_zC"
        data = quandl.get(code, collapse='daily', start_date=start_date, end_date=end_date)
        # Return he rate if possible.
        if 'Rate' in data.columns.values:
            return data[['Rate']]
        # Otherwise return the adjusted close.
        elif 'Adjusted Close' in data.columns.values:
            return data[['Adjusted Close']]
        # Lastly return the close.
        return data[['Close']]
    except Exception:
        print("__Waiting ...")
        i = random.uniform(0, 1000001)
        while i < 1000000:
            i = random.uniform(0, 1000001)
        print("__Trying again ...")
        return get_raw_data(code, start_date)


def detrend_data(data):
    # Lag the dataframe by one day for computing the log returns.
    data_lagged = pandas.DataFrame.shift(data, periods=1, freq=None, axis=0)
    logrets = numpy.log(data.ix[2:] / data_lagged.ix[2:])
    # Remove the drift component from the returns.
    return logrets - numpy.mean(logrets)


def decimate_data(logrets, decimation):
    if decimation >= 2:
        logrets = numpy.squeeze(numpy.array(logrets))
        decimated_logrets = []
        for i in range(0, len(logrets) - int(decimation / 2), decimation):
            decimated_logrets.append(logrets[i] + logrets[i + 1])
        return numpy.array(decimated_logrets)
    else:
        return numpy.array(logrets)


def binarize_data(logrets):
    # Replace + returns with 1.
    logrets[logrets > 0] = 1
    # Replace - returns with 0.
    logrets[logrets < 0] = 0
    # Return the data as a numpy array.
    return numpy.squeeze(numpy.asarray(logrets))


def window_data(bindata, window_size=20):
    # Store the windows in a list.
    windows = list()
    n = len(bindata) - window_size
    for ix in range(0, n):
        # Split out the windows.
        windows.append(bindata[ix:(ix + window_size)])
    # Merge the windows into a dataframe.
    return pandas.DataFrame(windows)


def split_data(windows):
    # Split the windows into train, test, and validate.
    train = numpy.asarray(windows[1:int(len(windows) * 0.8)])
    valid = numpy.asarray(windows[int(len(windows) * 0.9):len(windows)])
    tests = numpy.asarray(windows[int(len(windows) * 0.8):int(len(windows) * 0.9)])
    return train, tests, valid


def get_nn_data(code, dc, ws):
    # Download the raw binary data from Quandl.
    bindata = binarize_data(decimate_data(detrend_data(get_raw_data(code)), dc))
    # Split the binary data into windows of size ws and sets.
    train, tests, valid = split_data(window_data(bindata, ws))
    return bindata, train, tests, valid


def get_nn_benchmark_data(len_bindata, ws):
    # Generate ne binary digits randomly.
    bindata = numpy.random.binomial(1, 0.5, len_bindata)
    # Split the binary data into windows of size ws and sets.
    train, tests, valid = split_data(window_data(bindata, ws))
    return bindata, train, tests, valid
