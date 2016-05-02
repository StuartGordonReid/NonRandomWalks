import numpy
import quandl
import pandas


def get_raw_data(code, start_date="1985-01-01"):
    # Get the raw data from Quandl.
    quandl.ApiConfig.api_key = "t6Rn1d5N1W6Qt4jJq_zC"
    data = quandl.get(code, collapse='daily', start_date=start_date)
    # Return he rate if possible.
    if 'Rate' in data.columns.values:
        return data[['Rate']]
    # Otherwise return the adjusted close.
    elif 'Adjusted Close' in data.columns.values:
        return data[['Adjusted Close']]
    # Lastly return the close.
    return data[['Close']]


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


def preprocess_data(code, decimation, window_size):
    # Get raw data, detrend it, binarize it, and window it.
    return window_data(binarize_data(decimate_data(detrend_data(get_raw_data(code)), decimation)), window_size)


def benchmark_data(n, window_size):
    # Generate ne binary digits randomly.
    ne = int(n / window_size) + window_size
    bindata = numpy.random.binomial(1, 0.5, ne)
    # Window the digits and return dataframe.
    return window_data(bindata, window_size)
