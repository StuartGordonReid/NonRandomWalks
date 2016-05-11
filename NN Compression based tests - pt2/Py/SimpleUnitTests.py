from DataPreprocessor import *

data = [0.01, -0.01, 0.02, -0.02, 0.03, -0.03, 0.04, -0.04]
print(decimate_data(data, 2) == [0, 0, 0, 0])
print(decimate_data(data, 3) == [0.02, -0.02])
print(decimate_data(data, 4) == [0, 0])

data = quandl.get("CURRFX/USDZAR",
                  collapse='daily',
                  start_date="2015-01-01",
                  end_date="2015-02-01")

expect = [0.014726, -0.000248, -0.003015, 0.001862, -0.003025, -0.009851, -0.009038, -0.000248, 0.003727,
          -0.002542, -0.004975, 0.008629, -0.001262, -0.000248, 0.008009, -0.003686, -0.007290, -0.009470,
          0.002783, -0.000248, 0.001404, 0.010176, -0.000447, -0.002740, 0.007021]

print(abs(numpy.sum(numpy.array(detrend_data(data[["Rate"]])) - numpy.array(expect))) < 0.001)
expect = [1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1]
print(abs(numpy.sum(numpy.array(binarize_data(detrend_data(data[["Rate"]]))) - numpy.array(expect))) < 0.001)
print(window_data(binarize_data(detrend_data(data[["Rate"]]))))