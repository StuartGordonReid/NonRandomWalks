from NNCompressorBinary import *

# Every 2nd from 2 weeks to 1 month
windows = [12, 14, 16, 18, 20]

# Frequency of returns 1 to 5-daily.
decimations = [6, 7, 8, 9, 10]

codes = ["YAHOO/INDEX_NYA", "YAHOO/INDEX_GSPTSE", "YAHOO/INDEX_HSI", "YAHOO/INDEX_MID",
         "YAHOO/INDEX_XOI", "YAHOO/INDEX_N225", "YAHOO/INDEX_AORD"]

print("Number of permutations = ",
      len(windows) * len(decimations))

numpy.random.seed(seed=0)
compression_test(sims=35,
                 quandl_codes=codes,
                 window_sizes=windows,
                 decimation_levels=decimations,
                 compression_rate=0.86,
                 activation='sigmoid',
                 optimizer='RMSprop',
                 sparcity='l2',
                 prefix_name='rmsprop_p4',
                 max_epochs=2000)
