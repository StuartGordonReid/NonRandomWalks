from NeuralNetCompressor import *

windows = list(range(4, 10, 1))
windows.extend(list(range(10, 20, 2)))
windows.extend(list(range(20, 120, 10)))
windows.extend(list(range(120, 260, 20)))

decimations = list(range(1, 10, 1))
decimations.extend(list(range(10, 20, 2)))

codes = ["YAHOO/INDEX_NYA", "YAHOO/INDEX_GSPTSE", "YAHOO/INDEX_HSI", "YAHOO/INDEX_MID",
         "YAHOO/INDEX_XOI", "YAHOO/INDEX_N225", "YAHOO/INDEX_AORD"]

print(len(windows) * len(decimations))

compression_test(sims=35,
                 quandl_codes=codes,
                 window_sizes=windows,
                 decimation_levels=decimations,
                 compression_rate=0.86,
                 activation='hard_sigmoid',
                 sparcity='l1l2',
                 prefix_name='arch8')
