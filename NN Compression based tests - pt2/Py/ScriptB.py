from Compressor import *

quandl_codes = ["YAHOO/INDEX_N225",
                "YAHOO/INDEX_AORD",
                "YAHOO/INDEX_STOXX50E",
                "YAHOO/INDEX_RUT",
                "YAHOO/INDEX_RUA",
                "YAHOO/INDEX_STI"]

run_compression(quandl_codes, 100, "Compression Results B.csv")

