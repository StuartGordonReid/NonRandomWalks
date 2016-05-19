from Compressor import *

quandl_codes = ["YAHOO/INDEX_GSPTSE",
                "YAHOO/INDEX_GSPC",
                "YAHOO/INDEX_NYA",
                "YAHOO/INDEX_HSI",
                "YAHOO/INDEX_MID",
                "YAHOO/INDEX_XOI"]

run_compression(quandl_codes, 500, "Compression Results A.csv")
