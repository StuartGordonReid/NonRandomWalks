from Compressor import *

quandl_codes = ["YAHOO/INDEX_GD_AT",
                "YAHOO/INDEX_SML",
                "YAHOO/INDEX_FCHI",
                "YAHOO/INDEX_NSEI",
                "YAHOO/INDEX_JPN",
                "YAHOO/INDEX_SSMI"]

run_compression(quandl_codes, 100, "Compression Results C.csv")
