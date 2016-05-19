from Compressor import *

quandl_codes = ["YAHOO/INDEX_GDAXI",
                "YAHOO/INDEX_BFX",
                "YAHOO/INDEX_OMX",
                "YAHOO/INDEX_OMXC20_CO",
                "YAHOO/INDEX_MXX",
                "YAHOO/INDEX_DJI"]

run_compression(quandl_codes, 500, "Compression Results D.csv")
