from Compressor import *

quandl_codes = ["YAHOO/INDEX_AEX",
                "YAHOO/INDEX_ATX",
                "YAHOO/INDEX_AXJO",
                "YAHOO/INDEX_RUI",
                "YAHOO/INDEX_IBEX",
                "YAHOO/INDEX_BVSP",
                "YAHOO/INDEX_MXY"]

run_compression(quandl_codes, 500, "Compression Results E.csv")
