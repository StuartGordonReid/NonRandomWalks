from DataPreprocessor import *
comparisons = []
raw = log_returns(get_raw_data("YAHOO/INDEX_NYA"))
for r in range(0, 30):
    comparisons.append(shuffle_data(get_raw_data("YAHOO/INDEX_NYA")))
comp = pandas.DataFrame(comparisons)
comp.transpose().to_csv("Comparison.csv")
