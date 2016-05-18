import time
import quandl
import pandas


def hit_cache(code, start_date, end_date, collapse):
    # Construct a unique string for the parameter values.
    name = "cache/" + str(code).replace("/", "_") + \
           str(start_date).replace("-", "") + \
           str(end_date).replace("-", "") + collapse
    try:
        # Return the data from the cache.
        data = pandas.read_csv(name)
        return data
    except Exception:
        # Download the data from Quandl.
        data = quandl.get(dataset=code, collapse=collapse,
                          start_date=start_date, end_date=end_date)
        data.to_csv(name)
        return data


def get_raw_data(code, start_date, end_date, collapse):
    try:
        quandl.ApiConfig.api_key = "t6Rn1d5N1W6Qt4jJq_zC"
        data = hit_cache(code, start_date, end_date, collapse)
        # Return he rate if possible.
        if 'Rate' in data.columns.values:
            return data[['Rate']]
        # Otherwise return the adjusted close.
        elif 'Adjusted Close' in data.columns.values:
            return data[['Adjusted Close']]
        # Otherwise return the close.
        elif 'Close' in data.columns.values:
            return data[['Close']]
        # Lastly return a generic "Value"
        return data[['Value']]
    except Exception:
        print("__Waiting ...")
        time.sleep(30)  # Sleep for 30 seconds
        print("__Trying again ...")
        return get_raw_data(code, start_date, end_date, collapse)