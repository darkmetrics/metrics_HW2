import pandas as pd
import requests
import apimoex
import os

from functools import reduce, partial
from itertools import repeat
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor
from time import perf_counter


def get_tqbr_stocks():
    '''
    Returns list of tickers of all stocks traded in TQBR (i. e. T+) regime
    '''

    request_url = ('https://iss.moex.com/iss/engines/stock/'
                   'markets/shares/boards/TQBR/securities.json')
    arguments = {'securities.columns': ('SECID')}

    with requests.Session() as session:
        iss = apimoex.ISSClient(session, request_url, arguments)
        data = iss.get()
        tqbr_stocks = [x['SECID'] for x in data['securities']]

        return tqbr_stocks


def check_tqbr(ticker):
    '''
    Raises ValueError if ticker is written incorrectly or is traded not in TQBR regime
    '''
    tqbr_list = get_tqbr_stocks()
    # if multiple tickers passed
    if isinstance(ticker, list):
        return list(map(check_tqbr, ticker))
    else:
        if ticker not in tqbr_list:
            raise ValueError('Тикер, который вы ввели, не торгуется в основном режиме (T+) на ММВБ.'
                             'Либо вы неправильно ввели тикер, либо измените режим торгов!')


def concurrent_executor(f, iterable):
    with ThreadPoolExecutor() as exec:
        results = list(exec.map(f, iterable))
    return results


def get_historical_data(ticker, start_date: str, end_date: str, allow_na=True):
    '''
    Returns daily close data for TQBR regime
    :param allow_na: for instance, start_date is 2014-01-01, and some of tickers began to trade at 2014-06-01. Than if
    allow_na=True the data for such tickers over the period between 2014-01-01 and 2014-06-01 will be filled with NaNs
    :param ticker: str (one ticker) or list of strings
    :param start_date: str (yyyy-mm-dd)
    :param end_date: str (yyyy-mm-dd)
    '''

    # check whether ticker supplied is correct and trades in TQBR regime
    check_tqbr(ticker)

    # if multiple tickers are submitted in function
    if isinstance(ticker, list):

        args = ((x, start_date, end_date) for x in ticker)

        # if nans in data are allowed
        if allow_na:
            raw = list(concurrent_executor(lambda p: get_historical_data(*p), args))
            out = reduce(lambda left, right: pd.merge(left, right, on=['TRADEDATE'], how='left'),
                         list(sorted(raw, key=lambda x: x.shape[0], reverse=True)))
            return out

        # otherwise cut data to have no  NaNs
        out = reduce(lambda left, right: pd.merge(left, right, on=['TRADEDATE']),
                     concurrent_executor(lambda p: get_historical_data(*p),
                                         args))
        return out

    # get data for one security
    else:
        with requests.Session() as session:
            data = apimoex.get_board_history(session,
                                             security=ticker,
                                             start=start_date,
                                             end=end_date,
                                             columns=('TRADEDATE', 'CLOSE'))
            df = pd.DataFrame(data)
            df.set_index('TRADEDATE', inplace=True)
            df.columns = [ticker]

        return df


if __name__ == '__main__':
    tickers = ['FIVE', 'MGNT', 'LNTA', 'MVID']
    start = '2014-09-01'
    end = datetime.now().strftime('%Y-%m-%d')

    tic = perf_counter()
    df = get_historical_data(tickers, start, end)
    print(df.tail())
    toc = perf_counter()
    print(f'Downloaded data in {toc - tic:0.4f} seconds')

    fpath = os.getcwd().replace('\\', '/')[:-7] + 'data/daily_prices.csv'
    df.to_csv(fpath)
    print('Downloaded data and saved it to csv')
