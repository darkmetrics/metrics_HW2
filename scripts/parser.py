import pandas as pd
import requests
import apimoex
import os

from functools import reduce

# get all stocks traded in TQBR regime
request_url = ('https://iss.moex.com/iss/engines/stock/'
               'markets/shares/boards/TQBR/securities.json')
arguments = {'securities.columns': ('SECID')}


def get_tqbr_stocks():
    '''
    Returns list of tickers of all stocks traded in TQBR (i. e. T+) regime
    '''
    with requests.Session() as session:
        iss = apimoex.ISSClient(session, request_url, arguments)
        data = iss.get()
        tqbr_stocks = [x['SECID'] for x in data['securities']]

        return tqbr_stocks


def check_tqbr(ticker):
    tqbr_list = get_tqbr_stocks()
    # if multiple tickers passed
    if isinstance(ticker, list):
        return list(map(check_tqbr, ticker))
    else:
        if ticker not in tqbr_list:
            raise ValueError('Тикер, который вы ввели, не торгуется в основном режиме'
                             '(T+) на ММВБ. Попробуйте ещё раз, либо измените режим торгов!')


def get_ticker_history(ticker, start_date: str, end_date: str):
    with requests.Session() as session:
        data = apimoex.get_board_history(session,
                                         security=ticker,
                                         start=start_date,
                                         end=end_date,
                                         columns=('TRADEDATE', 'CLOSE'))
        df = pd.DataFrame(data)
        # df.set_index('TRADEDATE', inplace=True)

    return df


def get_historical_data(ticker, start_date: str, end_date: str):
    '''
    Returns daily close data for TQBR regime
    :param ticker: str (one ticker)
    :param start_date: str (yyyy-mm-dd)
    :param end_date: str (yyyy-mm-dd)
    '''

    # check whether ticker supplied is traded in TQBR regime
    check_tqbr(ticker)

    if isinstance(ticker, list):
        out = reduce(lambda left, right: pd.merge(left, right, on=['TRADEDATE'], ),
                     [get_ticker_history(ticker=x,
                                         start_date=start_date,
                                         end_date=end_date) for x in ticker])

        out.rename(columns=dict(zip(out.columns, ['TRADEDATE'] + ticker)), inplace=True)

        return out
    return get_ticker_history(ticker=ticker, start_date=start_date, end_date=end_date)


if __name__ == '__main__':
    df = get_historical_data(['SNGSP', 'ALRS'], start_date='2019-06-09', end_date='2019-07-17')
    print(df)
