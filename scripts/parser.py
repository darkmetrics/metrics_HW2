import pandas as pd
import requests
import apimoex
import os

from functools import reduce
from datetime import datetime


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


def get_historical_data(ticker, start_date: str, end_date: str):
    '''
    Returns daily close data for TQBR regime
    :param ticker: str (one ticker) or list of strings
    :param start_date: str (yyyy-mm-dd)
    :param end_date: str (yyyy-mm-dd)
    '''

    # check whether ticker supplied is correct and trades in TQBR regime
    check_tqbr(ticker)

    if isinstance(ticker, list):
        out = reduce(lambda left, right: pd.merge(left, right, on=['TRADEDATE'], ),
                     [get_historical_data(ticker=x,
                                          start_date=start_date,
                                          end_date=end_date) for x in ticker])

        out.rename(columns=dict(zip(out.columns, ticker)), inplace=True)
        return out

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
    tickers = ['FIVE', 'MGNT', 'LNTA', 'DSKY', 'MVID']
    start = '2014-09-01'
    end = datetime.now().strftime('%Y-%m-%d')
    df = get_historical_data(tickers, start, end)

    fpath = os.getcwd().replace('\\', '/')[:-7] + 'data/daily_prices.csv'
    df.to_csv(fpath)
    print('Downloaded data and saved it to csv')
