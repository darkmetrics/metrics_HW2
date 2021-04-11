# metrics_HW2

<img src="https://img.shields.io/pypi/pyversions/apimoex?color=orange&label=Python%20version&logoColor=blueviolet">

Задача второй домашки - оценить гарчи на российских акциях и пострить для акций VaR на основе GARCH. Наш сектор - retail.
Предлагаю моделировать в R,  так как в тз задания требуется реализовать модель ARIMA-GARCH, а в питоновских библиотеках нет реализации объединённой модели - отдельно ARIMA, отдельно GARCH.
Планируемые эмитенты:
* X5 Retail Group
* Magnit
* Lenta
* Detskiy Mir

Можно также посмотреть на РУСАГРО, М.Видео (но последняя неликвидна).

**Update.**
- [x] Загрузил все кроме Русагро (включая М.Видео) в виде .csv файла в папку `data`
- [x] написал скрипт для выкачки данных с сайта ММВБ (валяется в папке `scripts`) </br>
⛔ проблема - X5 Retail Group начала торговать своими депозитарными расписками на ММВБ с 2 февраля 2018 года, а задание в ЛМС требует оцненивать модель на данных начиная с 2014-09-01.


Про использование скрипта для подгрузки данных:
 1. Ставим нужные библиотеки:
 ```python
 pip install apimoex requests urllib3 pandas 
 ```
 2. Запускаем скрипт:
 ```python
 $ parser.py
 ```
 3. Парсер сохраняет в папку `data` исторические котировки для пяти компаний, которые мы пока рассматриваем.
