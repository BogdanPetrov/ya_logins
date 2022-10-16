# ya_logins

- [https://habr.com/ru/post/582816/](https://habr.com/ru/post/582816/)

Файл [ya_logins_2021-10-04_n=4.txt.gz](ya_logins_2021-10-04_n=4.txt.gz) содержит информацию о доступности логинов Яндекса длиной 4 символа по состоянию на 4 октября 2021 года. Для удобства перечислены все логины, а не только свободные. Каждая строка соответствует 1 логину и состоит из двух значений: сам логин и флаг, обозначающий доступность (0 - занят, 1 - свободен). 

Ниже приведены команды для начала работы с файлом на R и Python:

```r
df <- readr::read_delim(
    file = 'https://github.com/BogdanPetrov/ya_logins/raw/main/ya_logins_2021-10-04_n%3D4.txt.gz', 
    delim = '\t', 
    col_types = 'ci'
)
```

```python
import pandas as pd

df = pd.read_csv(
    'https://github.com/BogdanPetrov/ya_logins/raw/main/ya_logins_2021-10-04_n%3D4.txt.gz', 
    sep='\t',
    compression='gzip',
    dtype={
        'login': 'str',
        'is_available': 'UInt8'
    }
)
```

