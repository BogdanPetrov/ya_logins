library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# загружаем файл с данными

logins_raw <- read_delim(
  file = 'https://github.com/BogdanPetrov/ya_logins/raw/main/ya_logins_2021-10-04_n%3D4.txt.gz', 
  delim = '\t', 
  col_types = 'ci'
)

# разбиваем логины по символам и добавляем некоторые характеристики

positions <- paste0('pos', 1:4)

logins <- logins_raw %>%
  separate(login, positions, sep = 1:4, remove = F) %>%
  mutate(
    # флаг, что i-ый символ логина является цифрой
    is_digit_2 = str_detect(pos2, '\\d'), 
    is_digit_3 = str_detect(pos3, '\\d'),
    is_digit_4 = str_detect(pos4, '\\d'),
    # флаг, что i-ый символ логина является точкой
    is_point_2 = pos2 == '.',
    is_point_3 = pos3 == '.'
  )

# кол-во логинов

logins %>%
  nrow()

# кол-во свободных логинов

logins %>% 
  filter(is_available == 1) %>%
  nrow()

# для каждого допустимого символа считаем долю занятых логинов, содержащих этот символ

S <- c(letters, as.character(0:9), '.')

symbol_stat <- S %>%
  map(function(symbol) {
    data.frame(
      total = logins$login %>%
        str_detect(fixed(symbol)) %>%
        sum(), # всего логинов, которые содержат symbol
      available = logins %>%
        filter(is_available == 1) %>%
        pull(login) %>%
        str_detect(fixed(symbol)) %>%
        sum(), # из них доступно
      symbol = symbol,
      stringsAsFactors = F
    )
  }) %>%
  bind_rows() %>%
  mutate(not_available_share = (1 - available / total) * 100)

# для каждой комбинации символов считаем долю занятых логинов, содержащих эту комбинацию

symbol_combs <- cross_df(list('s1' = S, 's2' = S)) %>%
  mutate(comb = paste0(s1, s2)) %>%
  pull(comb)

comb_stat <- symbol_combs %>%
  map(function(comb) {
    data.frame(
      total = logins$login %>%
        str_detect(fixed(comb)) %>%
        sum(), # всего логинов, которые содержат comb
      available = logins %>%
        filter(is_available == 1) %>%
        pull(login) %>%
        str_detect(fixed(comb)) %>%
        sum(), # из них доступно
      comb = comb,
      stringsAsFactors = F
    )
  }) %>%
  bind_rows() %>%
  mutate(not_available_share = (1 - available / total) * 100)

# для каждого символа и его позиции считаем долю занятых логинов, содержащих этот символ на этой позиции

symbol_pos_stat <- S %>%
  map(function(symbol) {
    positions %>%
      map(function(pos) {
        data.frame(
          total = logins %>% 
            filter(!!as.symbol(pos) == symbol) %>%
            nrow(), # всего логинов с symbol на позиции pos
          available = logins %>% 
            filter(!!as.symbol(pos) == symbol & is_available == 1) %>%
            nrow(), # из них доступно
          symbol = symbol,
          pos = pos
        )
      }) %>%
      bind_rows()
  }) %>%
  bind_rows() %>%
  mutate(not_available_share = (1 - available / total) * 100)

# считаем доступность логинов по форматам

login_formats <- logins %>%
  filter(login %in% c(
    'aaaa', 'aaa1', 'aa1a', 'a1aa', 'a1a1', 'aa11', 'a11a', 'a111',
    'aa.a', 'a.aa', 'aa.1', 'a.a1', 'a.1a', 'a1.a', 'a1.1', 'a.11'
  )) %>%
  select(login_format = login, is_digit_2, is_digit_3, is_digit_4, is_point_2, is_point_3)

login_format_stat <- logins %>%
  group_by(is_digit_2, is_digit_3, is_digit_4, is_point_2, is_point_3, is_available) %>%
  summarize(login_count = n()) %>%
  ungroup() %>%
  mutate(is_available_str = ifelse(is_available == 1, 'available', 'not_available')) %>%
  inner_join(login_formats, by = c('is_digit_2', 'is_digit_3', 'is_digit_4', 'is_point_2', 'is_point_3')) %>%
  pivot_wider(login_format, names_from = is_available_str, values_from = login_count) %>%
  mutate(
    total = available + not_available,
    available_share = available / total
  ) %>%
  arrange(available_share)

# смотрим удобные логины

nice_logins <- logins %>%
  filter(str_detect(login, '^[tpadfkxbnm]+$'))

nice_logins %>%
  nrow() # кол-во логинов

nice_logins %>% 
  filter(is_available == 1) %>%
  nrow() # кол-во свободных логинов

# считаем доступность логинов по содержащимся в них числам

login_numbers_list <- logins$login %>%
  str_extract_all('[0-9]+') %>% # извлекаем из каждого логина все числа (например, из a1.2 будут извлечены 1 и 2)
  setNames(logins$login)
login_numbers_names <- rep(
  names(login_numbers_list), 
  login_numbers_list %>% map(length)
)
login_numbers <- data.frame(
  login = login_numbers_names,
  number = login_numbers_list %>% 
    unlist(use.names=F)
) %>%
  right_join(logins, by = 'login') %>%
  group_by(login) %>%
  mutate(number_count = sum(!is.na(number))) %>%
  ungroup()

login_numbers_stat <- login_numbers %>%
  distinct(login, is_available, number) %>%
  group_by(number) %>%
  summarize(
    total = n(),
    available = sum(is_available)
  ) %>%
  ungroup() %>%
  mutate(
    not_available_share = (1 - available/total) * 100, 
    number_len = str_length(number)
  ) %>%
  arrange(number_len, number)

# считаем группы свободных и занятых логинов, идущих подряд

login_groups <- logins %>%
  mutate(
    change_state = is_available != lag(is_available, 1, 0),
    group_number = cumsum(change_state) # порядковый номер группы
  ) %>%
  group_by(group_number) %>%
  summarize(
    login_from = first(login),
    login_to = last(login),
    group_size = n(),
    group_type = if (first(is_available) == 1) 'available' else 'not available'
  ) %>%
  ungroup()
