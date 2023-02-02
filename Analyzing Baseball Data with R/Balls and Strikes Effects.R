## Balls and Strikes Effects

mussina = read_excel('/Users/jeongminjae/Desktop/Analyzing Baseball Data with R/mussina.xlsx')

mussina = expand.grid(balls = 0:3, strikes = 0:2) %>%
  mutate(value = c(100, 118, 157, 207, 72, 82,
                   114, 171, 30, 38, 64, 122))
mussina

count_plot = mussina %>%
  ggplot(aes(x = strikes, y = balls, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3))) +
  scale_fill_gradient2('tOPS+', low = 'grey10', high = '#2905a1',
                       mid = 'white', midpoint = 100)

count_plot


##

headers = read_csv('/Users/jeongminjae/Desktop/Analyzing Baseball Data with R/baseball_R-master/data/fields.csv')
pbp2016 = read_csv('/Users/jeongminjae/Desktop/Analyzing Baseball Data with R/baseball_R-master/data/all2016.csv',
                   col_names = pull(headers, Header),
                   na = character())

pbp2016 = pbp2016 %>%
  mutate(pseq = gsub('[.>123N + *]', '', PITCH_SEQ_TX))

subset(pbp2016, select = c10)

## 초구가 볼
pbp2016 = pbp2016 %>%
  mutate(c10 = grepl('^[BIPV]', pseq))

## 초구가 스트라이크
pbp2016 = pbp2016 %>%
  mutate(c01 = grepl('^[CFKLMOQRST]', pseq))

pbp2016 %>%
  select(PITCH_SEQ_TX, c10, c01) %>%
  head(10)

pbp16rc %>%
  select(GAME_ID, EVENT_ID, RUNS.VALUE, c00, c10, c20,
         c11, c01, c30, c21, c31, c02, c12, c22, c32) %>%
  head()



data2016
