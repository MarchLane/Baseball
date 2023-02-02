data2016 = read_csv('/Users/jeongminjae/Desktop/Analyzing Baseball Data with R/baseball_R-master/data/data2016_Run_Scored.csv')

Master = read_csv('/Users/jeongminjae/Desktop/Analyzing Baseball Data with R/baseball_R-master/data/masterid.csv')

Master %>%
  filter(bref_name == 'Shin-Soo Choo') %>%
  pull(retro_id) -> choo_id

data2016 %>%
  filter(BAT_ID == choo_id,
         BAT_EVENT_FL == TRUE) -> choo

choo %>%
  select(STATE, NEW.STATE, run_value)

choo %>%
  group_by(BASES) %>%
  summarise(N = n())

ggplot(choo, aes(BASES, run_value)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_hline(yintercept = 0, color = 'blue') +
  xlab('RUNNERS')

choo %>%
  group_by(STATE) %>%
  summarise(RUNS = sum(run_value)) %>%
  arrange(STATE)

ggplot(choo, aes(STATE, run_value)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_hline(yintercept = 0, color = 'blue') +
  xlab('RUNNERS')

