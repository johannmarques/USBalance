library(tidyverse)

data <- read.csv('Table.csv', skip = 3, na.strings = c("n.a."))

str(data)

data$Type.of.investment <- str_trim(data$Type.of.investment)

head(data)

fig1top_classes <- read.csv('1top_classes.csv')

fig1data <- data %>%
  # For each entry, classify as asset or liability
  mutate(Side = ifelse(row_number() <
                         which(data$Type.of.investment == 'U.S. liabilities'),
                       'Assets', 'Liabilities')) %>%
  # Joining with class labels
  left_join(fig1top_classes, by = c('Type.of.investment' = 'Database')) %>%
  # For all columns with "X", convert into numeric
  mutate_at(vars(matches("X")), as.numeric) %>%
  group_by(Class, Side) %>%
  # Sum across all numeric columns
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = X1976:X2022,
               names_to = 'year') %>%
  mutate(year = as.integer(str_replace(year, 'X', '')))

fig1colors <-
  c('Equity' = '#275d91',
    'FDI' = '#d0e1f4',
    'Derivatives' = '#68a0d6',
    'Debt' = '#e0e1e0',
    'FX reserves' = '#919191',
    'NFA' = '#931a1d')

# Generate a balance sheet plot

bsheet <- function(.year){
  fig1data %>%
    filter(year == .year,
           !(Class %in% c('U.S. Assets', 'U.S. Liabilities'))) %>%
    na.omit() %>%
    mutate(Class = factor(Class,
                          levels = c('Equity', 'FDI',
                                     'Derivatives', 'Debt',
                                     'FX reserves', 'NFA'))) %>%
    ggplot(aes(x = 0, y = value, fill = Class,
               label = paste0(Class,': $',
                              round(value/10^6, 1), 'tr'))) +
    geom_col() +
    geom_text(aes(color = Class),
              position = position_stack(vjust = .5)) +
    scale_fill_manual(values = fig1colors) +
    scale_color_manual(values = c('Equity' = 'white',
                                  'FDI' = 'black',
                                  'Derivatives' = 'black',
                                  'Debt' = 'black',
                                  'FX reserves' = 'black',
                                  'NFA' = 'white')) +
    facet_wrap(~Side, scales = 'free') +
    theme_minimal() +
    theme(legend.position="none",
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 12)) +
    labs(title = paste0('US external balance sheet, ', .year)) %>%
    return()
}

bsheet(2022)
ggsave('figures/bsheet_2022.pdf')

bsheet(2015)
ggsave('figures/bsheet_2015.pdf')

fig1data %>%
  filter(!(Class %in% c('U.S. Assets', 'U.S. Liabilities'))) %>%
  na.omit() %>%
  mutate(Class = factor(Class,
                        levels = c('Equity', 'FDI',
                                   'Derivatives', 'Debt',
                                   'FX reserves', 'NFA')),
         value = value/sum(abs(value))) %>%
  ggplot(aes(x = year, y = value, fill = Class,
             label = paste0(Class,': $',
                            round(value/10^6, 1), 'tr'))) +
  geom_col() + facet_wrap(~Side)
  
# Middle

# Import labels
fig1mid_classes <- read.csv('1middle_classes.csv')

data %>%
  # For each entry, classify as asset or liability
  mutate(Side = ifelse(row_number() <
                         which(data$Type.of.investment == 'U.S. liabilities'),
                       'Assets', 'Liabilities')) %>%
  # Joining with class labels
  left_join(fig1mid_classes, by = c('Type.of.investment' = 'Database')) %>%
  # For all columns with "X", convert into numeric
  mutate_at(vars(matches("X")), as.numeric) %>%
  group_by(Class, Side) %>%
  # Sum across all numeric columns
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = X1976:X2022,
               names_to = 'year') %>%
  na.omit() %>%
  mutate(year = as.integer(str_replace(year, 'X', ''))) %>%
  pivot_wider(names_from = Class) %>%
  # Create measures
  mutate(`Riskier Assets` = (`Equity FDI` + Equity)/`Total Assets`,
         `Safer Liabilities` = (Other + Debt)/`Total Liabilities`) %>%
  select(year, `Riskier Assets`, `Safer Liabilities`) %>%
  pivot_longer(c(`Riskier Assets`, `Safer Liabilities`)) %>%
  # Remove inconsistent rows
  na.omit() %>%
  # Remove pre-1982: missing data
  # Authors imputed data
  # However, we are focousing in recent periods
  filter(year > 1981) %>%
  ggplot(aes(x = year, y = value*100,
             group = name, color = name,
             linetype = name)) +
  geom_line() +
  scale_color_manual(values = c('Riskier Assets' = '#3d58a7',
                                'Safer Liabilities' = '#f1594c')) +
  scale_linetype_manual(values = c('Riskier Assets' = 'solid',
                                   'Safer Liabilities' = 'dashed')) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white')) +
  labs(x = '', color = '', linetype = '', y = 'Percent',
       title = 'Asset class composition of US external portfolio')
ggsave('figures/composition.pdf')
ggsave('figures/composition.png') # Just for the README :)
