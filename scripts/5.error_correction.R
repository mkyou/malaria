library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#get data in wide format to make easier plots
df_vivax_pre = read_csv('results/preds_microrregion_vivax_df.csv') |>
  mutate(difs = real - preds) |>
  mutate(mes = sprintf('%02d', mes)) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(preds + 1))^2))

#using predictions off 2016 and 2017 to correct the predicts magnitude of 2018
#the first model is good to predict 0 values. So the pred must be greater than
#0.005 (the max pred value when the real incidence is 0)
df_vivax_teste = df_vivax_pre |> filter(ano == 2018 & preds > 0.005)
df_vivax_train = df_vivax_pre |> filter(ano < 2018 & ano > 2015 & 
                                          preds > 0.005)

#okay: 11.0773 is the preds coefficient.
fit = lm(real ~ -1 + preds, data = df_vivax_train)
summary(fit)

#creating new_preds column
df_vivax_pre = df_vivax_pre |>
  mutate(new_preds = ifelse(ano == 2018 & preds > 0.005, 
                            preds*11.0773, preds))

#creating new_difs and new_difs_rmsle columns
df_vivax_pre = df_vivax_pre |>
  mutate(new_difs = real - new_preds) |>
  mutate(new_difs_rmsle = sqrt((log(real + 1) - log(new_preds + 1))^2))

#get spatial information for spatial plot
df_vivax = geobr::read_micro_region(year = 2017, simplified = F) |>
  select('code_micro', 'name_micro') |>
  inner_join(
    df_vivax_pre,
    by = c('code_micro' = 'codMicroRes'),
    multiple = 'all'
  )

#vivax_preds
df_vivax |> filter((mes == '06' | mes == '11') & 
                     (ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  scale_fill_gradient(low = '#d7e1ee', high = '#991f17',
                      limits = c(-5, 10),
                      name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'Erros nominais de predição - Ajuste original')

ggsave('results/erros_vivax_2018.png')

#vivax_preds
df_vivax |> filter((mes == '06' | mes == '11') & 
                     (ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = new_difs), color = 'black', size = .15) +
  scale_fill_gradient(low = '#d7e1ee', high = '#991f17',
                      limits = c(-5, 10),
                      name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'Erros nominais de predição - Ajuste corrigido')

ggsave('results/erros_vivax_2018_corrigido.png')
