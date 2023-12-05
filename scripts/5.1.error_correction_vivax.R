library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#get data in wide format to make easier plots
df_vivax_pre = read_csv('results/preds_microrregion_vivax_df.csv') |>
  mutate(mes = sprintf('%02d', mes)) |>
  mutate(difs_bell = real - bell_preds) |>
  mutate(difs_rmsle_bell = 
           sqrt((log(real + 1) - log(bell_preds + 1))^2)) |>
  mutate(difs_nbinomial = real - nbinomial_preds) |>
  mutate(difs_rmsle_nbinomial = 
           sqrt((log(real + 1) - log(nbinomial_preds + 1))^2)) |>
  mutate(difs_poisson = real - poisson_preds) |>
  mutate(difs_rmsle_poisson = 
           sqrt((log(real + 1) - log(poisson_preds + 1))^2)) 
  
#using predictions off 2016 and 2017 to correct the predicts magnitude of 2018

df_vivax_train = df_vivax_pre |> filter(ano < 2018 & ano > 2015)
plot(df_vivax_train$bell_preds, df_vivax_train$real)

#the first model is good to predict 0 values. So the pred must be greater than
#0.005 (the max pred value when the real incidence is 0)

df_vivax_teste = df_vivax_pre |> filter(ano == 2018 & bell_preds > 0.005)
df_vivax_train = df_vivax_pre |> filter(ano < 2018 & ano > 2015 & 
                                          bell_preds > 0.005)

#okay: 10.9515 is the preds coefficient.
fit = lm(real ~ -1 + bell_preds, data = df_vivax_train)
summary(fit)

#creating new_preds column
df_vivax_pre = df_vivax_pre |>
  mutate(new_bell_preds = ifelse(ano == 2018 & bell_preds > 0.005, 
                            bell_preds*fit$coefficients[1], bell_preds))

#creating new_difs and new_difs_rmsle columns
df_vivax_pre = df_vivax_pre |>
  mutate(new_difs_bell = real - new_bell_preds) |>
  mutate(new_difs_rmsle_bell = sqrt((log(real + 1) - 
                                       log(new_bell_preds + 1))^2))

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
  geom_sf(aes(fill = difs_bell), color = 'black', size = .15) +
  scale_fill_gradient(low = '#d7e1ee', high = '#991f17',
                      limits = c(-5, 10),
                      name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave('results/erros_vivax_2018.png')

#vivax_preds
df_vivax |> filter((mes == '06' | mes == '11') & 
                     (ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = new_difs_bell), color = 'black', size = .15) +
  scale_fill_gradient(low = '#d7e1ee', high = '#991f17',
                      limits = c(-5, 10),
                      name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave('results/erros_vivax_2018_corrigido.png')

#let's evaluate all the predicts now
real = df_vivax_pre |> filter(ano == 2018) |> select(real) |> pull()

bell = df_vivax_pre |> filter(ano == 2018) |> select(bell_preds) |> 
  pull()

nbin = df_vivax_pre |> filter(ano == 2018) |> select(nbinomial_preds) |> 
  pull()

poi = df_vivax_pre |> filter(ano == 2018) |> select(poisson_preds) |> 
  pull()

new_bell = df_vivax_pre |> filter(ano == 2018) |> select(new_bell_preds) |> 
  pull()

#get metrics
test_errors_vivax = dplyr::tibble(
  dist = c('bell', 'poisson', 'nbinomial', 'new_bell'),
  
  mbe = c(
    mbe(real, bell),
    mbe(real, poi),
    mbe(real, nbin),
    mbe(real, new_bell)
  ),
  
  nrmse = c(
    nrmse(real, bell),
    nrmse(real, poi),
    nrmse(real, nbin),
    nrmse(real, new_bell)
  ),
  
  rae = c(
    rae(real, bell),
    rae(real, poi),
    rae(real, nbin),
    rae(real, new_bell)
  ),
  
  rmsle = c(
    rmsle(real, bell),
    rmsle(real, poi),
    rmsle(real, nbin),
    rmsle(real, new_bell)
  ),
  
  rse = c(
    rse(real, bell),
    rse(real, poi),
    rse(real, nbin),
    rse(real, new_bell)
  ),
  
  cor = c(
    cor(real, bell),
    cor(real, poi),
    cor(real, nbin),
    cor(real, new_bell)
  )
  
)

#view results
test_errors_vivax |> View()

test_errors_vivax |> 
  write_csv('results/test_metrics_microrregion_vivax.csv')

df_vivax_pre |> write_csv('results/preds_microrregion_vivax_df.csv')
