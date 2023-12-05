library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#get data in wide format to make easier plots
df_falciparum_pre = read_csv('results/preds_microrregion_falciparum_df.csv') |>
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

df_falciparum_train = df_falciparum_pre |> filter(ano < 2018 & ano > 2015)
plot(df_falciparum_train$poisson_preds, df_falciparum_train$real)

#Here, we will need to make three models. 
#One for preds < 0.01, another for preds > 0.01 and preds <0.04
#and the last for preds > 0.04.

df_falciparum_teste = df_falciparum_pre |> filter(ano == 2018 & 
                                                    poisson_preds < 0.01)
df_falciparum_train = df_falciparum_pre |> filter(ano < 2018 & ano > 2015 & 
                                          poisson_preds < 0.01)

#model2
df_falciparum_teste2 = df_falciparum_pre |> filter(ano == 2018 & 
                                                    poisson_preds >= 0.01 &
                                                     poisson_preds <= 0.04)
df_falciparum_train2 = df_falciparum_pre |> filter(ano < 2018 & ano > 2015 & 
                                                     poisson_preds >= 0.01 &
                                                     poisson_preds <= 0.04)

#model3
df_falciparum_teste3 = df_falciparum_pre |> filter(ano == 2018 & 
                                                    poisson_preds > 0.04)
df_falciparum_train3 = df_falciparum_pre |> filter(ano < 2018 & ano > 2015 & 
                                                    poisson_preds > 0.04)

#okay: 11.2528 is the preds coefficient.
fit = lm(real ~ -1 + poisson_preds, data = df_falciparum_train)
summary(fit)

#here, 14.892
fit2 = lm(real ~ -1 + poisson_preds, data = df_falciparum_train2)
summary(fit2)

#finally, 54.337 here.
fit3 = lm(real ~ -1 + poisson_preds, data = df_falciparum_train3)
summary(fit3)


#creating new_preds column
df_falciparum_pre = df_falciparum_pre |>
  mutate(new_poi_preds = ifelse(
    ano == 2018 & poisson_preds < 0.01,
    poisson_preds*fit$coefficients[1],
    
    ifelse(
      ano == 2018 & poisson_preds >= 0.01 & poisson_preds < 0.04,
      poisson_preds*fit2$coefficients[1],
      
      ifelse(
        ano == 2018 & poisson_preds >= 0.04,
        poisson_preds*fit3$coefficients[1],
        poisson_preds
      )
    )
  )
)

#creating new_difs and new_difs_rmsle columns
df_falciparum_pre = df_falciparum_pre |>
  mutate(new_difs_poi = real - new_poi_preds) |>
  mutate(new_difs_rmsle_poi = sqrt((log(real + 1) - 
                                       log(new_poi_preds + 1))^2))

#get spatial information for spatial plot
df_falciparum = geobr::read_micro_region(year = 2017, simplified = F) |>
  select('code_micro', 'name_micro') |>
  inner_join(
    df_falciparum_pre,
    by = c('code_micro' = 'codMicroRes'),
    multiple = 'all'
  )

#falciparum_preds
df_falciparum |> filter((mes == '06' | mes == '11') & 
                     (ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs_poisson), color = 'black', size = .15) +
  scale_fill_gradient(low = '#d7e1ee', high = '#991f17',
                      limits = c(-5, 10),
                      name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave('results/erros_falciparum_2018.png')

#falciparum_preds
df_falciparum |> filter((mes == '06' | mes == '11') & 
                     (ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = new_difs_poi), color = 'black', size = .15) +
  scale_fill_gradient(low = '#d7e1ee', high = '#991f17',
                      limits = c(-5, 10),
                      name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave('results/erros_falciparum_2018_corrigido.png')

#let's evaluate all the predicts now
real = df_falciparum_pre |> filter(ano == 2018) |> select(real) |> pull()

bell = df_falciparum_pre |> filter(ano == 2018) |> select(bell_preds) |> 
  pull()

nbin = df_falciparum_pre |> filter(ano == 2018) |> select(nbinomial_preds) |> 
  pull()

poi = df_falciparum_pre |> filter(ano == 2018) |> select(poisson_preds) |> 
  pull()

new_poi = df_falciparum_pre |> filter(ano == 2018) |> select(new_poi_preds) |> 
  pull()

#get metrics
test_errors_falciparum = dplyr::tibble(
  dist = c('bell', 'poisson', 'nbinomial', 'new_poi'),
  
  mbe = c(
    mbe(real, bell),
    mbe(real, poi),
    mbe(real, nbin),
    mbe(real, new_poi)
  ),
  
  nrmse = c(
    nrmse(real, bell),
    nrmse(real, poi),
    nrmse(real, nbin),
    nrmse(real, new_poi)
  ),
  
  rae = c(
    rae(real, bell),
    rae(real, poi),
    rae(real, nbin),
    rae(real, new_poi)
  ),
  
  rmsle = c(
    rmsle(real, bell),
    rmsle(real, poi),
    rmsle(real, nbin),
    rmsle(real, new_poi)
  ),
  
  rse = c(
    rse(real, bell),
    rse(real, poi),
    rse(real, nbin),
    rse(real, new_poi)
  ),
  
  cor = c(
    cor(real, bell),
    cor(real, poi),
    cor(real, nbin),
    cor(real, new_poi)
  )
  
)

#view results
test_errors_falciparum |> View()

test_errors_falciparum |> 
  write_csv('results/test_metrics_microrregion_falciparum.csv')

df_falciparum_pre |> write_csv('results/preds_microrregion_falciparum_df.csv')
