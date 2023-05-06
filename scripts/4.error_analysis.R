library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#get data in wide format to make easier plots
df_vivax = read_csv('results/preds_microrregion_vivax_df.csv') |>
  mutate(difs = real - preds) |>
  mutate(mes = sprintf('%02d', mes)) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(preds + 1))^2))

df_falciparum = read_csv('results/preds_microrregion_falciparum_df.csv') |>
  mutate(difs = real - preds) |>
  mutate(mes = sprintf('%02d', mes)) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(preds + 1))^2))


#spatial information for spatial plot
df_vivax = geobr::read_micro_region(year = 2017, simplified = F) |>
  select('code_micro', 'name_micro') |>
  inner_join(
    df_vivax,
    by = c('code_micro' = 'codMicroRes'),
    multiple = 'all'
  )

df_falciparum = geobr::read_micro_region(year = 2017, simplified = F) |>
  select('code_micro', 'name_micro') |>
  inner_join(
    df_falciparum,
    by = c('code_micro' = 'codMicroRes'),
    multiple = 'all'
  )


#ploting map---------------------------------------------------------------
#vivax_preds
df_vivax |> filter((mes == '02' | mes == '06' | mes == '11') & 
                     (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'Erros nominais de predição por microrregião - Vivax')

#falciparum preds
df_falciparum |> filter((mes == '02' | mes == '06' | mes == '11') &
                          (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Erro nominal') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'Erros nominais de predição por microrregião - Falciparum')

#vivax_preds rmsle
df_vivax |> filter((mes == '02' | mes == '06' | mes == '11') & 
                     (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'RSLE') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'RSLE por microrregião - Vivax')

#falciparum preds rmsle
df_falciparum |> filter((mes == '02' | mes == '06' | mes == '11') &
                          (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'RLSE') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'RSLE por microrregião - Falciparum')

#plot real values per year
df_vivax |> group_by(ano) |> summarise(real = sum(real)) |>
  ggplot() +
  geom_line(aes(x = ano, y = real)) +
  theme_bw()
