library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#get data in wide format to make easier plots
df_vivax_pre = read_csv('results/preds_microrregion_vivax_df.csv') |>
  mutate(difs = real - preds) |>
  mutate(mes = sprintf('%02d', mes)) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(preds + 1))^2))

df_falciparum_pre = read_csv('results/preds_microrregion_falciparum_df.csv') |>
  mutate(difs = real - preds) |>
  mutate(mes = sprintf('%02d', mes)) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(preds + 1))^2))


#spatial information for spatial plot
df_vivax = geobr::read_micro_region(year = 2017, simplified = F) |>
  select('code_micro', 'name_micro') |>
  inner_join(
    df_vivax_pre,
    by = c('code_micro' = 'codMicroRes'),
    multiple = 'all'
  )

df_falciparum = geobr::read_micro_region(year = 2017, simplified = F) |>
  select('code_micro', 'name_micro') |>
  inner_join(
    df_falciparum_pre,
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
  labs(subtitle = 'Erros nominais de predição a cada mil habitantes
       por microrregião - Vivax')
ggsave('results/erros_vivax.png')

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
  labs(subtitle = 'Erros nominais de predição a cada mil habitantes
       por microrregião - Falciparum')
ggsave('results/erros_falciparum.png')

#vivax erros rmsle
df_vivax |> filter((mes == '02' | mes == '06' | mes == '11') & 
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
  labs(subtitle = 'RSLE a cada mil habitantes 
       por microrregião - Vivax')
ggsave('results/erros_vivax_rsle.png')

#falciparum erros rmsle
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
  labs(subtitle = 'RSLE a cada mil habitantes 
       por microrregião - Falciparum')
ggsave('results/erros_falciparum_rsle.png')

#vivax real in 201711
df_vivax |> filter(mes == '11' & ano == 2017) |>
  ggplot() +
  geom_sf(aes(fill = real), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'N° Casos') +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'Casos de malária a cada mil habitantes
       por microrregião em 11/2017 - Vivax')
ggsave('results/real vivax 201711.png')

#vivax preds in 201711
df_vivax |> filter(mes == '11' & ano == 2017) |>
  ggplot() +
  geom_sf(aes(fill = preds), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Predição a cada mil habitantes') +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(subtitle = 'Predição de malária a cada mil habitantes
       por microrregião em 11/2017 - Vivax')
ggsave('results/preds vivax 201711.png')
#other plots------------------------------------------------------------
#plot real values per year
#2017 had more malaria cases. So, the predictions (in the nominal scale)
#probably will be worse in that year. 
df_vivax_pre |>
  mutate(dia = as.Date(paste(ano, '-', mes, '-01', sep = ''))) |>
  group_by(dia) |> summarise(real = mean(real)) |>
  ggplot(aes(x = dia, y = real)) +
  geom_line(size = .8) +
  theme_bw() +
  labs(subtitle = 'Número médio de casos de malária a cada mil habitantes 
  na Amazônia Legal ao longo do tempo - Vivax', 
       x = 'Tempo', y = 'Número de casos')
ggsave('results/real_vivax.png')

#error analysis for amazonia state------------------------------------------
df_am_v_pre = read_csv('results/preds_am_vivax_df.csv') |>
  mutate(difs = real - bell) |>
  mutate(mes = sprintf('%02d', mes)) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(bell + 1))^2))

#spatial information for spatial plot
df_am_v = geobr::read_municipality(code_muni = 'AM',
                                    year = 2017, simplified = F) |>
  select('code_muni', 'name_muni') |>
  inner_join(
    df_am_v_pre,
    by = c('code_muni' = 'codMunRes'),
    multiple = 'all'
  )

#spatial plot am
df_am_v |> filter((mes == '02' | mes == '06' | mes == '11') & 
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
  labs(subtitle = 'Erros nominais de predição a cada mil habitantes
       no estado do Amazonas - Vivax')
ggsave('results/erros_am_vivax.png')

df_am_v |> filter((mes == '02' | mes == '06' | mes == '11') & 
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
  labs(subtitle = 'RLSE de predição a cada mil habitantes
       no estado di Amazonas - Vivax')
ggsave('results/erros_am_rlse_vivax.png')

#comparing real and preds
df_am_v_pre |>
  mutate(dia = as.Date(paste(ano, '-', mes, '-01', sep = ''))) |>
  group_by(dia) |> summarise(real = mean(real),
                             bell = mean(bell)) |>
  ggplot(aes(x = dia)) +
  geom_line(aes(y = real), size = .8, colour = 'red') +
  geom_line(aes(y = bell), size = .8, linetype = 2, colour = 'blue') +
  theme_bw() +
  labs(subtitle = 'Número médio de casos de malária a cada mil habitantes 
  no estado do Amazonas ao longo do tempo - Vivax', 
       x = 'Tempo', y = 'Número de casos')
