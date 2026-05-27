library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

df_vivax_pre = read_csv('results/preds_microrregion_vivax_df.csv') |>
  mutate(preds = bell_preds) |>
  mutate(difs = real - preds) |>
  mutate(mes = sprintf('%02d', as.integer(mes))) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(preds + 1))^2))

df_falciparum_pre = read_csv('results/preds_microrregion_falciparum_df.csv') |>
  mutate(preds = poisson_preds) |>
  mutate(difs = real - preds) |>
  mutate(mes = sprintf('%02d', as.integer(mes))) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(preds + 1))^2))


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


df_vivax |> filter((mes == '02' | mes == '06' | mes == '11') & 
                     (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Nominal error\n(cases per 100k)') +
  facet_wrap(~ano + mes) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave('results/erros_vivax.png')

df_falciparum |> filter((mes == '02' | mes == '06' | mes == '11') &
                          (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Nominal error\n(cases per 100k)') +
  facet_wrap(~ano + mes) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave('results/erros_falciparum.png')

df_vivax |> filter((mes == '02' | mes == '06' | mes == '11') & 
                     (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'RMSLE') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
ggsave('results/erros_vivax_rsle.png')

df_falciparum |> filter((mes == '02' | mes == '06' | mes == '11') &
                          (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'RMSLE') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
ggsave('results/erros_falciparum_rsle.png')

df_vivax |> filter(mes == '11' & ano == 2017) |>
  ggplot() +
  geom_sf(aes(fill = real), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Cases per\n100k inhabitants') +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave('results/real vivax 201711.png')

df_vivax |> filter(mes == '11' & ano == 2017) |>
  ggplot() +
  geom_sf(aes(fill = preds), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Predicted cases per\n100k inhabitants') +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave('results/preds vivax 201711.png')
#2017 had more malaria cases. So, the predictions (in the nominal scale)
#probably will be worse in that year.
df_vivax_pre |>
  mutate(dia = as.Date(paste(ano, '-', mes, '-01', sep = ''))) |>
  group_by(dia) |> summarise(real = mean(real)) |>
  ggplot(aes(x = dia, y = real)) +
  geom_line(size = .8) +
  theme_bw() +
  labs(x = 'Time', y = 'Mean cases per 100k inhabitants')
ggsave('results/real_vivax.png')

df_am_v_pre = read_csv('results/preds_am_vivax_df.csv') |>
  mutate(difs = real - bell) |>
  mutate(mes = sprintf('%02d', as.integer(mes))) |>
  mutate(difs_rmsle = sqrt((log(real + 1) - log(bell + 1))^2))

df_am_v = geobr::read_municipality(code_muni = 'AM',
                                    year = 2017, simplified = F) |>
  select('code_muni', 'name_muni') |>
  inner_join(
    df_am_v_pre,
    by = c('code_muni' = 'codMunRes'),
    multiple = 'all'
  )

df_am_v |> filter((mes == '02' | mes == '06' | mes == '11') & 
                     (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  geom_sf(aes(fill = difs), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'Nominal error\n(cases per 100k)') +
  facet_wrap(~ano + mes) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave('results/erros_am_vivax.png')

df_am_v |> filter((mes == '02' | mes == '06' | mes == '11') & 
                    (ano == 2016 | ano == 2017 | ano == 2018)) |>
  ggplot() +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  geom_sf(aes(fill = difs_rmsle), color = 'black', size = .15) +
  scale_fill_gradientn(colours = c('#d7e1ee', '#991f17'),
                       name = 'RMSLE') +
  facet_wrap(~ano + mes) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave('results/erros_am_rlse_vivax.png')

