library(dplyr)
library(readr)
library(ggplot2)

df_vivax = read_csv('results/preds_microrregion_vivax_df.csv')
df_falciparum = read_csv('results/preds_microrregion_falciparum_df.csv')

df_vivax |> head()
df_falciparum |> head()


