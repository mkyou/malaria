library(geobr)
library(readr)
library(dplyr)
library(spdep)
library(INLA)

pa_map = read_municipality(code_muni = 'PA', year = 2017, simplified = F)
to_map = read_municipality(code_muni = 'TO', year = 2017, simplified = F)
ma_map = read_municipality(code_muni = 'MA', year = 2017, simplified = F)
mt_map = read_municipality(code_muni = 'MT', year = 2017, simplified = F)
ac_map = read_municipality(code_muni = 'AC', year = 2017, simplified = F)
ro_map = read_municipality(code_muni = 'RO', year = 2017, simplified = F)
rr_map = read_municipality(code_muni = 'RR', year = 2017, simplified = F)
am_map = read_municipality(code_muni = 'AM', year = 2017, simplified = F)
ap_map = read_municipality(code_muni = 'AP', year = 2017, simplified = F)

micro_reg_map = read_micro_region(year = 2017, simplified = F)

pa_map = pa_map |> 
  inner_join(
    read_csv('data/output_data/pa_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
    )

to_map = to_map |> 
  inner_join(
    read_csv('data/output_data/to_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

ma_map = ma_map |> 
  inner_join(
    read_csv('data/output_data/ma_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

mt_map = mt_map |> 
  inner_join(
    read_csv('data/output_data/mt_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

ac_map = ac_map |> 
  inner_join(
    read_csv('data/output_data/ac_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

ro_map = ro_map |> 
  inner_join(
    read_csv('data/output_data/ro_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

rr_map = rr_map |> 
  inner_join(
    read_csv('data/output_data/rr_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

am_map = am_map |> 
  inner_join(
    read_csv('data/output_data/am_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

ap_map = ap_map |> 
  inner_join(
    read_csv('data/output_data/ap_f_df.csv') |>
      select(codMunRes) |> unique(),
    by = c('code_muni' = 'codMunRes')
  )

micro_reg_map = micro_reg_map |>
  inner_join(
    read_csv('data/output_data/micro_reg_f_df.csv') |>
      select(codMicroRes) |> unique(),
    by = c('code_micro' = 'codMicroRes')
  )

ap_map |> plot() #ok

micro_reg_map |> plot()

pa_path = 'outputs/pa_map.graph'
nb2INLA(pa_path, poly2nb(pa_map))

to_path = 'outputs/to_map.graph'
nb2INLA(to_path, poly2nb(to_map))

ma_path = 'outputs/ma_map.graph'
nb2INLA(ma_path, poly2nb(ma_map))

mt_path = 'outputs/mt_map.graph'
nb2INLA(mt_path, poly2nb(mt_map))

ac_path = 'outputs/ac_map.graph'
nb2INLA(ac_path, poly2nb(ac_map))

ro_path = 'outputs/ro_map.graph'
nb2INLA(ro_path, poly2nb(ro_map))

rr_path = 'outputs/rr_map.graph'
nb2INLA(rr_path, poly2nb(rr_map))

am_path = 'outputs/am_map.graph'
nb2INLA(am_path, poly2nb(am_map))

ap_path = 'outputs/ap_map.graph'
nb2INLA(ap_path, poly2nb(ap_map))

micro_path = 'outputs/micro_map.graph'
nb2INLA(micro_path, poly2nb(micro_reg_map))

pa_map |> write_csv('data/spatial_data/pa_map.csv')
to_map |> write_csv('data/spatial_data/to_map.csv')
ma_map |> write_csv('data/spatial_data/ma_map.csv')
mt_map |> write_csv('data/spatial_data/mt_map.csv')
ac_map |> write_csv('data/spatial_data/ac_map.csv')
ro_map |> write_csv('data/spatial_data/ro_map.csv')
rr_map |> write_csv('data/spatial_data/rr_map.csv')
am_map |> write_csv('data/spatial_data/am_map.csv')
ap_map |> write_csv('data/spatial_data/ap_map.csv')

micro_reg_map |> write_csv('data/spatial_data/micro_map.csv')

rm(ac_map, am_map, ap_map, ma_map, mt_map, pa_map, 
   ro_map, rr_map, to_map, ac_path, am_path, ap_path,
   ma_path, mt_path, pa_path, ro_path, rr_path, to_path,
   micro_reg_map, micro_path)


