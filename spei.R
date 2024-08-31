library(terra)
library(tidyverse)
library(tidyterra)

pet <- rast("data/WA_ETP_month.nc")
dates <- time(pet)

# resample PET data first
tamsat <- rast("data/TAMSAT/rfe1983_01.v3.1.nc")
pet_resampled <- resample(pet,tamsat,"bilinear")

#---- read PET data in xyzt
eto <- pet_resampled |> 
  as_tibble(xy = TRUE) |> 
  pivot_longer(
    cols = contains("evsp"),
    names_to = "date",
    names_prefix = "evspsblpot_",
    values_to = "pet",
    names_transform = as.numeric
  ) |> 
  mutate(
    date = dates[date]
  ) |> 
  filter(between(year(date),2000,2021)) |> 
  select(x,y,date,pet) |> 
  mutate(date = date(date)) |> 
  mutate(date = make_date(year = year(date),month = month(date),day = 1))

# --- Work with tamsat files

rast("NEW_OUTFILE.nc") |> 
  `time<-`(value = seq.Date(as.Date("1983-01-15"),as.Date("2024-06-15"),"month"))


oo <- seq.Date(as.Date("1983-01-15"),as.Date("2024-06-15"),by = "month") |> 
  str_sub(1,7)
oo_good <- oo[oo!="1983-03"]
oo_g <- paste0(oo_good,"-01") |> 
  as.Date()
lubridate::date(oo_good)

# --- test dates
aa <- fs::dir_ls("data/TAMSAT/") |> 
  basename() |> 
  str_remove_all("rfe") |> 
  str_remove_all(".v3.1.nc") |> 
  str_replace_all("_","-")

identical(oo_good,aa)

#---- Rainfall 2001-2022
rr <- rast("NEW_OUTFILE.nc") |> 
  `time<-`(value = oo_g) |> 
  as_tibble(xy=TRUE) |> 
  `names<-`(value = c("x","y",paste0("lyr_",1:497))) |> 
  pivot_longer(
    cols = contains("lyr"),
    names_to = "date",
    values_to = "rain",
    names_prefix = "lyr_",
    names_transform = as.numeric
  ) |> 
  mutate(date = oo_g[date]) |> 
  filter(
    between(year(date),2000,2021)
  )

#---- Compute SPEI

library(SPEI)
# join rr and pet
data <- left_join(rr,eto) |> 
  rename(PRCP = rain, PET = pet) |> 
  mutate(BAL = PRCP-PET) 

# Compute SPEI 6 for all Gridpoints

 spei_data <- data |> 
  group_nest(x,y) |> 
  slice_sample(n = 3) |> # test for 3 sample grid points
  mutate(
    analysis = map(data, function(df) {
          data.frame(
            spei_6 = SPEI::spei(df$BAL,scale = 6)$fitted |> as.numeric(),
            spei_3 = SPEI::spei(df$BAL,scale = 3)$fitted |> as.numeric(),
            date = df$date
          ) |> 
            mutate(
              year = year(date),
              month = month(date)
            ) |> 
            select(-date)
        }
  )
  ) |> 
  select(x,y,analysis) |> 
  unnest(cols = c(analysis))

# Prepare annual SPEI 6 for the rainy season (June to October)

spei_data |> 
  filter(month == 10) |> 
  select(x,y,spei_6,year) |> 
  pivot_wider(id_cols = c(x,y),names_prefix = "spei6_",names_from = year,values_from = spei_6)
