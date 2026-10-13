data science Homework 3
================
Yiran Jin
2026-10-13

# Problem 1

``` r
library(p8105.datasets)
library(tidyverse)

data("ny_noaa")

ny_noaa
```

    ## # A tibble: 2,595,176 × 7
    ##    id          date        prcp  snow  snwd tmax  tmin 
    ##    <chr>       <date>     <int> <int> <int> <chr> <chr>
    ##  1 US1NYAB0001 2007-11-01    NA    NA    NA <NA>  <NA> 
    ##  2 US1NYAB0001 2007-11-02    NA    NA    NA <NA>  <NA> 
    ##  3 US1NYAB0001 2007-11-03    NA    NA    NA <NA>  <NA> 
    ##  4 US1NYAB0001 2007-11-04    NA    NA    NA <NA>  <NA> 
    ##  5 US1NYAB0001 2007-11-05    NA    NA    NA <NA>  <NA> 
    ##  6 US1NYAB0001 2007-11-06    NA    NA    NA <NA>  <NA> 
    ##  7 US1NYAB0001 2007-11-07    NA    NA    NA <NA>  <NA> 
    ##  8 US1NYAB0001 2007-11-08    NA    NA    NA <NA>  <NA> 
    ##  9 US1NYAB0001 2007-11-09    NA    NA    NA <NA>  <NA> 
    ## 10 US1NYAB0001 2007-11-10    NA    NA    NA <NA>  <NA> 
    ## # ℹ 2,595,166 more rows

We want to know the size and the structure of the data. \## Overview of
the data

``` r
dim(ny_noaa)
```

    ## [1] 2595176       7

``` r
glimpse(ny_noaa)
```

    ## Rows: 2,595,176
    ## Columns: 7
    ## $ id   <chr> "US1NYAB0001", "US1NYAB0001", "US1NYAB0001", "US1NYAB0001", "US1N…
    ## $ date <date> 2007-11-01, 2007-11-02, 2007-11-03, 2007-11-04, 2007-11-05, 2007…
    ## $ prcp <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3…
    ## $ snow <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3…
    ## $ snwd <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0…
    ## $ tmax <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ tmin <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

``` r
summary(ny_noaa)
```

    ##       id                 date                 prcp               snow       
    ##  Length:2595176     Min.   :1981-01-01   Min.   :    0.00   Min.   :  -13   
    ##  Class :character   1st Qu.:1988-11-29   1st Qu.:    0.00   1st Qu.:    0   
    ##  Mode  :character   Median :1997-01-21   Median :    0.00   Median :    0   
    ##                     Mean   :1997-01-01   Mean   :   29.82   Mean   :    5   
    ##                     3rd Qu.:2005-09-01   3rd Qu.:   23.00   3rd Qu.:    0   
    ##                     Max.   :2010-12-31   Max.   :22860.00   Max.   :10160   
    ##                                          NA's   :145838     NA's   :381221  
    ##       snwd            tmax               tmin          
    ##  Min.   :   0.0   Length:2595176     Length:2595176    
    ##  1st Qu.:   0.0   Class :character   Class :character  
    ##  Median :   0.0   Mode  :character   Mode  :character  
    ##  Mean   :  37.3                                        
    ##  3rd Qu.:   0.0                                        
    ##  Max.   :9195.0                                        
    ##  NA's   :591786

``` r
ny_noaa |>
  summarise(
    total_rows = n(),
    total_stations = n_distinct(id),
    missing_date = sum(is.na(date)),
    missing_prcp = sum(is.na(prcp)),
    missing_snow = sum(is.na(snow)),
    missing_snwd = sum(is.na(snwd)),
    missing_tmax = sum(is.na(tmax)),
    missing_tmin = sum(is.na(tmin))
  )
```

    ## # A tibble: 1 × 8
    ##   total_rows total_stations missing_date missing_prcp missing_snow missing_snwd
    ##        <int>          <int>        <int>        <int>        <int>        <int>
    ## 1    2595176            747            0       145838       381221       591786
    ## # ℹ 2 more variables: missing_tmax <int>, missing_tmin <int>

**A short description based on the data:** The `ny_noaa` dataset
contains daily weather observations from 2,595,176 records across 747
weather stations in New York State. It has 7 variables, including
station ID (`id`), observation date (`date`), precipitation (`prcp`),
snowfall (`snow`), snow depth (`snwd`), maximum temperature (`tmax`),
and minimum temperature (`tmin`). The observations span from 1981 to
2010. Missing data are an important issue. Precipitation has 145,838
missing values, snowfall has 381,221, and snow depth has 591,786. The
temperature variables (tmax and tmin) are stored as character variables,
and their missing values should also be examined.

## Data Cleaning

``` r
noaa_df =
  ny_noaa |>
  janitor::clean_names() |>
  mutate(
    year = as.numeric(str_sub(date, 1, 4)),
    month = as.numeric(str_sub(date, 6, 7)),
    day = as.numeric(str_sub(date, 9, 10)),
    tmax = as.numeric(tmax) / 10,
    tmin = as.numeric(tmin) / 10,
    prcp = as.numeric(prcp) / 10,
    snow = as.numeric(snow),
    snwd = as.numeric(snwd)
  )
```

Snow falls

``` r
noaa_df |>
  count(snow, sort = TRUE) |>
  slice_head(n = 10)
```

    ## # A tibble: 10 × 2
    ##     snow       n
    ##    <dbl>   <int>
    ##  1     0 2008508
    ##  2    NA  381221
    ##  3    25   31022
    ##  4    13   23095
    ##  5    51   18274
    ##  6    76   10173
    ##  7     8    9962
    ##  8     5    9748
    ##  9    38    9197
    ## 10     3    8790

The most commonly observed snowfall value is expected to be 0 mm because
snowfall does not occur on most observation days.

## Average max temperature in January and in July

``` r
temp_df =
  noaa_df |>
  filter(month %in% c(1, 7)) |>
  group_by(id, year, month) |>
  summarise(
    mean_tmax = mean(tmax, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    month = factor(
      month,
      levels = c(1, 7),
      labels = c("January", "July")
    )
  )

ggplot(
  temp_df,
  aes(
    x = year,
    y = mean_tmax,
    group = id
  )
) +
  geom_line(alpha = 0.15, color = "blue") +
  facet_grid(. ~ month) +
  labs(
    title = "Average Maximum Temperature in January and July",
    x = "Year",
    y = "Average Maximum Temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 11)
  )
```

![](datascience_hw3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Summary of the plots:** July has much higher average maximum
temperatures (mostly around 25–30°C) than January (mostly around
−5–5°C). There is variation among stations in both months, shown by the
many overlapping lines. January temperatures appear more variable than
July temperatures. There is no obvious long-term increasing or
decreasing trend in either panel. Temperatures fluctuate from year to
year.

Potential outliers: A few stations show unusually low values. In July,
some observations fall well below the main cluster, including one in the
late 1980s. In January, a few observations fall below −10°C.

## tmax vs tmin for the full dataset

``` r
p_temp =
  noaa_df |>
  filter(!is.na(tmax), !is.na(tmin)) |>
  ggplot(aes(x = tmin, y = tmax)) +
  geom_bin2d(bins = 50) +
  labs(
    title = "Maximum vs. Minimum Temperature",
    x = "Minimum Temperature (°C)",
    y = "Maximum Temperature (°C)",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )
```

## Distribution of snowfall values greater than 0 and less than 100

``` r
p_snow =
  noaa_df |>
  filter(!is.na(snow), snow > 0, snow < 100) |>
  ggplot(aes(x = factor(year), y = snow)) +
  geom_violin(
    fill = "lightblue",
    color = "gray",
    alpha = 0.8,
    scale = "width",
    width = 0.9,
    trim = TRUE
  ) +
  scale_x_discrete(breaks = seq(1981, 2010, by = 2)) +
  labs(
    title = "Snowfall Distribution by Year",
    x = "Year",
    y = "Snowfall (mm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )
```

``` r
library(patchwork)
p_temp + p_snow +
  plot_layout(widths = c(1, 1.5))
```

![](datascience_hw3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**Maximum vs. Minimum Temperature:** The plot shows a positive
association between minimum and maximum temperatures. Days with higher
minimum temperatures generally also have higher maximum temperatures.
Most observations are concentrated in the central region, while a few
extreme values appear to be potential outliers.

**Snowfall Distribution by Year:** The snowfall distributions appear
similar across years, with multiple peaks around certain snowfall
amounts. The distributions are spread across a wide range of snowfall
amounts. Overall, there is no obvious change in the snowfall
distribution over time.
