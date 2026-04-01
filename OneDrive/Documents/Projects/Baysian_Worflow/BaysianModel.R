---
  title: "Initial_Modelling_for_priors"
format: html
editor: 
  markdown: 
  wrap: 72
---
  ```{r}
#| message: false
#| warning: false
library(tidyverse)
library(DHARMa)
library(glmmTMB)
library(here)
library(splines)
library(MASS)
library(ggeffects)
library(car)
library(dotwhisker)
library(brms)
library(kingCoData)
library(ggmosaic)
library(GGally)
library(lme4)
library(performance)
library(lubridate)
```

## Data Sources
...



## Preprocessing and Exploration
```{r}
data(kingco_sales)

kingco_sales <- kingco_sales |>
  dplyr::select(-c(pinx, sale_id, subdivision)) |>
  mutate(
    was_renod     = (year_reno > 0),
    has_bsmnt_gar = (garb_sqft > 0),
    has_att_gar   = (gara_sqft > 0),
    has_bsmt      = (sqft_fbsmt > 0),
    wfnt          = as.factor(wfnt > 0),
    year_sold     = year(ymd(sale_date))
  )

kingco_sales2015 <- kingco_sales |>
  filter(year_sold <= 2015) |> 
  slice_sample(n = 15000)

kingco_sales2015 <- kingco_sales2015 |> 
  mutate(
    sale_warning = na_if(sale_warning, "   ")
  ) |> 
  mutate(
    sale_warning = !is.na(sale_warning)
  )

```

```{r}
#| message: false

# Combine view columns into single binary
kingco_sales2015 |> 
  dplyr::select(starts_with('view_')) |> 
  pivot_longer(cols = everything(), values_to = 'View Indicator', names_to = 'View Type') |> 
  ggplot(mapping = aes(x = `View Indicator`)) + 
  geom_bar() + 
  facet_wrap(~ `View Type`)

kingco_sales2015 <- kingco_sales2015 |>
  mutate(view = rowSums(across(starts_with("view_"))) > 0) |>
  dplyr::select(-starts_with("view_"))

kingco_sales2015 |> 
  ggplot(mapping = aes(x = view)) +
  geom_bar()

# Drop unused join columns
kingco_sales2015 <- kingco_sales2015 |>
  dplyr::select(-c(join_year, join_status))
```

## Changing things to factors
```{r}
#| message: false

catagoricals <- c('sale_warning', 'area', 'city', 'zoning', 'golf', 'wfnt', 'greenbelt','present_use', 'noise_traffic', 'view', 'year_sold', 'stories', 'stories', 'was_renod', 'has_bsmnt_gar', 'has_att_gar', 'has_bsmt', 'condition')


kingco_sales2015 <- kingco_sales2015 |>
  mutate(across(all_of(categoricals), as.factor))

kingco_sales2015 |>
  dplyr::select(all_of(categoricals)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  xlab(NULL) + ylab("Count")


```

## Standardize Continuous Predictors
```{r}
#| message: false

kingco_sales2015 <- kingco_sales2015 |>
  mutate(across(where(is.numeric) & !sale_price, ~ as.numeric(scale(.x))))
```