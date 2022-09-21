---
title: "Housing Project"
author: "Cyrus Tadjiki"
date: "January 19th, 2022"
output: 
  html_document:
    theme: united
    highlight: tango 
    toc: yes
    toc_depth: 4
    toc_float: yes
    keep_md: true
---

<style type="text/css">

h1.title {
  font-size: 38px;
  color: Black;
  text-align: center;
}
h4.author { /* Header 4 - and the author and data headers use this too  */
    font-size: 18px;
  font-family: "Times New Roman", Times, serif;
  color: Black;
  text-align: center;
}
h4.date { /* Header 4 - and the author and data headers use this too  */
  font-size: 18px;
  font-family: "Times New Roman", Times, serif;
  color: Black;
  text-align: center;
}
</style>



# Set Up

```r
library(pacman)
p_load(tidyverse, magrittr, janitor, skimr, broom, MLmetrics)
```


# Cleaning Data

```r
# Loading training dataset
df = read_csv("train.csv")
# Kaggle Method
# df = "../input/house-prices-advanced-regression-techniques//train.csv" %>% read_csv()


# Cleaning Data
df %>% dim() # Check dimension
```

```
## [1] 1460   81
```

```r
unclean = df # Making a copy of old dataframe
df = clean_names(df) 

df = subset(df, select = -c(bsmt_qual,bsmt_cond,bsmt_exposure,
                            bsmt_fin_type1,bsmt_fin_type2,alley,
                            garage_yr_blt, lot_frontage)) # removing columns

df$pool = ifelse(df$pool_area > 1, 1, 0) # Binary Varible for having a pool

df$fireplace_good_qu =  ifelse(df$fireplace_qu == "Ex"| # Making all Average, Good, and
                               df$fireplace_qu == "Gd"| # Excellent Quality Fireplaces = 1
                               df$fireplace_qu == "TA", 1, 0) 

df$fireplace_good_qu[is.na(df$fireplace_good_qu)] = 0 # Making NAs zero
# df$fireplace_good_qu


df$fence_good =  ifelse(df$fence == "GdPrv"| df$fence == "GdWo", 1, 0)
df$fence_good[is.na(df$fence_good)] = 0 # Making NAs zero
# df$fence_good

df$mas_vnr_area[is.na(df$mas_vnr_area)] = 0 # Making NAs zero
# df$mas_vnr_area


df$heating_qc_rank = ifelse(df$heating_qc == "Ex", 5,
                     ifelse(df$heating_qc == "Gd", 4,
                     ifelse(df$heating_qc == "TA", 3,
                     ifelse(df$heating_qc == "Fa", 2,
                     ifelse(df$heating_qc == "Po", 1, 0)))))

# df$heating_qc_rank


df$central_air_ac = ifelse(df$central_air == "Y", 1, 0)
# df$central_air_ac



df$paved_drive_qu = ifelse(df$paved_drive == "Y", 2,
                    ifelse(df$paved_drive == "P", 1,0))
# df$paved_drive_qu

# names(df)[36] = "first_flr_sf"
# names(df)[37] = "second_flr_sf"
# names(df)[61] = "three_ssn_porch"

df = df %>% 
  rename(first_flr_sf = "x1st_flr_sf",
         second_flr_sf = "x2nd_flr_sf",
         three_ssn_porch = "x3ssn_porch")
```

# Viewing Data

```r
names(df)
```

```
##  [1] "id"                "ms_sub_class"      "ms_zoning"        
##  [4] "lot_area"          "street"            "lot_shape"        
##  [7] "land_contour"      "utilities"         "lot_config"       
## [10] "land_slope"        "neighborhood"      "condition1"       
## [13] "condition2"        "bldg_type"         "house_style"      
## [16] "overall_qual"      "overall_cond"      "year_built"       
## [19] "year_remod_add"    "roof_style"        "roof_matl"        
## [22] "exterior1st"       "exterior2nd"       "mas_vnr_type"     
## [25] "mas_vnr_area"      "exter_qual"        "exter_cond"       
## [28] "foundation"        "bsmt_fin_sf1"      "bsmt_fin_sf2"     
## [31] "bsmt_unf_sf"       "total_bsmt_sf"     "heating"          
## [34] "heating_qc"        "central_air"       "electrical"       
## [37] "first_flr_sf"      "second_flr_sf"     "low_qual_fin_sf"  
## [40] "gr_liv_area"       "bsmt_full_bath"    "bsmt_half_bath"   
## [43] "full_bath"         "half_bath"         "bedroom_abv_gr"   
## [46] "kitchen_abv_gr"    "kitchen_qual"      "tot_rms_abv_grd"  
## [49] "functional"        "fireplaces"        "fireplace_qu"     
## [52] "garage_type"       "garage_finish"     "garage_cars"      
## [55] "garage_area"       "garage_qual"       "garage_cond"      
## [58] "paved_drive"       "wood_deck_sf"      "open_porch_sf"    
## [61] "enclosed_porch"    "three_ssn_porch"   "screen_porch"     
## [64] "pool_area"         "pool_qc"           "fence"            
## [67] "misc_feature"      "misc_val"          "mo_sold"          
## [70] "yr_sold"           "sale_type"         "sale_condition"   
## [73] "sale_price"        "pool"              "fireplace_good_qu"
## [76] "fence_good"        "heating_qc_rank"   "central_air_ac"   
## [79] "paved_drive_qu"
```

```r
skim(df) # viewing the columns
```


Table: Data summary

|                         |     |
|:------------------------|:----|
|Name                     |df   |
|Number of rows           |1460 |
|Number of columns        |79   |
|_______________________  |     |
|Column type frequency:   |     |
|character                |37   |
|numeric                  |42   |
|________________________ |     |
|Group variables          |None |


**Variable type: character**

|skim_variable  | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:--------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|ms_zoning      |         0|          1.00|   2|   7|     0|        5|          0|
|street         |         0|          1.00|   4|   4|     0|        2|          0|
|lot_shape      |         0|          1.00|   3|   3|     0|        4|          0|
|land_contour   |         0|          1.00|   3|   3|     0|        4|          0|
|utilities      |         0|          1.00|   6|   6|     0|        2|          0|
|lot_config     |         0|          1.00|   3|   7|     0|        5|          0|
|land_slope     |         0|          1.00|   3|   3|     0|        3|          0|
|neighborhood   |         0|          1.00|   5|   7|     0|       25|          0|
|condition1     |         0|          1.00|   4|   6|     0|        9|          0|
|condition2     |         0|          1.00|   4|   6|     0|        8|          0|
|bldg_type      |         0|          1.00|   4|   6|     0|        5|          0|
|house_style    |         0|          1.00|   4|   6|     0|        8|          0|
|roof_style     |         0|          1.00|   3|   7|     0|        6|          0|
|roof_matl      |         0|          1.00|   4|   7|     0|        8|          0|
|exterior1st    |         0|          1.00|   5|   7|     0|       15|          0|
|exterior2nd    |         0|          1.00|   5|   7|     0|       16|          0|
|mas_vnr_type   |         8|          0.99|   4|   7|     0|        4|          0|
|exter_qual     |         0|          1.00|   2|   2|     0|        4|          0|
|exter_cond     |         0|          1.00|   2|   2|     0|        5|          0|
|foundation     |         0|          1.00|   4|   6|     0|        6|          0|
|heating        |         0|          1.00|   4|   5|     0|        6|          0|
|heating_qc     |         0|          1.00|   2|   2|     0|        5|          0|
|central_air    |         0|          1.00|   1|   1|     0|        2|          0|
|electrical     |         1|          1.00|   3|   5|     0|        5|          0|
|kitchen_qual   |         0|          1.00|   2|   2|     0|        4|          0|
|functional     |         0|          1.00|   3|   4|     0|        7|          0|
|fireplace_qu   |       690|          0.53|   2|   2|     0|        5|          0|
|garage_type    |        81|          0.94|   6|   7|     0|        6|          0|
|garage_finish  |        81|          0.94|   3|   3|     0|        3|          0|
|garage_qual    |        81|          0.94|   2|   2|     0|        5|          0|
|garage_cond    |        81|          0.94|   2|   2|     0|        5|          0|
|paved_drive    |         0|          1.00|   1|   1|     0|        3|          0|
|pool_qc        |      1453|          0.00|   2|   2|     0|        3|          0|
|fence          |      1179|          0.19|   4|   5|     0|        4|          0|
|misc_feature   |      1406|          0.04|   4|   4|     0|        4|          0|
|sale_type      |         0|          1.00|   2|   5|     0|        9|          0|
|sale_condition |         0|          1.00|   6|   7|     0|        6|          0|


**Variable type: numeric**

|skim_variable     | n_missing| complete_rate|      mean|       sd|    p0|       p25|      p50|       p75|   p100|hist                                     |
|:-----------------|---------:|-------------:|---------:|--------:|-----:|---------:|--------:|---------:|------:|:----------------------------------------|
|id                |         0|             1|    730.50|   421.61|     1|    365.75|    730.5|   1095.25|   1460|▇▇▇▇▇ |
|ms_sub_class      |         0|             1|     56.90|    42.30|    20|     20.00|     50.0|     70.00|    190|▇▅▂▁▁ |
|lot_area          |         0|             1|  10516.83|  9981.26|  1300|   7553.50|   9478.5|  11601.50| 215245|▇▁▁▁▁ |
|overall_qual      |         0|             1|      6.10|     1.38|     1|      5.00|      6.0|      7.00|     10|▁▂▇▅▁ |
|overall_cond      |         0|             1|      5.58|     1.11|     1|      5.00|      5.0|      6.00|      9|▁▁▇▅▁ |
|year_built        |         0|             1|   1971.27|    30.20|  1872|   1954.00|   1973.0|   2000.00|   2010|▁▂▃▆▇ |
|year_remod_add    |         0|             1|   1984.87|    20.65|  1950|   1967.00|   1994.0|   2004.00|   2010|▅▂▂▃▇ |
|mas_vnr_area      |         0|             1|    103.12|   180.73|     0|      0.00|      0.0|    164.25|   1600|▇▁▁▁▁ |
|bsmt_fin_sf1      |         0|             1|    443.64|   456.10|     0|      0.00|    383.5|    712.25|   5644|▇▁▁▁▁ |
|bsmt_fin_sf2      |         0|             1|     46.55|   161.32|     0|      0.00|      0.0|      0.00|   1474|▇▁▁▁▁ |
|bsmt_unf_sf       |         0|             1|    567.24|   441.87|     0|    223.00|    477.5|    808.00|   2336|▇▅▂▁▁ |
|total_bsmt_sf     |         0|             1|   1057.43|   438.71|     0|    795.75|    991.5|   1298.25|   6110|▇▃▁▁▁ |
|first_flr_sf      |         0|             1|   1162.63|   386.59|   334|    882.00|   1087.0|   1391.25|   4692|▇▅▁▁▁ |
|second_flr_sf     |         0|             1|    346.99|   436.53|     0|      0.00|      0.0|    728.00|   2065|▇▃▂▁▁ |
|low_qual_fin_sf   |         0|             1|      5.84|    48.62|     0|      0.00|      0.0|      0.00|    572|▇▁▁▁▁ |
|gr_liv_area       |         0|             1|   1515.46|   525.48|   334|   1129.50|   1464.0|   1776.75|   5642|▇▇▁▁▁ |
|bsmt_full_bath    |         0|             1|      0.43|     0.52|     0|      0.00|      0.0|      1.00|      3|▇▆▁▁▁ |
|bsmt_half_bath    |         0|             1|      0.06|     0.24|     0|      0.00|      0.0|      0.00|      2|▇▁▁▁▁ |
|full_bath         |         0|             1|      1.57|     0.55|     0|      1.00|      2.0|      2.00|      3|▁▇▁▇▁ |
|half_bath         |         0|             1|      0.38|     0.50|     0|      0.00|      0.0|      1.00|      2|▇▁▅▁▁ |
|bedroom_abv_gr    |         0|             1|      2.87|     0.82|     0|      2.00|      3.0|      3.00|      8|▁▇▂▁▁ |
|kitchen_abv_gr    |         0|             1|      1.05|     0.22|     0|      1.00|      1.0|      1.00|      3|▁▇▁▁▁ |
|tot_rms_abv_grd   |         0|             1|      6.52|     1.63|     2|      5.00|      6.0|      7.00|     14|▂▇▇▁▁ |
|fireplaces        |         0|             1|      0.61|     0.64|     0|      0.00|      1.0|      1.00|      3|▇▇▁▁▁ |
|garage_cars       |         0|             1|      1.77|     0.75|     0|      1.00|      2.0|      2.00|      4|▁▃▇▂▁ |
|garage_area       |         0|             1|    472.98|   213.80|     0|    334.50|    480.0|    576.00|   1418|▂▇▃▁▁ |
|wood_deck_sf      |         0|             1|     94.24|   125.34|     0|      0.00|      0.0|    168.00|    857|▇▂▁▁▁ |
|open_porch_sf     |         0|             1|     46.66|    66.26|     0|      0.00|     25.0|     68.00|    547|▇▁▁▁▁ |
|enclosed_porch    |         0|             1|     21.95|    61.12|     0|      0.00|      0.0|      0.00|    552|▇▁▁▁▁ |
|three_ssn_porch   |         0|             1|      3.41|    29.32|     0|      0.00|      0.0|      0.00|    508|▇▁▁▁▁ |
|screen_porch      |         0|             1|     15.06|    55.76|     0|      0.00|      0.0|      0.00|    480|▇▁▁▁▁ |
|pool_area         |         0|             1|      2.76|    40.18|     0|      0.00|      0.0|      0.00|    738|▇▁▁▁▁ |
|misc_val          |         0|             1|     43.49|   496.12|     0|      0.00|      0.0|      0.00|  15500|▇▁▁▁▁ |
|mo_sold           |         0|             1|      6.32|     2.70|     1|      5.00|      6.0|      8.00|     12|▃▆▇▃▃ |
|yr_sold           |         0|             1|   2007.82|     1.33|  2006|   2007.00|   2008.0|   2009.00|   2010|▇▇▇▇▅ |
|sale_price        |         0|             1| 180921.20| 79442.50| 34900| 129975.00| 163000.0| 214000.00| 755000|▇▅▁▁▁ |
|pool              |         0|             1|      0.00|     0.07|     0|      0.00|      0.0|      0.00|      1|▇▁▁▁▁ |
|fireplace_good_qu |         0|             1|      0.49|     0.50|     0|      0.00|      0.0|      1.00|      1|▇▁▁▁▇ |
|fence_good        |         0|             1|      0.08|     0.27|     0|      0.00|      0.0|      0.00|      1|▇▁▁▁▁ |
|heating_qc_rank   |         0|             1|      4.15|     0.96|     1|      3.00|      5.0|      5.00|      5|▁▁▅▂▇ |
|central_air_ac    |         0|             1|      0.93|     0.25|     0|      1.00|      1.0|      1.00|      1|▁▁▁▁▇ |
|paved_drive_qu    |         0|             1|      1.86|     0.50|     0|      2.00|      2.0|      2.00|      2|▁▁▁▁▇ |




# Subsetting to Numeric Columns

```r
num_cols = unlist(lapply(df, is.numeric)) # Identify numeric columns  
data_num = df[ , num_cols] # Subset numeric columns of data
head(data_num)
```

```
## # A tibble: 6 x 42
##      id ms_sub~1 lot_a~2 overa~3 overa~4 year_~5 year_~6 mas_v~7 bsmt_~8 bsmt_~9
##   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
## 1     1       60    8450       7       5    2003    2003     196     706       0
## 2     2       20    9600       6       8    1976    1976       0     978       0
## 3     3       60   11250       7       5    2001    2002     162     486       0
## 4     4       70    9550       7       5    1915    1970       0     216       0
## 5     5       60   14260       8       5    2000    2000     350     655       0
## 6     6       50   14115       5       5    1993    1995       0     732       0
## # ... with 32 more variables: bsmt_unf_sf <dbl>, total_bsmt_sf <dbl>,
## #   first_flr_sf <dbl>, second_flr_sf <dbl>, low_qual_fin_sf <dbl>,
## #   gr_liv_area <dbl>, bsmt_full_bath <dbl>, bsmt_half_bath <dbl>,
## #   full_bath <dbl>, half_bath <dbl>, bedroom_abv_gr <dbl>,
## #   kitchen_abv_gr <dbl>, tot_rms_abv_grd <dbl>, fireplaces <dbl>,
## #   garage_cars <dbl>, garage_area <dbl>, wood_deck_sf <dbl>,
## #   open_porch_sf <dbl>, enclosed_porch <dbl>, three_ssn_porch <dbl>, ...
```

# Methods
Here I ran different regressions with every numeric variable to attempt and identify what parameters are most significant for predicting housing prices.

```r
reg1 = lm(sale_price~.-id, data = data_num)
# tidy(reg1)
summary(reg1)
```

```
## 
## Call:
## lm(formula = sale_price ~ . - id, data = data_num)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -369000  -16006   -1725   13286  335993 
## 
## Coefficients: (2 not defined because of singularities)
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        3.954e+05  1.375e+06   0.288 0.773662    
## ms_sub_class      -1.668e+02  2.554e+01  -6.529 9.17e-11 ***
## lot_area           4.119e-01  9.830e-02   4.190 2.96e-05 ***
## overall_qual       1.679e+04  1.162e+03  14.448  < 2e-16 ***
## overall_cond       4.698e+03  1.035e+03   4.538 6.16e-06 ***
## year_built         3.602e+02  6.435e+01   5.598 2.60e-08 ***
## year_remod_add     1.388e+02  6.712e+01   2.068 0.038810 *  
## mas_vnr_area       2.988e+01  5.773e+00   5.175 2.60e-07 ***
## bsmt_fin_sf1       2.672e+01  4.669e+00   5.722 1.28e-08 ***
## bsmt_fin_sf2       1.496e+01  6.916e+00   2.164 0.030660 *  
## bsmt_unf_sf        1.290e+01  4.140e+00   3.115 0.001874 ** 
## total_bsmt_sf             NA         NA      NA       NA    
## first_flr_sf       4.810e+01  5.614e+00   8.568  < 2e-16 ***
## second_flr_sf      5.144e+01  4.890e+00  10.519  < 2e-16 ***
## low_qual_fin_sf    4.205e+01  1.922e+01   2.188 0.028835 *  
## gr_liv_area               NA         NA      NA       NA    
## bsmt_full_bath     7.961e+03  2.548e+03   3.124 0.001817 ** 
## bsmt_half_bath     2.786e+03  3.973e+03   0.701 0.483351    
## full_bath          2.994e+03  2.743e+03   1.091 0.275355    
## half_bath         -1.474e+03  2.601e+03  -0.567 0.570976    
## bedroom_abv_gr    -1.020e+04  1.663e+03  -6.133 1.11e-09 ***
## kitchen_abv_gr    -1.426e+04  5.110e+03  -2.791 0.005329 ** 
## tot_rms_abv_grd    5.009e+03  1.206e+03   4.153 3.48e-05 ***
## fireplaces         5.650e+03  2.763e+03   2.045 0.041077 *  
## garage_cars        7.656e+03  2.797e+03   2.737 0.006279 ** 
## garage_area        8.919e+00  9.506e+00   0.938 0.348257    
## wood_deck_sf       1.862e+01  7.817e+00   2.382 0.017343 *  
## open_porch_sf     -1.555e+01  1.485e+01  -1.047 0.295132    
## enclosed_porch     1.913e+01  1.643e+01   1.165 0.244404    
## three_ssn_porch    1.437e+01  3.051e+01   0.471 0.637868    
## screen_porch       6.416e+01  1.673e+01   3.836 0.000131 ***
## pool_area          1.395e+03  1.633e+02   8.540  < 2e-16 ***
## misc_val          -3.060e-01  1.803e+00  -0.170 0.865268    
## mo_sold           -5.446e+01  3.354e+02  -0.162 0.871009    
## yr_sold           -7.154e+02  6.834e+02  -1.047 0.295340    
## pool              -8.418e+05  9.596e+04  -8.772  < 2e-16 ***
## fireplace_good_qu -2.584e+03  3.568e+03  -0.724 0.469077    
## fence_good        -7.909e+03  3.423e+03  -2.311 0.020998 *  
## heating_qc_rank    3.053e+03  1.183e+03   2.581 0.009950 ** 
## central_air_ac    -1.140e+04  4.318e+03  -2.640 0.008377 ** 
## paved_drive_qu    -1.906e+03  2.098e+03  -0.908 0.363928    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33740 on 1421 degrees of freedom
## Multiple R-squared:  0.8243,	Adjusted R-squared:  0.8196 
## F-statistic: 175.4 on 38 and 1421 DF,  p-value: < 2.2e-16
```

```r
reg2 = lm(sale_price~central_air_ac, data = data_num)
# tidy(reg2)
summary(reg2)
```

```
## 
## Call:
## lm(formula = sale_price ~ central_air_ac, data = data_num)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -134187  -51187  -16187   30394  568813 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      105264       7892  13.339   <2e-16 ***
## central_air_ac    80923       8162   9.915   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 76920 on 1458 degrees of freedom
## Multiple R-squared:  0.06317,	Adjusted R-squared:  0.06252 
## F-statistic: 98.31 on 1 and 1458 DF,  p-value: < 2.2e-16
```

```r
reg3 = lm(sale_price~
          . # Everything
          -id-total_bsmt_sf-gr_liv_area # Dropping
          + poly(bsmt_unf_sf, 3) # Sig
          + poly(year_built, 3) # Sig
          + poly(bsmt_fin_sf1, 3) # Sig
          + poly(lot_area, 3), # Sig
          data = data_num)

# tidy(reg3)
summary(reg3)
```

```
## 
## Call:
## lm(formula = sale_price ~ . - id - total_bsmt_sf - gr_liv_area + 
##     poly(bsmt_unf_sf, 3) + poly(year_built, 3) + poly(bsmt_fin_sf1, 
##     3) + poly(lot_area, 3), data = data_num)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -419794  -13625    -433   11977  235049 
## 
## Coefficients: (4 not defined because of singularities)
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.315e+05  1.223e+06  -0.353 0.724284    
## ms_sub_class           -9.319e+01  2.465e+01  -3.780 0.000164 ***
## lot_area                5.637e-01  9.045e-02   6.233 6.05e-10 ***
## overall_qual            1.285e+04  1.099e+03  11.694  < 2e-16 ***
## overall_cond            7.324e+03  9.787e+02   7.483 1.27e-13 ***
## year_built              5.145e+02  6.480e+01   7.940 4.10e-15 ***
## year_remod_add          3.497e+01  6.532e+01   0.535 0.592449    
## mas_vnr_area            2.928e+01  5.202e+00   5.629 2.18e-08 ***
## bsmt_fin_sf1            3.055e+01  4.291e+00   7.118 1.74e-12 ***
## bsmt_fin_sf2            2.730e+01  6.207e+00   4.398 1.18e-05 ***
## bsmt_unf_sf             1.760e+01  3.839e+00   4.585 4.93e-06 ***
## first_flr_sf            5.255e+01  5.321e+00   9.874  < 2e-16 ***
## second_flr_sf           5.837e+01  4.412e+00  13.231  < 2e-16 ***
## low_qual_fin_sf         2.167e+01  1.712e+01   1.266 0.205855    
## bsmt_full_bath          2.597e+03  2.315e+03   1.122 0.262056    
## bsmt_half_bath         -8.053e+02  3.549e+03  -0.227 0.820540    
## full_bath              -1.028e+03  2.465e+03  -0.417 0.676714    
## half_bath              -2.133e+02  2.332e+03  -0.091 0.927121    
## bedroom_abv_gr         -8.202e+03  1.529e+03  -5.364 9.51e-08 ***
## kitchen_abv_gr         -1.694e+04  4.583e+03  -3.695 0.000228 ***
## tot_rms_abv_grd         3.358e+03  1.087e+03   3.090 0.002044 ** 
## fireplaces              7.541e+03  2.482e+03   3.038 0.002423 ** 
## garage_cars             2.789e+03  2.514e+03   1.109 0.267469    
## garage_area             1.490e+01  8.523e+00   1.748 0.080604 .  
## wood_deck_sf            1.382e+01  7.056e+00   1.959 0.050325 .  
## open_porch_sf          -1.087e+01  1.326e+01  -0.819 0.412765    
## enclosed_porch         -1.376e+00  1.463e+01  -0.094 0.925085    
## three_ssn_porch         3.129e+01  2.726e+01   1.148 0.251191    
## screen_porch            4.191e+01  1.495e+01   2.804 0.005114 ** 
## pool_area               2.945e+02  1.604e+02   1.836 0.066616 .  
## misc_val               -8.413e-01  1.605e+00  -0.524 0.600301    
## mo_sold                -1.181e+02  2.981e+02  -0.396 0.691980    
## yr_sold                -3.449e+02  6.082e+02  -0.567 0.570705    
## pool                   -1.351e+05  9.609e+04  -1.406 0.160011    
## fireplace_good_qu      -5.609e+03  3.176e+03  -1.766 0.077605 .  
## fence_good             -5.574e+03  3.060e+03  -1.821 0.068749 .  
## heating_qc_rank         9.912e+02  1.105e+03   0.897 0.370031    
## central_air_ac         -6.360e+03  3.916e+03  -1.624 0.104610    
## paved_drive_qu         -1.144e+02  1.899e+03  -0.060 0.951960    
## poly(bsmt_unf_sf, 3)1          NA         NA      NA       NA    
## poly(bsmt_unf_sf, 3)2   1.219e+05  3.367e+04   3.620 0.000305 ***
## poly(bsmt_unf_sf, 3)3   1.221e+05  3.223e+04   3.790 0.000157 ***
## poly(year_built, 3)1           NA         NA      NA       NA    
## poly(year_built, 3)2    1.933e+05  5.034e+04   3.840 0.000128 ***
## poly(year_built, 3)3    2.135e+05  3.412e+04   6.257 5.18e-10 ***
## poly(bsmt_fin_sf1, 3)1         NA         NA      NA       NA    
## poly(bsmt_fin_sf1, 3)2 -5.143e+05  4.048e+04 -12.705  < 2e-16 ***
## poly(bsmt_fin_sf1, 3)3 -3.927e+05  3.553e+04 -11.053  < 2e-16 ***
## poly(lot_area, 3)1             NA         NA      NA       NA    
## poly(lot_area, 3)2     -1.006e+05  3.529e+04  -2.850 0.004438 ** 
## poly(lot_area, 3)3      9.256e+04  3.315e+04   2.792 0.005313 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29930 on 1413 degrees of freedom
## Multiple R-squared:  0.8625,	Adjusted R-squared:  0.858 
## F-statistic: 192.7 on 46 and 1413 DF,  p-value: < 2.2e-16
```

```r
reg4 = lm(sale_price~
            . # Everything
          -id-total_bsmt_sf-gr_liv_area # Dropping NA Varibles
          # Getting rid of unsig ones
          -year_remod_add-low_qual_fin_sf-bsmt_full_bath
          -bsmt_half_bath-full_bath-half_bath-garage_cars
          -open_porch_sf-enclosed_porch-three_ssn_porch
          -misc_val-mo_sold-yr_sold-pool-heating_qc_rank
          -central_air_ac-paved_drive_qu
          + poly(bsmt_unf_sf, 3) # Sig
          + poly(year_built, 3) # Sig
          + poly(bsmt_fin_sf1, 3) # Sig
          + poly(lot_area, 3), # Sig
          data = data_num)

# tidy(reg4)
summary(reg4)
```

```
## 
## Call:
## lm(formula = sale_price ~ . - id - total_bsmt_sf - gr_liv_area - 
##     year_remod_add - low_qual_fin_sf - bsmt_full_bath - bsmt_half_bath - 
##     full_bath - half_bath - garage_cars - open_porch_sf - enclosed_porch - 
##     three_ssn_porch - misc_val - mo_sold - yr_sold - pool - heating_qc_rank - 
##     central_air_ac - paved_drive_qu + poly(bsmt_unf_sf, 3) + 
##     poly(year_built, 3) + poly(bsmt_fin_sf1, 3) + poly(lot_area, 
##     3), data = data_num)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -423456  -13527    -558   11893  234940 
## 
## Coefficients: (4 not defined because of singularities)
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1.080e+06  8.139e+04 -13.274  < 2e-16 ***
## ms_sub_class           -8.595e+01  2.422e+01  -3.549 0.000399 ***
## lot_area                5.792e-01  8.936e-02   6.482 1.24e-10 ***
## overall_qual            1.271e+04  1.086e+03  11.709  < 2e-16 ***
## overall_cond            7.367e+03  8.151e+02   9.038  < 2e-16 ***
## year_built              5.265e+02  4.151e+01  12.683  < 2e-16 ***
## mas_vnr_area            2.959e+01  5.147e+00   5.750 1.09e-08 ***
## bsmt_fin_sf1            3.083e+01  3.761e+00   8.199 5.35e-16 ***
## bsmt_fin_sf2            2.764e+01  5.966e+00   4.633 3.93e-06 ***
## bsmt_unf_sf             1.672e+01  3.783e+00   4.421 1.06e-05 ***
## first_flr_sf            5.272e+01  4.998e+00  10.547  < 2e-16 ***
## second_flr_sf           5.687e+01  3.663e+00  15.526  < 2e-16 ***
## bedroom_abv_gr         -8.468e+03  1.493e+03  -5.672 1.70e-08 ***
## kitchen_abv_gr         -1.695e+04  4.434e+03  -3.823 0.000137 ***
## tot_rms_abv_grd         3.553e+03  1.064e+03   3.338 0.000866 ***
## fireplaces              7.191e+03  2.461e+03   2.922 0.003535 ** 
## garage_area             2.154e+01  5.046e+00   4.269 2.09e-05 ***
## wood_deck_sf            1.501e+01  6.925e+00   2.168 0.030335 *  
## screen_porch            3.860e+01  1.465e+01   2.635 0.008515 ** 
## pool_area               7.169e+01  2.118e+01   3.385 0.000732 ***
## fireplace_good_qu      -5.186e+03  3.153e+03  -1.645 0.100193    
## fence_good             -5.364e+03  3.041e+03  -1.764 0.077931 .  
## poly(bsmt_unf_sf, 3)1          NA         NA      NA       NA    
## poly(bsmt_unf_sf, 3)2   1.201e+05  3.330e+04   3.607 0.000321 ***
## poly(bsmt_unf_sf, 3)3   1.164e+05  3.189e+04   3.650 0.000271 ***
## poly(year_built, 3)1           NA         NA      NA       NA    
## poly(year_built, 3)2    2.342e+05  4.036e+04   5.802 8.04e-09 ***
## poly(year_built, 3)3    2.290e+05  3.237e+04   7.073 2.37e-12 ***
## poly(bsmt_fin_sf1, 3)1         NA         NA      NA       NA    
## poly(bsmt_fin_sf1, 3)2 -5.523e+05  3.499e+04 -15.788  < 2e-16 ***
## poly(bsmt_fin_sf1, 3)3 -3.988e+05  3.471e+04 -11.490  < 2e-16 ***
## poly(lot_area, 3)1             NA         NA      NA       NA    
## poly(lot_area, 3)2     -1.058e+05  3.489e+04  -3.034 0.002458 ** 
## poly(lot_area, 3)3      9.684e+04  3.294e+04   2.940 0.003339 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29890 on 1430 degrees of freedom
## Multiple R-squared:  0.8613,	Adjusted R-squared:  0.8585 
## F-statistic: 306.2 on 29 and 1430 DF,  p-value: < 2.2e-16
```

# Loading and Cleaning Test Data Set

```r
## Loading Test Data
test_df = read.csv("test.csv")

# Cleaning Data
test_unclean = test_df # Making a copy of old dataframe
test_df = clean_names(test_df) 


test_df = test_df %>% 
  rename(first_flr_sf = "x1st_flr_sf",
         second_flr_sf = "x2nd_flr_sf",
         three_ssn_porch = "x3ssn_porch")

### Adding Varibles to test_df
test_df = subset(test_df, select = -c(bsmt_qual,bsmt_cond,bsmt_exposure,
                                      bsmt_fin_type1,bsmt_fin_type2,alley,
                                      garage_yr_blt, lot_frontage)) # removing columns

test_df$pool = ifelse(test_df$pool_area > 1, 1, 0) # Binary Varible for having a pool


# Making all Average, Good, and Excellent Quality Fireplaces = 1
test_df$fireplace_good_qu =  ifelse(test_df$fireplace_qu == "Ex"|
                                    test_df$fireplace_qu == "Gd"| 
                                   test_df$fireplace_qu == "TA", 1, 0) 

test_df$fireplace_good_qu[is.na(test_df$fireplace_good_qu)] = 0 # Making NAs zero
# test_df$fireplace_good_qu


test_df$fence_good =  ifelse(test_df$fence == "GdPrv"| test_df$fence == "GdWo", 1, 0)
test_df$fence_good[is.na(test_df$fence_good)] = 0 # Making NAs zero
# test_df$fence_good

test_df$mas_vnr_area[is.na(test_df$mas_vnr_area)] = 0 # Making NAs zero
# test_df$mas_vnr_area


test_df$heating_qc_rank = ifelse(test_df$heating_qc == "Ex", 5,
                          ifelse(test_df$heating_qc == "Gd", 4,
                          ifelse(test_df$heating_qc == "TA", 3,
                          ifelse(test_df$heating_qc == "Fa", 2,
                          ifelse(test_df$heating_qc == "Po", 1, 0)))))

# test_df$heating_qc_rank


test_df$central_air_ac = ifelse(test_df$central_air == "Y", 1, 0)
# test_df$central_air_ac


test_df$paved_drive_qu = ifelse(test_df$paved_drive == "Y", 2,
                         ifelse(test_df$paved_drive == "P", 1,0))
```

# Making Predictions with `predict()`

```r
pred = predict(object = reg1, newdata = test_df)
pred2 = predict(object = reg2, newdata = test_df)
pred3 = predict(object = reg3, newdata = test_df)
pred4 = predict(object = reg4, newdata = test_df)
```

## Removing NAs and prepping Kaggle submissions

```r
pred3[is.na(pred3)] = mean(pred3, na.rm = TRUE) # Making NAs mean(pred)


pred4[661] = mean(pred4, na.rm = TRUE)
pred4[1117] = mean(pred4, na.rm = TRUE)
sum(is.na(pred4))
```

```
## [1] 0
```

```r
# pred4[661]
# pred4[1117]

# Sanity Checks
# pred %>% length()
# pred2 %>% length()
# pred3 %>% length()
# pred4 %>% length()
# test_df %>% nrow()

submit_df = data.frame(
  Id = test_df$id,
  SalePrice = pred
)


submit_df2 = data.frame(
  Id = test_df$id,
  SalePrice = pred2
)

submit_df3 = data.frame(
  Id = test_df$id,
  SalePrice = pred3
)

submit_df4 = data.frame(
  Id = test_df$id,
  SalePrice = pred4
)

# View the first few lines of the dataset
head(submit_df3)  
```

```
##     Id SalePrice
## 1 1461  126191.1
## 2 1462  164843.1
## 3 1463  186276.9
## 4 1464  201684.6
## 5 1465  172712.8
## 6 1466  174549.5
```

```r
submit_df$SalePrice[is.na(submit_df$SalePrice)] = mean(pred) # Making NAs mean(pred)
# submit_df$SalePrice

# submit_df4$SalePrice[is.na(submit_df4$SalePrice)] = mean(pred4)

# Sanity Checks
# sum(is.na(test_df$mas_vnr_area))
# sum(is.na(submit_df2$SalePrice))
# sum(is.na(submit_df3$SalePrice))
# sum(is.na(submit_df4$SalePrice))

# mean(pred2)
```

# Saving Submissions

```r
write_csv(submit_df, file =  "submissions/cyrus-submit-01.csv")
write_csv(submit_df2, file = "submissions/cyrus-submit-01-sub2.csv")
write_csv(submit_df3, file = "submissions/cyrus-submit-01-sub3.csv")
write_csv(submit_df4, file = "submissions/cyrus-submit-01-sub4.csv")

## New Log Regs

log_reg1 = lm(sale_price~.-id, data = data_num)

# Submission Numbers
# 1108244040
# 5908414949
# 867241474
# 874791986
```

# Finding Training Performance (MSE)

```r
# data(cars)
# reg <- lm(log(dist) ~ log(speed), data = cars)
# MSE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
# y_pred = exp(reg$fitted.values)
# y_true = cars$dist
# MSE(y_pred, y_true)


y_pred1 = reg1$fitted.values
y_true1 = df$sale_price
mse1 = MSE(y_pred1, y_true1)


y_pred2 = reg2$fitted.values
y_true2 = df$sale_price
mse2 = MSE(y_pred2, y_true2)

y_pred3 = reg3$fitted.values
y_true3 = df$sale_price
mse3 = MSE(y_pred3, y_true3)

y_pred4 = reg4$fitted.values
y_true4 = df$sale_price
mse4 = MSE(y_pred4, y_true4)
```

