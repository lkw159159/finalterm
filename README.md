Rmarkdown for Final test
================
Ki Wook LEE
2022-12-06

\###Load the library & data

``` r
library(tableone);library(data.table);library(magrittr);library(survival);library(jstable)
url <- "https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv"
dt <- fread(url, header=T)
```

``` r
head(dt)
```

    ##    EXMD_BZ_YYYY RN_INDI HME_YYYYMM Q_PHX_DX_STK Q_PHX_DX_HTDZ Q_PHX_DX_HTN
    ## 1:         2009  562083     200909            0             0            1
    ## 2:         2009  334536     200911            0             0            0
    ## 3:         2009  911867     200903            0             0            0
    ## 4:         2009  183321     200908           NA            NA           NA
    ## 5:         2009  942671     200909           NA            NA           NA
    ## 6:         2009  979358     200912           NA            NA           NA
    ##    Q_PHX_DX_DM Q_PHX_DX_DLD Q_PHX_DX_PTB Q_HBV_AG Q_SMK_YN Q_DRK_FRQ_V09N HGHT
    ## 1:           0            0           NA        3        1              0  144
    ## 2:           0            0           NA        2        1              0  162
    ## 3:           0            0           NA        3        1              0  163
    ## 4:          NA           NA           NA        3        1              0  152
    ## 5:          NA           NA           NA        3        1              0  159
    ## 6:          NA           NA           NA        2        1              0  157
    ##    WGHT WSTC  BMI VA_LT VA_RT BP_SYS BP_DIA URN_PROT  HGB FBS TOT_CHOL  TG HDL
    ## 1:   61   90 29.4   0.7   0.8    120     80        1 12.6 117      264 128  60
    ## 2:   51   63 19.4   0.8   1.0    120     80        1 13.8  96      169  92  70
    ## 3:   65   82 24.5   0.7   0.6    130     80        1 15.0 118      216 132  55
    ## 4:   51   70 22.1   0.8   0.9    101     62        1 13.1  90      199 100  65
    ## 5:   50   73 19.8   0.7   0.8    132     78        1 13.0  92      162  58  40
    ## 6:   55   73 22.3   1.5   1.5    110     70        1 11.9 100      192 109  53
    ##    LDL CRTN SGOT SGPT GGT GFR
    ## 1: 179  0.9   25   20  25  59
    ## 2:  80  0.9   18   15  28  74
    ## 3: 134  0.8   26   30  30  79
    ## 4: 114  0.9   18   14  11  61
    ## 5: 111  0.9   24   23  15  49
    ## 6: 117  0.7   15   12  14  83

<br/>

### Q.1 Table 만들기

``` r
data = dt[,c('HGHT',"WGHT", 'BMI', 'HDL', 'LDL', 'Q_SMK_YN', 'Q_HBV_AG','EXMD_BZ_YYYY')]
data$Q_SMK_YN = as.factor(data$Q_SMK_YN)
data$Q_HBV_AG = as.factor(data$Q_HBV_AG)
```

``` r
CreateTableOne(vars = colnames(data)[-8], strata= 'EXMD_BZ_YYYY', data=data)
```

    ##                   Stratified by EXMD_BZ_YYYY
    ##                    2009            2010           2011           2012          
    ##   n                   214             236            223            234        
    ##   HGHT (mean (SD)) 164.08 (9.13)   164.93 (8.73)  164.15 (9.58)  164.92 (9.20) 
    ##   WGHT (mean (SD))  64.33 (12.50)   65.14 (12.24)  64.90 (12.84)  65.82 (12.41)
    ##   BMI (mean (SD))   23.76 (3.39)    23.83 (3.27)   23.96 (3.46)   24.09 (3.34) 
    ##   HDL (mean (SD))   55.61 (15.26)   55.16 (13.96)  57.31 (38.67)  55.95 (13.49)
    ##   LDL (mean (SD))  150.95 (547.09) 112.99 (35.67) 112.94 (33.56) 117.53 (33.05)
    ##   Q_SMK_YN (%)                                                                 
    ##      1                125 (59.0)      132 (55.9)     140 (62.8)     146 (62.4) 
    ##      2                 34 (16.0)       42 (17.8)      35 (15.7)      36 (15.4) 
    ##      3                 53 (25.0)       62 (26.3)      48 (21.5)      52 (22.2) 
    ##   Q_HBV_AG (%)                                                                 
    ##      1                 14 ( 6.6)        6 ( 2.5)      12 ( 5.4)      12 ( 5.1) 
    ##      2                129 (60.8)      147 (62.3)     147 (65.9)     160 (68.4) 
    ##      3                 69 (32.5)       83 (35.2)      64 (28.7)      62 (26.5) 
    ##                   Stratified by EXMD_BZ_YYYY
    ##                    2013           2014           2015           p      test
    ##   n                   243            254            240                    
    ##   HGHT (mean (SD)) 164.91 (8.87)  164.32 (9.37)  164.48 (9.47)   0.890     
    ##   WGHT (mean (SD))  64.91 (12.27)  64.47 (12.13)  66.08 (13.36)  0.705     
    ##   BMI (mean (SD))   23.75 (3.33)   23.78 (3.32)   24.28 (3.54)   0.541     
    ##   HDL (mean (SD))   55.46 (14.61)  55.64 (13.66)  56.27 (14.96)  0.935     
    ##   LDL (mean (SD))  111.16 (32.33) 116.55 (65.55) 111.53 (31.83)  0.371     
    ##   Q_SMK_YN (%)                                                   0.856     
    ##      1                141 (58.0)     157 (61.8)     154 (64.2)             
    ##      2                 35 (14.4)      38 (15.0)      36 (15.0)             
    ##      3                 67 (27.6)      59 (23.2)      50 (20.8)             
    ##   Q_HBV_AG (%)                                                   0.073     
    ##      1                 10 ( 4.1)       9 ( 3.5)      14 ( 5.8)             
    ##      2                168 (69.1)     189 (74.4)     162 (67.5)             
    ##      3                 65 (26.7)      56 (22.0)      64 (26.7)

<br/>

### Q.2 선형회귀, 로지스틱, 콕스생존분석 table 만들기

#### Q.2-1

``` r
head(colon)
```

    ##   id study      rx sex age obstruct perfor adhere nodes status differ extent
    ## 1  1     1 Lev+5FU   1  43        0      0      0     5      1      2      3
    ## 2  1     1 Lev+5FU   1  43        0      0      0     5      1      2      3
    ## 3  2     1 Lev+5FU   1  63        0      0      0     1      0      2      3
    ## 4  2     1 Lev+5FU   1  63        0      0      0     1      0      2      3
    ## 5  3     1     Obs   0  71        0      0      1     7      1      2      2
    ## 6  3     1     Obs   0  71        0      0      1     7      1      2      2
    ##   surg node4 time etype
    ## 1    0     1 1521     2
    ## 2    0     1  968     1
    ## 3    0     0 3087     2
    ## 4    0     0 3087     1
    ## 5    0     1  963     2
    ## 6    0     1  542     1

``` r
lin.reg = glm(time ~ rx + age + sex, data=colon)
table1 = glmshow.display(lin.reg)
```

|              | crude coeff.(95%CI)    | crude P value | adj. coeff.(95%CI)    | adj. P value |
|:-------------|:-----------------------|:--------------|:----------------------|:-------------|
| rx: ref.=Obs | NA                     | NA            | NA                    | NA           |
| Lev          | 24.66 (-79.49,128.82)  | 0.643         | 22.98 (-81.3,127.27)  | 0.666        |
| Lev+5FU      | 271.07 (166.41,375.74) | \< 0.001      | 273.05 (168.19,377.9) | \< 0.001     |
| age          | 0.38 (-3.22,3.99)      | 0.835         | 0.37 (-3.21,3.95)     | 0.84         |
| sex          | 13.93 (-72.26,100.12)  | 0.751         | 32.67 (-53.21,118.55) | 0.456        |

Linear regression predicting time

#### Q.2-2

``` r
head(colon)
```

    ##   id study      rx sex age obstruct perfor adhere nodes status differ extent
    ## 1  1     1 Lev+5FU   1  43        0      0      0     5      1      2      3
    ## 2  1     1 Lev+5FU   1  43        0      0      0     5      1      2      3
    ## 3  2     1 Lev+5FU   1  63        0      0      0     1      0      2      3
    ## 4  2     1 Lev+5FU   1  63        0      0      0     1      0      2      3
    ## 5  3     1     Obs   0  71        0      0      1     7      1      2      2
    ## 6  3     1     Obs   0  71        0      0      1     7      1      2      2
    ##   surg node4 time etype
    ## 1    0     1 1521     2
    ## 2    0     1  968     1
    ## 3    0     0 3087     2
    ## 4    0     0 3087     1
    ## 5    0     1  963     2
    ## 6    0     1  542     1

``` r
log.reg = glm(status ~ rx +age + sex, data=colon, family = binomial)
table2 = glmshow.display(log.reg)
```

|              | crude OR.(95%CI) | crude P value | adj. OR.(95%CI)  | adj. P value |
|:-------------|:-----------------|:--------------|:-----------------|:-------------|
| rx: ref.=Obs | NA               | NA            | NA               | NA           |
| Lev          | 0.96 (0.77,1.2)  | 0.709         | 0.96 (0.77,1.2)  | 0.747        |
| Lev+5FU      | 0.55 (0.44,0.68) | \< 0.001      | 0.54 (0.43,0.68) | \< 0.001     |
| age          | 1 (0.99,1)       | 0.296         | 1 (0.99,1)       | 0.294        |
| sex          | 0.97 (0.81,1.17) | 0.758         | 0.93 (0.77,1.12) | 0.454        |

Logistic regression predicting status

#### Q.2-3

``` r
head(colon)
```

    ##   id study      rx sex age obstruct perfor adhere nodes status differ extent
    ## 1  1     1 Lev+5FU   1  43        0      0      0     5      1      2      3
    ## 2  1     1 Lev+5FU   1  43        0      0      0     5      1      2      3
    ## 3  2     1 Lev+5FU   1  63        0      0      0     1      0      2      3
    ## 4  2     1 Lev+5FU   1  63        0      0      0     1      0      2      3
    ## 5  3     1     Obs   0  71        0      0      1     7      1      2      2
    ## 6  3     1     Obs   0  71        0      0      1     7      1      2      2
    ##   surg node4 time etype
    ## 1    0     1 1521     2
    ## 2    0     1  968     1
    ## 3    0     0 3087     2
    ## 4    0     0 3087     1
    ## 5    0     1  963     2
    ## 6    0     1  542     1

``` r
cox.pro = coxph(Surv(time, status) ~ rx + age + sex, data = colon, model = T)
table3 <- cox2.display(cox.pro)
```

|              | crude HR(95%CI)  | crude P value | adj. HR(95%CI)   | adj. P value |
|:-------------|:-----------------|:--------------|:-----------------|:-------------|
| rx: ref.=Obs | NA               | NA            | NA               | NA           |
| Lev          | 0.98 (0.84,1.14) | 0.786         | 0.98 (0.84,1.14) | 0.811        |
| Lev+5FU      | 0.64 (0.55,0.76) | \< 0.001      | 0.64 (0.55,0.76) | \< 0.001     |
| age          | 1 (0.99,1)       | 0.382         | 1 (0.99,1)       | 0.468        |
| sex          | 0.97 (0.85,1.1)  | 0.61          | 0.95 (0.84,1.08) | 0.446        |

Cox model on time (‘time’) to event (‘status’)


### Q.3 샤이니 웹 앱 만든 후 ShinyApps.io 배포하기

https://lkw159159.shinyapps.io/finalterm/
