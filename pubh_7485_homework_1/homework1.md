PUBH 7485 Homework 1
================

``` r
# Read in data
load(here("OPT_Study_PUBH7485_8485_2022.Rdata"))
```

``` r
# Get variable names
dput(names(opt_causal))
```

    ## c("PID", "Clinic", "Group", "Age", "Black", "White", "Nat.Am", 
    ## "Asian", "Hisp", "Education", "Public.Asstce", "Hypertension", 
    ## "Diabetes", "BL.Diab.Type", "BMI", "Use.Tob", "BL.Cig.Day", "Use.Alc", 
    ## "BL.Drks.Day", "Drug.Add", "Prev.preg", "N.prev.preg", "Live.PTB", 
    ## "Any.stillbirth", "Spont.ab", "Induced.ab", "Any.live.ptb.sb.sp.ab.in.ab", 
    ## "N.living.kids", "N.qualifying.teeth", "BL.GE", "BL..BOP", "BL.PD.avg", 
    ## "BL..PD.4", "BL..PD.5", "BL.CAL.avg", "BL..CAL.2", "BL..CAL.3", 
    ## "BL.Calc.I", "BL.Pl.I", "Birth.outcome", "Preg.ended...37.wk", 
    ## "GA.at.outcome", "Birthweight", "Preg.ended")

``` r
# Vector of variables to summarize
myVars <- c("Clinic", "Age", "Black", "White", "Nat.Am", 
"Asian", "Hisp", "Education", "Public.Asstce", "Hypertension", 
"Diabetes", "BL.Diab.Type", "BMI", "Use.Tob", "BL.Cig.Day", "Use.Alc", 
"BL.Drks.Day", "Drug.Add", "Prev.preg", "N.prev.preg", "Live.PTB", 
"Any.stillbirth", "Spont.ab", "Induced.ab", "Any.live.ptb.sb.sp.ab.in.ab", 
"N.living.kids", "N.qualifying.teeth", "BL.GE", "BL..BOP", "BL.PD.avg", 
"BL..PD.4", "BL..PD.5", "BL.CAL.avg", "BL..CAL.2", "BL..CAL.3", 
"BL.Calc.I", "BL.Pl.I", "Birth.outcome", "Preg.ended...37.wk", 
"GA.at.outcome", "Birthweight", "Preg.ended")

# Vector of categorical variables that need transformation
catVars <- c("Clinic", "Group", "Black", "White", "Nat.Am", 
"Asian", "Hisp", "Education", "Public.Asstce", "Hypertension", 
"Diabetes", "BL.Diab.Type", "Use.Tob", "Use.Alc", "Drug.Add", "Prev.preg", "Live.PTB", "Any.stillbirth", "Spont.ab", "Induced.ab", "Any.live.ptb.sb.sp.ab.in.ab", "Birth.outcome", "Preg.ended...37.wk", "Preg.ended")

# Create Table 1
tab1 <- CreateTableOne(vars = myVars, strata = "Group", data = opt_causal, factorVars = catVars, test = FALSE)

tab1Mat <- print(tab1, smd = TRUE, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

## Save to CSV
write.csv(tab1Mat, file = "homework1_table1.csv")
```

``` r
# Get info about missing variables
summary(tab1)
```

    ## 
    ##      ### Summary of continuous variables ###
    ## 
    ## Group: C
    ##                      n miss p.miss    mean    sd median    p25  p75    min  max
    ## Age                406    0    0.0   25.91   5.5     25   22.0   30  16.00   44
    ## BMI                406   34    8.4   27.49   6.9     26   23.0   31  16.00   62
    ## BL.Cig.Day         406    9    2.2    0.82   3.1      0    0.0    0   0.00   30
    ## BL.Drks.Day        406   11    2.7    0.04   0.4      0    0.0    0   0.00    5
    ## N.prev.preg        406    3    0.7    1.87   1.8      1    0.0    3   0.00   11
    ## N.living.kids      406    3    0.7    1.87   1.8      1    0.0    3   0.00   11
    ## N.qualifying.teeth 406    0    0.0   14.33   6.7     13    9.0   19   4.00   28
    ## BL.GE              406    0    0.0    1.42   0.4      1    1.1    2   0.43    3
    ## BL..BOP            406    0    0.0   69.12  17.0     68   55.2   83  35.12  100
    ## BL.PD.avg          406    0    0.0    2.84   0.5      3    2.5    3   1.91    6
    ## BL..PD.4           406    0    0.0   24.80  15.9     20   13.1   33   3.70   84
    ## BL..PD.5           406    0    0.0    9.95  13.4      5    1.2   12   0.00   77
    ## BL.CAL.avg         406    0    0.0    1.37   0.7      1    0.9    2   0.18    5
    ## BL..CAL.2          406    0    0.0   41.29  25.3     38   19.0   63   2.38   96
    ## BL..CAL.3          406    0    0.0   14.03  16.1      8    2.4   21   0.00   86
    ## BL.Calc.I          406    0    0.0    1.13   0.6      1    0.6    2   0.03    3
    ## BL.Pl.I            406    0    0.0    1.23   0.5      1    0.9    2   0.08    3
    ## GA.at.outcome      406    0    0.0  269.18  26.5    275  267.0  281 103.00  302
    ## Birthweight        406    3    0.7 3180.82 727.5   3260 2972.5 3560 170.00 5160
    ##                     skew  kurt
    ## Age                 0.60  -0.3
    ## BMI                 1.16   2.2
    ## BL.Cig.Day          5.34  35.5
    ## BL.Drks.Day        11.24 130.8
    ## N.prev.preg         1.32   2.3
    ## N.living.kids       1.32   2.3
    ## N.qualifying.teeth  0.53  -0.8
    ## BL.GE               0.24  -0.2
    ## BL..BOP            -0.04  -1.0
    ## BL.PD.avg           1.62   4.4
    ## BL..PD.4            1.19   0.8
    ## BL..PD.5            2.08   4.3
    ## BL.CAL.avg          0.71   1.4
    ## BL..CAL.2           0.30  -1.1
    ## BL..CAL.3           1.68   2.9
    ## BL.Calc.I           0.34  -0.9
    ## BL.Pl.I            -0.10  -0.2
    ## GA.at.outcome      -3.63  15.0
    ## Birthweight        -1.69   4.9
    ## ------------------------------------------------------------ 
    ## Group: T
    ##                      n miss p.miss    mean    sd median    p25  p75    min  max
    ## Age                184    0      0   25.99   5.6     25   22.0   29  16.00   44
    ## BMI                184   21     11   28.04   7.9     27   23.0   30  17.00   68
    ## BL.Cig.Day         184    2      1    0.90   3.4      0    0.0    0   0.00   30
    ## BL.Drks.Day        184    2      1    0.03   0.4      0    0.0    0   0.00    5
    ## N.prev.preg        184    0      0    1.67   1.7      1    0.0    3   0.00   11
    ## N.living.kids      184    0      0    1.67   1.7      1    0.0    3   0.00   11
    ## N.qualifying.teeth 184    0      0   14.41   6.9     13    8.8   19   4.00   28
    ## BL.GE              184    0      0    1.49   0.4      2    1.1    2   0.70    3
    ## BL..BOP            184    0      0   70.08  17.0     69   57.1   85  33.95  100
    ## BL.PD.avg          184    0      0    2.87   0.6      3    2.5    3   1.85    7
    ## BL..PD.4           184    0      0   25.67  15.5     23   14.3   33   3.57   99
    ## BL..PD.5           184    0      0   10.09  14.0      5    1.2   14   0.00   92
    ## BL.CAL.avg         184    0      0    1.50   0.8      2    0.8    2   0.20    5
    ## BL..CAL.2          184    0      0   46.10  27.5     50   19.7   69   2.98  100
    ## BL..CAL.3          184    0      0   16.91  17.8     14    1.8   25   0.00   95
    ## BL.Calc.I          184    0      0    1.17   0.6      1    0.7    2   0.00    3
    ## BL.Pl.I            184    0      0    1.28   0.5      1    1.0    2   0.06    3
    ## GA.at.outcome      184    0      0  272.29  17.5    276  268.0  280 131.00  293
    ## Birthweight        184    0      0 3259.16 574.2   3322 2987.5 3637 186.00 4400
    ##                     skew   kurt
    ## Age                 0.63   0.07
    ## BMI                 2.00   6.35
    ## BL.Cig.Day          5.28  34.41
    ## BL.Drks.Day        12.81 168.26
    ## N.prev.preg         1.73   4.85
    ## N.living.kids       1.73   4.85
    ## N.qualifying.teeth  0.42  -0.91
    ## BL.GE               0.37   0.53
    ## BL..BOP            -0.06  -0.85
    ## BL.PD.avg           2.97  15.56
    ## BL..PD.4            1.52   3.31
    ## BL..PD.5            2.51   8.10
    ## BL.CAL.avg          0.86   3.19
    ## BL..CAL.2          -0.07  -1.22
    ## BL..CAL.3           1.51   2.84
    ## BL.Calc.I           0.37  -0.62
    ## BL.Pl.I            -0.11   0.30
    ## GA.at.outcome      -4.69  31.85
    ## Birthweight        -1.53   6.12
    ## 
    ## Standardize mean differences
    ##                    1 vs 2
    ## Age                0.0150
    ## BMI                0.0737
    ## BL.Cig.Day         0.0245
    ## BL.Drks.Day        0.0129
    ## N.prev.preg        0.1140
    ## N.living.kids      0.1140
    ## N.qualifying.teeth 0.0115
    ## BL.GE              0.1727
    ## BL..BOP            0.0565
    ## BL.PD.avg          0.0637
    ## BL..PD.4           0.0553
    ## BL..PD.5           0.0103
    ## BL.CAL.avg         0.1763
    ## BL..CAL.2          0.1826
    ## BL..CAL.3          0.1696
    ## BL.Calc.I          0.0656
    ## BL.Pl.I            0.1109
    ## GA.at.outcome      0.1387
    ## Birthweight        0.1195
    ## 
    ## =======================================================================================
    ## 
    ##      ### Summary of categorical variables ### 
    ## 
    ## Group: C
    ##                          var   n miss p.miss                level freq percent
    ##                       Clinic 406    0    0.0                   KY  103    25.4
    ##                                                                MN  123    30.3
    ##                                                                MS   96    23.6
    ##                                                                NY   84    20.7
    ##                                                                               
    ##                        Black 406    0    0.0                  No   226    55.7
    ##                                                               Yes  180    44.3
    ##                                                                               
    ##                        White 406    0    0.0                  No   288    70.9
    ##                                                               Yes  118    29.1
    ##                                                                               
    ##                       Nat.Am 406    0    0.0                  No   289    71.2
    ##                                                               Yes  117    28.8
    ##                                                                               
    ##                        Asian 406    0    0.0                  No   402    99.0
    ##                                                               Yes    4     1.0
    ##                                                                               
    ##                         Hisp 406   69   17.0                  No   159    47.2
    ##                                                               Yes  178    52.8
    ##                                                                               
    ##                    Education 406    0    0.0            8-12 yrs   238    58.6
    ##                                                         LT 8 yrs    76    18.7
    ##                                                         MT 12 yrs   92    22.7
    ##                                                                               
    ##                Public.Asstce 406    0    0.0                  No    97    23.9
    ##                                                               Yes  309    76.1
    ##                                                                               
    ##                 Hypertension 406    0    0.0                  N    397    97.8
    ##                                                               Y      9     2.2
    ##                                                                               
    ##                     Diabetes 406    0    0.0                  No   398    98.0
    ##                                                               Yes    8     2.0
    ##                                                                               
    ##                 BL.Diab.Type 406  398   98.0           Type I        1    12.5
    ##                                                        Type II       7    87.5
    ##                                                                               
    ##                      Use.Tob 406    9    2.2                  No   353    88.9
    ##                                                               Yes   44    11.1
    ##                                                                               
    ##                      Use.Alc 406    9    2.2                  No   389    98.0
    ##                                                               Yes    8     2.0
    ##                                                                               
    ##                     Drug.Add 406   10    2.5                  No   396   100.0
    ##                                                                               
    ##                    Prev.preg 406    0    0.0                  No   103    25.4
    ##                                                               Yes  303    74.6
    ##                                                                               
    ##                     Live.PTB 406    0    0.0                  No   363    89.4
    ##                                                               Yes   43    10.6
    ##                                                                               
    ##               Any.stillbirth 406    0    0.0                  No   400    98.5
    ##                                                               Yes    6     1.5
    ##                                                                               
    ##                     Spont.ab 406    0    0.0                  No   312    76.8
    ##                                                               Yes   94    23.2
    ##                                                                               
    ##                   Induced.ab 406    0    0.0                  No   339    83.5
    ##                                                               Yes   67    16.5
    ##                                                                               
    ##  Any.live.ptb.sb.sp.ab.in.ab 406    0    0.0                  No   237    58.4
    ##                                                               Yes  169    41.6
    ##                                                                               
    ##                Birth.outcome 406    0    0.0 Elective abortion       1     0.2
    ##                                              Live birth            391    96.3
    ##                                              Non-live birth         14     3.4
    ##                                                                               
    ##           Preg.ended...37.wk 406    0    0.0                  No   353    86.9
    ##                                                               Yes   53    13.1
    ##                                                                               
    ##                   Preg.ended 406    0    0.0                    0  353    86.9
    ##                                                                 1   53    13.1
    ##                                                                               
    ##  cum.percent
    ##         25.4
    ##         55.7
    ##         79.3
    ##        100.0
    ##             
    ##         55.7
    ##        100.0
    ##             
    ##         70.9
    ##        100.0
    ##             
    ##         71.2
    ##        100.0
    ##             
    ##         99.0
    ##        100.0
    ##             
    ##         47.2
    ##        100.0
    ##             
    ##         58.6
    ##         77.3
    ##        100.0
    ##             
    ##         23.9
    ##        100.0
    ##             
    ##         97.8
    ##        100.0
    ##             
    ##         98.0
    ##        100.0
    ##             
    ##         12.5
    ##        100.0
    ##             
    ##         88.9
    ##        100.0
    ##             
    ##         98.0
    ##        100.0
    ##             
    ##        100.0
    ##             
    ##         25.4
    ##        100.0
    ##             
    ##         89.4
    ##        100.0
    ##             
    ##         98.5
    ##        100.0
    ##             
    ##         76.8
    ##        100.0
    ##             
    ##         83.5
    ##        100.0
    ##             
    ##         58.4
    ##        100.0
    ##             
    ##          0.2
    ##         96.6
    ##        100.0
    ##             
    ##         86.9
    ##        100.0
    ##             
    ##         86.9
    ##        100.0
    ##             
    ## ------------------------------------------------------------ 
    ## Group: T
    ##                          var   n miss p.miss                level freq percent
    ##                       Clinic 184    0    0.0                   KY   45    24.5
    ##                                                                MN   58    31.5
    ##                                                                MS   52    28.3
    ##                                                                NY   29    15.8
    ##                                                                               
    ##                        Black 184    0    0.0                  No   102    55.4
    ##                                                               Yes   82    44.6
    ##                                                                               
    ##                        White 184    0    0.0                  No   157    85.3
    ##                                                               Yes   27    14.7
    ##                                                                               
    ##                       Nat.Am 184    0    0.0                  No   104    56.5
    ##                                                               Yes   80    43.5
    ##                                                                               
    ##                        Asian 184    0    0.0                  No   184   100.0
    ##                                                               Yes    0     0.0
    ##                                                                               
    ##                         Hisp 184    0    0.0                  No    99    53.8
    ##                                                               Yes   85    46.2
    ##                                                                               
    ##                    Education 184    0    0.0            8-12 yrs   107    58.2
    ##                                                         LT 8 yrs    33    17.9
    ##                                                         MT 12 yrs   44    23.9
    ##                                                                               
    ##                Public.Asstce 184    0    0.0                  No    48    26.1
    ##                                                               Yes  136    73.9
    ##                                                                               
    ##                 Hypertension 184    0    0.0                  N    178    96.7
    ##                                                               Y      6     3.3
    ##                                                                               
    ##                     Diabetes 184    0    0.0                  No   175    95.1
    ##                                                               Yes    9     4.9
    ##                                                                               
    ##                 BL.Diab.Type 184  175   95.1           Type I        4    44.4
    ##                                                        Type II       5    55.6
    ##                                                                               
    ##                      Use.Tob 184    2    1.1                  No   165    90.7
    ##                                                               Yes   17     9.3
    ##                                                                               
    ##                      Use.Alc 184    2    1.1                  No   178    97.8
    ##                                                               Yes    4     2.2
    ##                                                                               
    ##                     Drug.Add 184    2    1.1                  No   182   100.0
    ##                                                                               
    ##                    Prev.preg 184    0    0.0                  No    51    27.7
    ##                                                               Yes  133    72.3
    ##                                                                               
    ##                     Live.PTB 184    0    0.0                  No   175    95.1
    ##                                                               Yes    9     4.9
    ##                                                                               
    ##               Any.stillbirth 184    0    0.0                  No   182    98.9
    ##                                                               Yes    2     1.1
    ##                                                                               
    ##                     Spont.ab 184    0    0.0                  No   132    71.7
    ##                                                               Yes   52    28.3
    ##                                                                               
    ##                   Induced.ab 184    0    0.0                  No   176    95.7
    ##                                                               Yes    8     4.3
    ##                                                                               
    ##  Any.live.ptb.sb.sp.ab.in.ab 184    0    0.0                  No   122    66.3
    ##                                                               Yes   62    33.7
    ##                                                                               
    ##                Birth.outcome 184    0    0.0 Elective abortion       1     0.5
    ##                                              Live birth            183    99.5
    ##                                              Non-live birth          0     0.0
    ##                                                                               
    ##           Preg.ended...37.wk 184    0    0.0                  No   166    90.2
    ##                                                               Yes   18     9.8
    ##                                                                               
    ##                   Preg.ended 184    0    0.0                    0  166    90.2
    ##                                                                 1   18     9.8
    ##                                                                               
    ##  cum.percent
    ##         24.5
    ##         56.0
    ##         84.2
    ##        100.0
    ##             
    ##         55.4
    ##        100.0
    ##             
    ##         85.3
    ##        100.0
    ##             
    ##         56.5
    ##        100.0
    ##             
    ##        100.0
    ##        100.0
    ##             
    ##         53.8
    ##        100.0
    ##             
    ##         58.2
    ##         76.1
    ##        100.0
    ##             
    ##         26.1
    ##        100.0
    ##             
    ##         96.7
    ##        100.0
    ##             
    ##         95.1
    ##        100.0
    ##             
    ##         44.4
    ##        100.0
    ##             
    ##         90.7
    ##        100.0
    ##             
    ##         97.8
    ##        100.0
    ##             
    ##        100.0
    ##             
    ##         27.7
    ##        100.0
    ##             
    ##         95.1
    ##        100.0
    ##             
    ##         98.9
    ##        100.0
    ##             
    ##         71.7
    ##        100.0
    ##             
    ##         95.7
    ##        100.0
    ##             
    ##         66.3
    ##        100.0
    ##             
    ##          0.5
    ##        100.0
    ##        100.0
    ##             
    ##         90.2
    ##        100.0
    ##             
    ##         90.2
    ##        100.0
    ##             
    ## 
    ## Standardize mean differences
    ##                              1 vs 2
    ## Clinic                      0.14995
    ## Black                       0.00463
    ## White                       0.35352
    ## Nat.Am                      0.30877
    ## Asian                       0.14107
    ## Hisp                        0.13276
    ## Education                   0.03237
    ## Public.Asstce               0.05072
    ## Hypertension                0.06401
    ## Diabetes                    0.16099
    ## BL.Diab.Type                0.75685
    ## Use.Tob                     0.05757
    ## Use.Alc                     0.01272
    ## Drug.Add                    0.00000
    ## Prev.preg                   0.05319
    ## Live.PTB                    0.21450
    ## Any.stillbirth              0.03475
    ## Spont.ab                    0.11709
    ## Induced.ab                  0.40585
    ## Any.live.ptb.sb.sp.ab.in.ab 0.16421
    ## Birth.outcome               0.27119
    ## Preg.ended...37.wk          0.10300
    ## Preg.ended                  0.10300
