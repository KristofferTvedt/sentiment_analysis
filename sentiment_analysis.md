Sentiment Analysis EU commission AI act
================
Kristoffer Bakke Tvedt
2023-06-02

``` r
library(quanteda)
```

    ## Warning: package 'quanteda' was built under R version 4.2.3

    ## Package version: 3.3.1
    ## Unicode version: 13.0
    ## ICU version: 69.1

    ## Parallel computing: 16 of 16 threads used.

    ## See https://quanteda.io for tutorials and examples.

``` r
library(quanteda.textstats)
```

    ## Warning: package 'quanteda.textstats' was built under R version 4.2.3

``` r
library(quanteda.textplots)
```

    ## Warning: package 'quanteda.textplots' was built under R version 4.2.3

``` r
library(quanteda.textmodels)
```

    ## Warning: package 'quanteda.textmodels' was built under R version 4.2.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(remotes)
```

    ## Warning: package 'remotes' was built under R version 4.2.3

``` r
library(quanteda.sentiment)
```

    ## 
    ## Attaching package: 'quanteda.sentiment'
    ## 
    ## The following object is masked from 'package:quanteda':
    ## 
    ##     data_dictionary_LSD2015

Reads the RDS file created from pdf files and filters out any papers
that are not by Business or NGO

``` r
df <- readRDS("data_corpus_aiact_english_new.rds")
df_business_ngo <- df%>%
  filter(type_actor %in% c("Business", "NGO"))
```

Creates a corpus from df and changes docvar Names to the actor of the
paper. Displays a summary of said corpus.

``` r
original_names <- df_business_ngo$actor
actor_types <- df_business_ngo$type_actor
data_corpus_aiact <- corpus(df_business_ngo)
docnames(data_corpus_aiact) <- paste(original_names, actor_types, sep = " - ")
summary(data_corpus_aiact)
```

    ## Corpus consisting of 129 documents, showing 100 documents:
    ## 
    ##                              Text Types Tokens Sentences       id
    ##                  SAZKA - Business   595   1582        57 F2488672
    ##             Allied-Startups - NGO   285    601        21 F2662175
    ##             Civil-Liberties - NGO  1252   4899       181 F2662292
    ##                 Google - Business  1188   5927       154 F2662492
    ##                        PGEU - NGO   416   1075        27 F2662654
    ##                   SICK - Business   161    296        11 F2662771
    ##                 sanofi - Business   376    736        29 F2662846
    ##     Co-operative-Banks - Business  1098   4721       133 F2662901
    ##                Siemens - Business  1159   4317       104 F2662941
    ##             BlackBerry - Business   333    738        21 F2662944
    ##          Not-for-profit-law - NGO  1584   8604       318 F2663061
    ##                  Eurocities - NGO   815   2747       104 F2663127
    ##               Hangzhou - Business  1142   4568       109 F2663157
    ##         European-Disability - NGO   285    676         6 F2663268
    ##                 EUCOPE - Business   985   2737        90 F2663324
    ##              Eurosmart - Business   997   3134       125 F2663348
    ##                 Orange - Business   595   1567        47 F2663358
    ##                  Bayer - Business   335    659        21 F2663359
    ##  Information-Accountability - NGO  1248   5928       219 F2663380
    ##                 ResMed - Business   321    839        19 F2663398
    ##                 IDEMIA - Business   993   5226       134 F2663405
    ##      Hoffmann-La-Roche - Business  1667   8398       172 F2665165
    ##            Intesa-Bank - Business   800   2785        66 F2665169
    ##           Hospital-Pharmacy - NGO   287    736        20 F2665208
    ##          European-Radiology - NGO   243    481        17 F2665222
    ##                 Biogen - Business   355    679        20 F2665227
    ##                 OpenAI - Business   468   1043        50 F2665231
    ##                Workday - Business  1213   5370       172 F2665233
    ##                        EDRi - NGO  2776  16936       369 F2665234
    ##                    SAP - Business  1026   3929       122 F2665235
    ##        Democracy-Technology - NGO   338    712        28 F2665242
    ##                   Wikimedia - NGO   302    614        26 F2665256
    ##                     5Rights - NGO  4607  21783       642 F2665266
    ##                       Thorn - NGO   258    617        24 F2665284
    ##                    LNE - Business   616   1765        47 F2665323
    ##            AstraZeneca - Business   666   1868        76 F2665331
    ##              AlgorithmWatch - NGO  1611   6895       194 F2665406
    ##                  JEITA - Business  1398   5924       135 F2665415
    ##                    NEC - Business   929   4521       116 F2665416
    ##               Lewiatan - Business   976   3388       115 F2665421
    ##                  Glovo - Business   566   1475        53 F2665424
    ##                        BEUC - NGO   964   3490        93 F2665432
    ##                 SPLUNK - Business   726   2210        73 F2665433
    ##                   ThinkTech - NGO  2565  12365       575 F2665437
    ##                  MERCK - Business   498   1044        46 F2665438
    ##                   BBVA - Business  1003   5186       128 F2665439
    ##                   SEMI - Business  1072   3895       127 F2665441
    ##                 Huawei - Business  1036   4004       119 F2665442
    ##         Liberty-Global - Business   742   2161        62 F2665443
    ##               Mediaset - Business  1226   4634       104 F2665444
    ##       Renaissance-Numerique - NGO  1975   9547       273 F2665447
    ##                         ACM - NGO   933   2482        67 F2665448
    ##                    BMW - Business   386    862        41 F2665452
    ##             Bits-of-Freedom - NGO   413   1066        37 F2665458
    ##                  Access-Now - NGO  2192  15688       364 F2665462
    ##               Novartis - Business  1782   7261       201 F2665464
    ##           Arthur-Legal - Business  1296   5275       192 F2665467
    ##                    EON - Business   470   1235        43 F2665468
    ##                KMD-NEC - Business   544   1443        51 F2665469
    ##             SellaGroup - Business   928   3916       114 F2665471
    ##               DeepMind - Business  1116   4327       130 F2665473
    ##                   RELX - Business  1168   4294       149 F2665475
    ##                      MoveEU - NGO   333    681        21 F2665477
    ##      Standard-Chartred - Business   513   1389        47 F2665478
    ##                    CIO - Business  1076   4081       147 F2665479
    ##                Fujitsu - Business  1287   5163       148 F2665483
    ##         Deutsche-Börse - Business   366    887        30 F2665486
    ##        Johnson-Johnson - Business   584   1648        49 F2665490
    ##                  Intel - Business   284    750        21 F2665494
    ##                 Twilio - Business   865   2783       104 F2665505
    ##            German-Bank - Business  1982   9644       351 F2665508
    ##           European-Dentists - NGO   359    714        22 F2665510
    ##                        AIAL - NGO   386    863        34 F2665514
    ##             Volkswagen - Business   729   2239        89 F2665515
    ##                   Enel - Business   650   1920        39 F2665516
    ##                   Zvei - Business  1220   5596       165 F2665517
    ##                        CPME - NGO   578   1645        62 F2665519
    ##                Equifax - Business   955   3842       119 F2665520
    ##      Center-Data-Innovation - NGO   746   2084        69 F2665521
    ##             Mastercard - Business   953   3374        91 F2665525
    ##                    Sky - Business   470   1165        43 F2665526
    ##               Moje-Panvisto - NGO   479   1485        42 F2665527
    ##                         EPF - NGO  1883  11324       301 F2665528
    ##                     Medtech - NGO   589   1911        50 F2665532
    ##                    BSA - Business  1025   3997        94 F2665533
    ##            Medical-Oncology - NGO   300    720        20 F2665541
    ##                 Onfido - Business   470   1108        35 F2665542
    ##         Siemens-Energy - Business   481   1258        36 F2665543
    ##               Future-ofLife - NGO  1227   4124       144 F2665546
    ##          Hogan-Lovells - Business   705   2050        54 F2665548
    ##             Edtech-Alliance - NGO   417    968        19 F2665550
    ##             Trilateral - Business   617   1757        57 F2665554
    ##              Microsoft - Business  1614   8165       221 F2665556
    ##                  Nokia - Business  1186   5570       131 F2665565
    ##                    BVI - Business   601   1571        62 F2665568
    ##              Medtronic - Business   920   4513       122 F2665573
    ##                 Open-Future - NGO   521   1338        39 F2665576
    ##                 Women-in-AI - NGO  1251   5238       127 F2665578
    ##          Evangelig-Alliance - NGO   873   2684        89 F2665580
    ##               Infineon - Business   316    780        12 F2665583
    ##                       actor type_actor
    ##                       SAZKA   Business
    ##             Allied-Startups        NGO
    ##             Civil-Liberties        NGO
    ##                      Google   Business
    ##                        PGEU        NGO
    ##                        SICK   Business
    ##                      sanofi   Business
    ##          Co-operative-Banks   Business
    ##                     Siemens   Business
    ##                  BlackBerry   Business
    ##          Not-for-profit-law        NGO
    ##                  Eurocities        NGO
    ##                    Hangzhou   Business
    ##         European-Disability        NGO
    ##                      EUCOPE   Business
    ##                   Eurosmart   Business
    ##                      Orange   Business
    ##                       Bayer   Business
    ##  Information-Accountability        NGO
    ##                      ResMed   Business
    ##                      IDEMIA   Business
    ##           Hoffmann-La-Roche   Business
    ##                 Intesa-Bank   Business
    ##           Hospital-Pharmacy        NGO
    ##          European-Radiology        NGO
    ##                      Biogen   Business
    ##                      OpenAI   Business
    ##                     Workday   Business
    ##                        EDRi        NGO
    ##                         SAP   Business
    ##        Democracy-Technology        NGO
    ##                   Wikimedia        NGO
    ##                     5Rights        NGO
    ##                       Thorn        NGO
    ##                         LNE   Business
    ##                 AstraZeneca   Business
    ##              AlgorithmWatch        NGO
    ##                       JEITA   Business
    ##                         NEC   Business
    ##                    Lewiatan   Business
    ##                       Glovo   Business
    ##                        BEUC        NGO
    ##                      SPLUNK   Business
    ##                   ThinkTech        NGO
    ##                       MERCK   Business
    ##                        BBVA   Business
    ##                        SEMI   Business
    ##                      Huawei   Business
    ##              Liberty-Global   Business
    ##                    Mediaset   Business
    ##       Renaissance-Numerique        NGO
    ##                         ACM        NGO
    ##                         BMW   Business
    ##             Bits-of-Freedom        NGO
    ##                  Access-Now        NGO
    ##                    Novartis   Business
    ##                Arthur-Legal   Business
    ##                         EON   Business
    ##                     KMD-NEC   Business
    ##                  SellaGroup   Business
    ##                    DeepMind   Business
    ##                        RELX   Business
    ##                      MoveEU        NGO
    ##           Standard-Chartred   Business
    ##                         CIO   Business
    ##                     Fujitsu   Business
    ##              Deutsche-Börse   Business
    ##             Johnson-Johnson   Business
    ##                       Intel   Business
    ##                      Twilio   Business
    ##                 German-Bank   Business
    ##           European-Dentists        NGO
    ##                        AIAL        NGO
    ##                  Volkswagen   Business
    ##                        Enel   Business
    ##                        Zvei   Business
    ##                        CPME        NGO
    ##                     Equifax   Business
    ##      Center-Data-Innovation        NGO
    ##                  Mastercard   Business
    ##                         Sky   Business
    ##               Moje-Panvisto        NGO
    ##                         EPF        NGO
    ##                     Medtech        NGO
    ##                         BSA   Business
    ##            Medical-Oncology        NGO
    ##                      Onfido   Business
    ##              Siemens-Energy   Business
    ##               Future-ofLife        NGO
    ##               Hogan-Lovells   Business
    ##             Edtech-Alliance        NGO
    ##                  Trilateral   Business
    ##                   Microsoft   Business
    ##                       Nokia   Business
    ##                         BVI   Business
    ##                   Medtronic   Business
    ##                 Open-Future        NGO
    ##                 Women-in-AI        NGO
    ##          Evangelig-Alliance        NGO
    ##                    Infineon   Business

Creates a atoken object from corpus

``` r
toks_aiact <- tokens(data_corpus_aiact)
```

Creates a dfm from token object

``` r
toks_aiact2 <- tokens(data_corpus_aiact, remove_punct = T, remove_numbers = T) %>%
tokens_remove(stopwords("en")) %>%
dfm()
toks_aiact2
```

    ## Document-feature matrix of: 129 documents, 15,350 features (95.06% sparse) and 3 docvars.
    ##                        features
    ## docs                    sazka group submission ec public consultation
    ##   SAZKA - Business          7     7          1  4      4            1
    ##   Allied-Startups - NGO     0     0          0  0      0            0
    ##   Civil-Liberties - NGO     0     4          0  0     28            1
    ##   Google - Business         0     0          1  0      2            2
    ##   PGEU - NGO                0     1          0  0      1            1
    ##   SICK - Business           0     0          0  0      0            0
    ##                        features
    ## docs                    artificial intelligence ethical legal
    ##   SAZKA - Business               3            4       1     6
    ##   Allied-Startups - NGO          7            7       1     3
    ##   Civil-Liberties - NGO          4            4       0     0
    ##   Google - Business              1            2       0    11
    ##   PGEU - NGO                     4            4       0     1
    ##   SICK - Business                2            2       1     1
    ## [ reached max_ndoc ... 123 more documents, reached max_nfeat ... 15,340 more features ]

Shows most frequent terms by document

``` r
textstat_frequency(toks_aiact2, n = 10, groups = actor)
```

    ##               feature frequency rank docfreq                          group
    ## 1            children       175    1       1                        5Rights
    ## 2             digital       108    2       1                        5Rights
    ## 3              social       103    3       1                        5Rights
    ## 4                 use        81    4       1                        5Rights
    ## 5               media        79    5       1                        5Rights
    ## 6              people        70    6       1                        5Rights
    ## 7               young        70    6       1                        5Rights
    ## 8              design        69    8       1                        5Rights
    ## 9              online        67    9       1                        5Rights
    ## 10         persuasive        65   10       1                        5Rights
    ## 11                 ai        54    1       1                            ABB
    ## 12             system        30    2       1                            ABB
    ## 13            systems        24    3       1                            ABB
    ## 14             safety        20    4       1                            ABB
    ## 15            article        20    4       1                            ABB
    ## 16               data        17    6       1                            ABB
    ## 17              shall        17    6       1                            ABB
    ## 18            methods        15    8       1                            ABB
    ## 19          machinery        15    8       1                            ABB
    ## 20         regulation        14   10       1                            ABB
    ## 21                 ai       201    1       1                     Access-Now
    ## 22            systems       118    2       1                     Access-Now
    ## 23               data        91    3       1                     Access-Now
    ## 24            article        83    4       1                     Access-Now
    ## 25             rights        80    5       1                     Access-Now
    ## 26           proposal        73    6       1                     Access-Now
    ## 27             system        67    7       1                     Access-Now
    ## 28             access        61    8       1                     Access-Now
    ## 29                use        60    9       1                     Access-Now
    ## 30           european        58   10       1                     Access-Now
    ## 31             europe        30    1       1                            ACM
    ## 32                 ai        27    2       1                            ACM
    ## 33               data        22    3       1                            ACM
    ## 34            systems        19    4       1                            ACM
    ## 35                tpc        19    4       1                            ACM
    ## 36            article        16    6       1                            ACM
    ## 37               also        15    7       1                            ACM
    ## 38           proposal        13    8       1                            ACM
    ## 39                may        12    9       1                            ACM
    ## 40         regulation        11   10       1                            ACM
    ## 41                 ai        14    1       1                     AI-Austria
    ## 42         artificial        13    2       1                     AI-Austria
    ## 43       intelligence        13    2       1                     AI-Austria
    ## 44                act        11    4       1                     AI-Austria
    ## 45               data        11    4       1                     AI-Austria
    ## 46            systems         8    6       1                     AI-Austria
    ## 47           european         5    7       1                     AI-Austria
    ## 48               risk         5    7       1                     AI-Austria
    ## 49            believe         5    7       1                     AI-Austria
    ## 50            support         4   10       1                     AI-Austria
    ## 51                 ai         9    1       1                           AIAL
    ## 52            systems         8    2       1                           AIAL
    ## 53          biometric         6    3       1                           AIAL
    ## 54                  ●         6    3       1                           AIAL
    ## 55             remote         5    5       1                           AIAL
    ## 56                can         5    5       1                           AIAL
    ## 57         technology         5    5       1                           AIAL
    ## 58         guidelines         5    5       1                           AIAL
    ## 59       requirements         4    9       1                           AIAL
    ## 60        application         4    9       1                           AIAL
    ## 61            systems       105    1       1                 AlgorithmWatch
    ## 62                 ai        47    2       1                 AlgorithmWatch
    ## 63                act        38    3       1                 AlgorithmWatch
    ## 64                use        34    4       1                 AlgorithmWatch
    ## 65             public        31    5       1                 AlgorithmWatch
    ## 66             rights        28    6       1                 AlgorithmWatch
    ## 67        fundamental        26    7       1                 AlgorithmWatch
    ## 68               must        22    8       1                 AlgorithmWatch
    ## 69       requirements        19    9       1                 AlgorithmWatch
    ## 70               used        19    9       1                 AlgorithmWatch
    ## 71                 ai       219    1       1                          ALLAI
    ## 72                aia        64    2       1                          ALLAI
    ## 73            systems        55    3       1                          ALLAI
    ## 74          high-risk        50    4       1                          ALLAI
    ## 75              human        45    5       1                          ALLAI
    ## 76             rights        45    5       1                          ALLAI
    ## 77                law        37    7       1                          ALLAI
    ## 78        recognition        36    8       1                          ALLAI
    ## 79             social        36    8       1                          ALLAI
    ## 80               data        35   10       1                          ALLAI
    ## 81                 ai        13    1       1                Allied-Startups
    ## 82           startups         9    2       1                Allied-Startups
    ## 83         artificial         7    3       1                Allied-Startups
    ## 84       intelligence         7    3       1                Allied-Startups
    ## 85          high-risk         5    5       1                Allied-Startups
    ## 86      entrepreneurs         5    5       1                Allied-Startups
    ## 87       requirements         4    7       1                Allied-Startups
    ## 88                act         4    7       1                Allied-Startups
    ## 89               risk         4    7       1                Allied-Startups
    ## 90               high         4    7       1                Allied-Startups
    ## 91                 ai        13    1       1 American-Insurance-Association
    ## 92           proposed        11    2       1 American-Insurance-Association
    ## 93         regulation        10    3       1 American-Insurance-Association
    ## 94              apcia         9    4       1 American-Insurance-Association
    ## 95         commission         6    5       1 American-Insurance-Association
    ## 96          insurance         6    5       1 American-Insurance-Association
    ## 97               risk         5    7       1 American-Insurance-Association
    ## 98         regulatory         4    8       1 American-Insurance-Association
    ## 99           property         4    8       1 American-Insurance-Association
    ## 100         high-risk         4    8       1 American-Insurance-Association
    ## 101                ai        84    1       1                        Amnesty
    ## 102           systems        72    2       1                        Amnesty
    ## 103            rights        69    3       1                        Amnesty
    ## 104             human        67    4       1                        Amnesty
    ## 105               aia        34    5       1                        Amnesty
    ## 106         biometric        33    6       1                        Amnesty
    ## 107       recognition        26    7       1                        Amnesty
    ## 108               use        24    8       1                        Amnesty
    ## 109     international        24    8       1                        Amnesty
    ## 110              data        20   10       1                        Amnesty
    ## 111                ai        71    1       1                   Arthur-Legal
    ## 112           systems        42    2       1                   Arthur-Legal
    ## 113               can        30    3       1                   Arthur-Legal
    ## 114               act        28    4       1                   Arthur-Legal
    ## 115           purpose        28    4       1                   Arthur-Legal
    ## 116              risk        27    6       1                   Arthur-Legal
    ## 117               use        23    7       1                   Arthur-Legal
    ## 118             legal        22    8       1                   Arthur-Legal
    ## 119              also        20    9       1                   Arthur-Legal
    ## 120          proposed        19   10       1                   Arthur-Legal
    ## 121                ai        68    1       1                    AstraZeneca
    ## 122           systems        32    2       1                    AstraZeneca
    ## 123              data        17    3       1                    AstraZeneca
    ## 124               use        14    4       1                    AstraZeneca
    ## 125               act        12    5       1                    AstraZeneca
    ## 126       astrazeneca        12    5       1                    AstraZeneca
    ## 127        artificial        11    7       1                    AstraZeneca
    ## 128      intelligence        11    7       1                    AstraZeneca
    ## 129           medical         7    9       1                    AstraZeneca
    ## 130       development         6   10       1                    AstraZeneca
    ## 131                ai       248    1       1                          Avaaz
    ## 132           systems       156    2       1                          Avaaz
    ## 133               use        93    3       1                          Avaaz
    ## 134            rights        85    4       1                          Avaaz
    ## 135           article        81    5       1                          Avaaz
    ## 136            system        71    6       1                          Avaaz
    ## 137             shall        61    7       1                          Avaaz
    ## 138              risk        55    8       1                          Avaaz
    ## 139             human        47    9       1                          Avaaz
    ## 140       fundamental        45   10       1                          Avaaz
    ## 141                ai        21    1       1                          Bayer
    ## 142              data         9    2       1                          Bayer
    ## 143        regulation         6    3       1                          Bayer
    ## 144             bayer         5    4       1                          Bayer
    ## 145             human         4    5       1                          Bayer
    ## 146        regulatory         4    5       1                          Bayer
    ## 147        definition         4    5       1                          Bayer
    ## 148      requirements         3    8       1                          Bayer
    ## 149          proposal         3    8       1                          Bayer
    ## 150               act         3    8       1                          Bayer
    ## 151                ai        72    1       1                           BBVA
    ## 152            system        40    2       1                           BBVA
    ## 153           systems        38    3       1                           BBVA
    ## 154           article        38    3       1                           BBVA
    ## 155        regulation        37    5       1                           BBVA
    ## 156         high-risk        37    5       1                           BBVA
    ## 157             shall        32    7       1                           BBVA
    ## 158       authorities        25    8       1                           BBVA
    ## 159              data        22    9       1                           BBVA
    ## 160              bias        20   10       1                           BBVA
    ## 161         financial        29    1       1                 Better-Finance
    ## 162                ai        23    2       1                 Better-Finance
    ## 163          services        17    3       1                 Better-Finance
    ## 164        artificial        16    4       1                 Better-Finance
    ## 165      intelligence        16    4       1                 Better-Finance
    ## 166         platforms        14    6       1                 Better-Finance
    ## 167               use        13    7       1                 Better-Finance
    ## 168            advice        12    8       1                 Better-Finance
    ## 169        investment        12    8       1                 Better-Finance
    ## 170           provide        11   10       1                 Better-Finance
    ## 171                ai        69    1       1                           BEUC
    ## 172          proposal        34    2       1                           BEUC
    ## 173         consumers        26    3       1                           BEUC
    ## 174               use        18    4       1                           BEUC
    ## 175            rights        17    5       1                           BEUC
    ## 176               see        16    6       1                           BEUC
    ## 177         high-risk        14    7       1                           BEUC
    ## 178           article        14    7       1                           BEUC
    ## 179              data        13    9       1                           BEUC
    ## 180           systems        13    9       1                           BEUC
    ## 181                ai        16    1       1                         Biogen
    ## 182           support         5    2       1                         Biogen
    ## 183              data         5    2       1                         Biogen
    ## 184              risk         5    2       1                         Biogen
    ## 185              high         4    5       1                         Biogen
    ## 186            access         4    5       1                         Biogen
    ## 187             rules         4    5       1                         Biogen
    ## 188            biogen         4    5       1                         Biogen
    ## 189         providers         3    9       1                         Biogen
    ## 190            system         3    9       1                         Biogen
    ## 191           systems        12    1       1                Bits-of-Freedom
    ## 192                ai        11    2       1                Bits-of-Freedom
    ## 193       fundamental        10    3       1                Bits-of-Freedom
    ## 194            rights        10    3       1                Bits-of-Freedom
    ## 195        commission         9    5       1                Bits-of-Freedom
    ## 196        protection         7    6       1                Bits-of-Freedom
    ## 197           article         7    6       1                Bits-of-Freedom
    ## 198         practices         7    6       1                Bits-of-Freedom
    ## 199       prohibition         7    6       1                Bits-of-Freedom
    ## 200           freedom         7    6       1                Bits-of-Freedom
    ## 201                ai        18    1       1                     BlackBerry
    ## 202         standards         6    2       1                     BlackBerry
    ## 203      requirements         5    3       1                     BlackBerry
    ## 204           support         5    3       1                     BlackBerry
    ## 205           systems         5    3       1                     BlackBerry
    ## 206     cybersecurity         5    3       1                     BlackBerry
    ## 207       development         4    7       1                     BlackBerry
    ## 208        compliance         4    7       1                     BlackBerry
    ## 209        conformity         4    7       1                     BlackBerry
    ## 210           article         4    7       1                     BlackBerry
    ## 211               bmw        30    1       1                            BMW
    ## 212             group        24    2       1                            BMW
    ## 213                ai        21    3       1                            BMW
    ## 214      applications         8    4       1                            BMW
    ## 215           company         7    5       1                            BMW
    ## 216        artificial         6    6       1                            BMW
    ## 217      intelligence         6    6       1                            BMW
    ## 218      technologies         6    6       1                            BMW
    ## 219              data         6    6       1                            BMW
    ## 220         corporate         5   10       1                            BMW
    ## 221                ai       132    1       1                            BSA
    ## 222               bsa        34    2       1                            BSA
    ## 223               act        30    3       1                            BSA
    ## 224            system        26    4       1                            BSA
    ## 225       obligations        20    5       1                            BSA
    ## 226         high-risk        19    6       1                            BSA
    ## 227          proposal        18    7       1                            BSA
    ## 228               may        17    8       1                            BSA
    ## 229      requirements        16    9       1                            BSA
    ## 230        recommends        15   10       1                            BSA
    ## 231              data        31    1       1                            BVI
    ## 232                ai        17    2       1                            BVI
    ## 233            market        12    3       1                            BVI
    ## 234        regulation        10    4       1                            BVI
    ## 235             asset        10    4       1                            BVI
    ## 236          business         8    6       1                            BVI
    ## 237               big         8    6       1                            BVI
    ## 238      applications         7    8       1                            BVI
    ## 239               can         7    8       1                            BVI
    ## 240         financial         7    8       1                            BVI
    ## 241                ai        42    1       1         Center-Data-Innovation
    ## 242               new        16    2       1         Center-Data-Innovation
    ## 243        regulatory        14    3       1         Center-Data-Innovation
    ## 244               aia        13    4       1         Center-Data-Innovation
    ## 245        businesses        11    5       1         Center-Data-Innovation
    ## 246          european        10    6       1         Center-Data-Innovation
    ## 247           sandbox        10    6       1         Center-Data-Innovation
    ## 248             tools        10    6       1         Center-Data-Innovation
    ## 249      requirements         9    9       1         Center-Data-Innovation
    ## 250          approach         9    9       1         Center-Data-Innovation
    ## 251                ai        64    1       1           Chamber-Commerce-USA
    ## 252               act        27    2       1           Chamber-Commerce-USA
    ## 253              data        24    3       1           Chamber-Commerce-USA
    ## 254          european        15    4       1           Chamber-Commerce-USA
    ## 255      applications        14    5       1           Chamber-Commerce-USA
    ## 256         high-risk        14    5       1           Chamber-Commerce-USA
    ## 257           chamber        14    5       1           Chamber-Commerce-USA
    ## 258      requirements        13    8       1           Chamber-Commerce-USA
    ## 259                eu        12    9       1           Chamber-Commerce-USA
    ## 260           systems        12    9       1           Chamber-Commerce-USA
    ## 261                ai       102    1       1                            CIO
    ## 262             users        36    2       1                            CIO
    ## 263          business        35    3       1                            CIO
    ## 264           systems        32    4       1                            CIO
    ## 265               act        25    5       1                            CIO
    ## 266         providers        19    6       1                            CIO
    ## 267            system        19    6       1                            CIO
    ## 268          european        16    8       1                            CIO
    ## 269          provider        15    9       1                            CIO
    ## 270      technologies        14   10       1                            CIO
    ## 271           systems        98    1       1                Civil-Liberties
    ## 272                ai        59    2       1                Civil-Liberties
    ## 273          proposal        37    3       1                Civil-Liberties
    ## 274              used        33    4       1                Civil-Liberties
    ## 275            public        28    5       1                Civil-Liberties
    ## 276       authorities        27    6       1                Civil-Liberties
    ## 277         liberties        27    6       1                Civil-Liberties
    ## 278         biometric        24    8       1                Civil-Liberties
    ## 279      commission’s        23    9       1                Civil-Liberties
    ## 280               can        20   10       1                Civil-Liberties
    ## 281                ai       515    1       1             CLAIRE-AI-Research
    ## 282          european       198    2       1             CLAIRE-AI-Research
    ## 283        regulation       157    3       1             CLAIRE-AI-Research
    ## 284              plan       134    4       1             CLAIRE-AI-Research
    ## 285       coordinated       116    5       1             CLAIRE-AI-Research
    ## 286            claire        96    6       1             CLAIRE-AI-Research
    ## 287          research        94    7       1             CLAIRE-AI-Research
    ## 288            europe        85    8       1             CLAIRE-AI-Research
    ## 289          proposed        81    9       1             CLAIRE-AI-Research
    ## 290          response        61   10       1             CLAIRE-AI-Research
    ## 291                ai        59    1       1              Climate-Change-AI
    ## 292           climate        46    2       1              Climate-Change-AI
    ## 293            change        44    3       1              Climate-Change-AI
    ## 294           systems        22    4       1              Climate-Change-AI
    ## 295           impacts        15    5       1              Climate-Change-AI
    ## 296               can        14    6       1              Climate-Change-AI
    ## 297               gas        13    7       1              Climate-Change-AI
    ## 298        greenhouse        13    7       1              Climate-Change-AI
    ## 299         high-risk        11    9       1              Climate-Change-AI
    ## 300         emissions        11    9       1              Climate-Change-AI
    ## 301                ai        88    1       1             Co-operative-Banks
    ## 302           systems        37    2       1             Co-operative-Banks
    ## 303               art        36    3       1             Co-operative-Banks
    ## 304            system        35    4       1             Co-operative-Banks
    ## 305          european        28    5       1             Co-operative-Banks
    ## 306      co-operative        26    6       1             Co-operative-Banks
    ## 307        definition        24    7       1             Co-operative-Banks
    ## 308        regulation        23    8       1             Co-operative-Banks
    ## 309             banks        22    9       1             Co-operative-Banks
    ## 310        regulatory        19   10       1             Co-operative-Banks
    ## 311                ai        41    1       1                           CPME
    ## 312            system        18    2       1                           CPME
    ## 313              cpme        17    3       1                           CPME
    ## 314           article        13    4       1                           CPME
    ## 315              data        11    5       1                           CPME
    ## 316           systems        11    5       1                           CPME
    ## 317           medical        11    5       1                           CPME
    ## 318          european        10    8       1                           CPME
    ## 319        healthcare        10    8       1                           CPME
    ## 320         oversight         8   10       1                           CPME
    ## 321               aia         8    1       1                    Croation-AI
    ## 322                ai         6    2       1                    Croation-AI
    ## 323        innovation         5    3       1                    Croation-AI
    ## 324          startups         4    4       1                    Croation-AI
    ## 325             croai         4    4       1                    Croation-AI
    ## 326          european         3    6       1                    Croation-AI
    ## 327         sandboxes         3    6       1                    Croation-AI
    ## 328           conduct         3    6       1                    Croation-AI
    ## 329              code         3    6       1                    Croation-AI
    ## 330         complying         3    6       1                    Croation-AI
    ## 331       crowdstrike        21    1       1                    Croudstrike
    ## 332                ai        17    2       1                    Croudstrike
    ## 333     cybersecurity        15    3       1                    Croudstrike
    ## 334              data         9    4       1                    Croudstrike
    ## 335        protection         7    5       1                    Croudstrike
    ## 336            policy         6    6       1                    Croudstrike
    ## 337         solutions         6    6       1                    Croudstrike
    ## 338           threats         6    6       1                    Croudstrike
    ## 339                ml         6    6       1                    Croudstrike
    ## 340            falcon         6    6       1                    Croudstrike
    ## 341                ai       126    1       1                       DeepMind
    ## 342           systems        31    2       1                       DeepMind
    ## 343          research        25    3       1                       DeepMind
    ## 344               aia        25    3       1                       DeepMind
    ## 345            system        18    5       1                       DeepMind
    ## 346               may        17    6       1                       DeepMind
    ## 347              also        16    7       1                       DeepMind
    ## 348       development        14    8       1                       DeepMind
    ## 349               way        13    9       1                       DeepMind
    ## 350              risk        13    9       1                       DeepMind
    ## 351            rights        13    1       1           Democracy-Technology
    ## 352                ai        10    2       1           Democracy-Technology
    ## 353             human         8    3       1           Democracy-Technology
    ## 354              risk         7    4       1           Democracy-Technology
    ## 355             draft         6    5       1           Democracy-Technology
    ## 356               act         5    6       1           Democracy-Technology
    ## 357       enforcement         5    6       1           Democracy-Technology
    ## 358            public         4    8       1           Democracy-Technology
    ## 359          proposal         4    8       1           Democracy-Technology
    ## 360             risks         4    8       1           Democracy-Technology
    ## 361                ai        15    1       1                 Deutsche-Börse
    ## 362              data        14    2       1                 Deutsche-Börse
    ## 363         financial        11    3       1                 Deutsche-Börse
    ## 364      applications         8    4       1                 Deutsche-Börse
    ## 365               use         6    5       1                 Deutsche-Börse
    ## 366            market         5    6       1                 Deutsche-Börse
    ## 367      technologies         5    6       1                 Deutsche-Börse
    ## 368               can         5    6       1                 Deutsche-Börse
    ## 369             rules         5    6       1                 Deutsche-Börse
    ## 370         high-risk         5    6       1                 Deutsche-Börse
    ## 371                ai       222    1       1                 Digitalcourage
    ## 372           systems       128    2       1                 Digitalcourage
    ## 373            rights       113    3       1                 Digitalcourage
    ## 374       fundamental        90    4       1                 Digitalcourage
    ## 375         biometric        85    5       1                 Digitalcourage
    ## 376           article        79    6       1                 Digitalcourage
    ## 377               use        74    7       1                 Digitalcourage
    ## 378              risk        68    8       1                 Digitalcourage
    ## 379               aia        68    8       1                 Digitalcourage
    ## 380              data        66   10       1                 Digitalcourage
    ## 381                ai       222    1       1                           EDRi
    ## 382           systems       128    2       1                           EDRi
    ## 383            rights       113    3       1                           EDRi
    ## 384       fundamental        90    4       1                           EDRi
    ## 385         biometric        85    5       1                           EDRi
    ## 386           article        79    6       1                           EDRi
    ## 387               use        74    7       1                           EDRi
    ## 388              risk        68    8       1                           EDRi
    ## 389               aia        68    8       1                           EDRi
    ## 390              data        66   10       1                           EDRi
    ## 391            edtech        16    1       1                Edtech-Alliance
    ## 392          european        13    2       1                Edtech-Alliance
    ## 393         education        12    3       1                Edtech-Alliance
    ## 394                ai         9    4       1                Edtech-Alliance
    ## 395         companies         6    5       1                Edtech-Alliance
    ## 396              many         6    5       1                Edtech-Alliance
    ## 397        technology         6    5       1                Edtech-Alliance
    ## 398          industry         5    8       1                Edtech-Alliance
    ## 399        innovation         5    8       1                Edtech-Alliance
    ## 400        commission         5    8       1                Edtech-Alliance
    ## 401                ai        38    1       1       Employment-Confederation
    ## 402        wec-europe        23    2       1       Employment-Confederation
    ## 403        employment        22    3       1       Employment-Confederation
    ## 404            market        21    4       1       Employment-Confederation
    ## 405            labour        17    5       1       Employment-Confederation
    ## 406          services        16    6       1       Employment-Confederation
    ## 407       recruitment        13    7       1       Employment-Confederation
    ## 408           private        12    8       1       Employment-Confederation
    ## 409          proposal        11    9       1       Employment-Confederation
    ## 410      applications        11    9       1       Employment-Confederation
    ## 411                ai        29    1       1                           Enel
    ## 412           systems        17    2       1                           Enel
    ## 413              enel        14    3       1                           Enel
    ## 414        regulation        13    4       1                           Enel
    ## 415         high-risk        11    5       1                           Enel
    ## 416           article        11    5       1                           Enel
    ## 417        definition        10    7       1                           Enel
    ## 418          critical        10    7       1                           Enel
    ## 419                eu         9    9       1                           Enel
    ## 420     certification         9    9       1                           Enel
    ## 421                ai        39    1       1                            EON
    ## 422               act        10    2       1                            EON
    ## 423              e.on         8    3       1                            EON
    ## 424        definition         7    4       1                            EON
    ## 425      technologies         6    5       1                            EON
    ## 426               use         6    5       1                            EON
    ## 427           systems         6    5       1                            EON
    ## 428               art         6    5       1                            EON
    ## 429              risk         5    9       1                            EON
    ## 430        particular         5    9       1                            EON
    ## 431              data       200    1       1                            EPF
    ## 432            health       167    2       1                            EPF
    ## 433                ai       131    3       1                            EPF
    ## 434          patients       121    4       1                            EPF
    ## 435        healthcare        89    5       1                            EPF
    ## 436          european        59    6       1                            EPF
    ## 437           digital        57    7       1                            EPF
    ## 438              also        57    7       1                            EPF
    ## 439            access        56    9       1                            EPF
    ## 440               epf        53   10       1                            EPF
    ## 441                ai        55    1       1                        Equifax
    ## 442            credit        53    2       1                        Equifax
    ## 443           systems        49    3       1                        Equifax
    ## 444  creditworthiness        39    4       1                        Equifax
    ## 445              risk        27    5       1                        Equifax
    ## 446         financial        21    6       1                        Equifax
    ## 447              high        20    7       1                        Equifax
    ## 448        regulation        19    8       1                        Equifax
    ## 449          services        19    8       1                        Equifax
    ## 450                 ●        18   10       1                        Equifax
    ## 451                ai        69    1       1                        Equinet
    ## 452          equality        60    2       1                        Equinet
    ## 453            rights        40    3       1                        Equinet
    ## 454        regulation        30    4       1                        Equinet
    ## 455          national        29    5       1                        Equinet
    ## 456           systems        28    6       1                        Equinet
    ## 457       fundamental        23    7       1                        Equinet
    ## 458            bodies        23    7       1                        Equinet
    ## 459                eu        20    9       1                        Equinet
    ## 460             human        16   10       1                        Equinet
    ## 461              data        56    1       1                         EUCOPE
    ## 462                ai        29    2       1                         EUCOPE
    ## 463         solutions        18    3       1                         EUCOPE
    ## 464               can        17    4       1                         EUCOPE
    ## 465        healthcare        17    4       1                         EUCOPE
    ## 466            health        15    6       1                         EUCOPE
    ## 467          european        14    7       1                         EUCOPE
    ## 468     reimbursement        13    8       1                         EUCOPE
    ## 469        regulatory        12    9       1                         EUCOPE
    ## 470           quality        12    9       1                         EUCOPE
    ## 471                ai        73    1       1                     Eurocities
    ## 472              data        39    2       1                     Eurocities
    ## 473            cities        32    3       1                     Eurocities
    ## 474             local        24    4       1                     Eurocities
    ## 475           digital        22    5       1                     Eurocities
    ## 476       governments        20    6       1                     Eurocities
    ## 477            public        17    7       1                     Eurocities
    ## 478            skills        17    7       1                     Eurocities
    ## 479               use        16    9       1                     Eurocities
    ## 480       development        14   10       1                     Eurocities
    ## 481                ai        23    1       1              European-AI-Forum
    ## 482              data        13    2       1              European-AI-Forum
    ## 483               act         7    3       1              European-AI-Forum
    ## 484          european         7    3       1              European-AI-Forum
    ## 485        definition         7    3       1              European-AI-Forum
    ## 486         sandboxes         7    3       1              European-AI-Forum
    ## 487        artificial         6    7       1              European-AI-Forum
    ## 488      intelligence         6    7       1              European-AI-Forum
    ## 489           believe         6    7       1              European-AI-Forum
    ## 490        regulation         6    7       1              European-AI-Forum
    ## 491                ai        21    1       1              European-Dentists
    ## 492        healthcare        10    2       1              European-Dentists
    ## 493            ensure         6    3       1              European-Dentists
    ## 494        regulation         6    3       1              European-Dentists
    ## 495       application         5    5       1              European-Dentists
    ## 496          dentists         5    5       1              European-Dentists
    ## 497           systems         4    7       1              European-Dentists
    ## 498             draft         4    7       1              European-Dentists
    ## 499               ced         4    7       1              European-Dentists
    ## 500             legal         3   10       1              European-Dentists
    ## 501                ai        23    1       1            European-Disability
    ## 502           persons        11    2       1            European-Disability
    ## 503      disabilities        11    2       1            European-Disability
    ## 504           systems         9    4       1            European-Disability
    ## 505     accessibility         9    4       1            European-Disability
    ## 506            ensure         7    6       1            European-Disability
    ## 507               use         6    7       1            European-Disability
    ## 508          measures         6    7       1            European-Disability
    ## 509          european         5    9       1            European-Disability
    ## 510                eu         5    9       1            European-Disability
    ## 511                ai         9    1       1             European-Radiology
    ## 512        healthcare         9    1       1             European-Radiology
    ## 513          european         8    3       1             European-Radiology
    ## 514              data         6    4       1             European-Radiology
    ## 515               use         4    5       1             European-Radiology
    ## 516         radiology         4    5       1             European-Radiology
    ## 517           prevent         3    7       1             European-Radiology
    ## 518        regulation         3    7       1             European-Radiology
    ## 519           society         3    7       1             European-Radiology
    ## 520        considered         3    7       1             European-Radiology
    ## 521                ai        72    1       1                      Eurosmart
    ## 522              data        33    2       1                      Eurosmart
    ## 523         eurosmart        33    2       1                      Eurosmart
    ## 524           systems        32    4       1                      Eurosmart
    ## 525               use        20    5       1                      Eurosmart
    ## 526          european        17    6       1                      Eurosmart
    ## 527            system        15    7       1                      Eurosmart
    ## 528     cybersecurity        15    7       1                      Eurosmart
    ## 529               act        13    9       1                      Eurosmart
    ## 530        commission        12   10       1                      Eurosmart
    ## 531                ai        51    1       1             Evangelig-Alliance
    ## 532             human        22    2       1             Evangelig-Alliance
    ## 533            impact        16    3       1             Evangelig-Alliance
    ## 534               can        15    4       1             Evangelig-Alliance
    ## 535              data        14    5       1             Evangelig-Alliance
    ## 536              harm        12    6       1             Evangelig-Alliance
    ## 537            rights        11    7       1             Evangelig-Alliance
    ## 538              used        11    7       1             Evangelig-Alliance
    ## 539            people        10    9       1             Evangelig-Alliance
    ## 540          humanity         9   10       1             Evangelig-Alliance
    ## 541                ai       272    1       1                       Facebook
    ## 542              data       142    2       1                       Facebook
    ## 543           systems        76    3       1                       Facebook
    ## 544               act        53    4       1                       Facebook
    ## 545               can        46    5       1                       Facebook
    ## 546         standards        44    6       1                       Facebook
    ## 547            system        41    7       1                       Facebook
    ## 548          facebook        38    8       1                       Facebook
    ## 549               use        37    9       1                       Facebook
    ## 550              bias        37    9       1                       Facebook
    ## 551                ai       144    1       1                    Fair-Trials
    ## 552           systems        97    2       1                    Fair-Trials
    ## 553          criminal        82    3       1                    Fair-Trials
    ## 554           justice        68    4       1                    Fair-Trials
    ## 555              data        65    5       1                    Fair-Trials
    ## 556               law        63    6       1                    Fair-Trials
    ## 557       enforcement        52    7       1                    Fair-Trials
    ## 558            system        46    8       1                    Fair-Trials
    ## 559           article        45    9       1                    Fair-Trials
    ## 560               act        42   10       1                    Fair-Trials
    ## 561              data         7    1       1            Fraud-Corruption-AI
    ## 562              also         6    2       1            Fraud-Corruption-AI
    ## 563                ai         5    3       1            Fraud-Corruption-AI
    ## 564            health         5    3       1            Fraud-Corruption-AI
    ## 565            public         3    5       1            Fraud-Corruption-AI
    ## 566        artificial         3    5       1            Fraud-Corruption-AI
    ## 567      intelligence         3    5       1            Fraud-Corruption-AI
    ## 568        regulatory         3    5       1            Fraud-Corruption-AI
    ## 569              gdpr         3    5       1            Fraud-Corruption-AI
    ## 570              user         3    5       1            Fraud-Corruption-AI
    ## 571                ai       110    1       1                        Fujitsu
    ## 572           fujitsu        50    2       1                        Fujitsu
    ## 573          european        36    3       1                        Fujitsu
    ## 574           article        29    4       1                        Fujitsu
    ## 575               act        25    5       1                        Fujitsu
    ## 576              data        24    6       1                        Fujitsu
    ## 577        commission        23    7       1                        Fujitsu
    ## 578      intelligence        21    8       1                        Fujitsu
    ## 579           systems        21    8       1                        Fujitsu
    ## 580        artificial        20   10       1                        Fujitsu
    ## 581                ai        92    1       1                  Future-ofLife
    ## 582           systems        33    2       1                  Future-ofLife
    ## 583          proposal        21    3       1                  Future-ofLife
    ## 584               fli        20    4       1                  Future-ofLife
    ## 585          european        19    5       1                  Future-ofLife
    ## 586               act        17    6       1                  Future-ofLife
    ## 587                eu        16    7       1                  Future-ofLife
    ## 588      applications        15    8       1                  Future-ofLife
    ## 589               may        14    9       1                  Future-ofLife
    ## 590               can        14    9       1                  Future-ofLife
    ## 591                ai        92    1       1                 Future-Society
    ## 592                 ●        52    2       1                 Future-Society
    ## 593        governance        31    3       1                 Future-Society
    ## 594            system        29    4       1                 Future-Society
    ## 595           systems        24    5       1                 Future-Society
    ## 596          european        22    6       1                 Future-Society
    ## 597            market        21    7       1                 Future-Society
    ## 598       information        21    7       1                 Future-Society
    ## 599                eu        20    9       1                 Future-Society
    ## 600               act        18   10       1                 Future-Society
    ## 601                ai        75    1       1                    German-Bank
    ## 602        regulation        71    2       1                    German-Bank
    ## 603              risk        62    3       1                    German-Bank
    ## 604      requirements        55    4       1                    German-Bank
    ## 605              also        48    5       1                    German-Bank
    ## 606            credit        43    6       1                    German-Bank
    ## 607         financial        42    7       1                    German-Bank
    ## 608             risks        41    8       1                    German-Bank
    ## 609               can        39    9       1                    German-Bank
    ## 610      institutions        39    9       1                    German-Bank
    ## 611                ai        26    1       1                   Getty-Images
    ## 612           content        26    1       1                   Getty-Images
    ## 613            images        13    3       1                   Getty-Images
    ## 614         editorial        13    3       1                   Getty-Images
    ## 615         high-risk        12    5       1                   Getty-Images
    ## 616             getty        12    5       1                   Getty-Images
    ## 617           systems        11    7       1                   Getty-Images
    ## 618           article        11    7       1                   Getty-Images
    ## 619            system        10    9       1                   Getty-Images
    ## 620          creative         8   10       1                   Getty-Images
    ## 621              data        25    1       1                          GLEIF
    ## 622                ai        23    2       1                          GLEIF
    ## 623               lei        22    3       1                          GLEIF
    ## 624             legal        20    4       1                          GLEIF
    ## 625             gleif        20    4       1                          GLEIF
    ## 626            entity        15    6       1                          GLEIF
    ## 627               use        12    7       1                          GLEIF
    ## 628         financial        12    7       1                          GLEIF
    ## 629            global         9    9       1                          GLEIF
    ## 630         standards         8   10       1                          GLEIF
    ## 631                ai        17    1       1                          Glovo
    ## 632             glovo        14    2       1                          Glovo
    ## 633           systems        11    3       1                          Glovo
    ## 634                 y        11    3       1                          Glovo
    ## 635      transparency        10    5       1                          Glovo
    ## 636        artificial         8    6       1                          Glovo
    ## 637      intelligence         8    6       1                          Glovo
    ## 638            models         8    6       1                          Glovo
    ## 639        innovation         7    9       1                          Glovo
    ## 640         practices         7    9       1                          Glovo
    ## 641                ai        89    1       1                         Google
    ## 642            system        72    2       1                         Google
    ## 643           systems        67    3       1                         Google
    ## 644           article        41    4       1                         Google
    ## 645              data        38    5       1                         Google
    ## 646         high-risk        29    6       1                         Google
    ## 647          datasets        29    6       1                         Google
    ## 648               use        28    8       1                         Google
    ## 649         providers        27    9       1                         Google
    ## 650               aia        27    9       1                         Google
    ## 651                ai       131    1       1                       Hangzhou
    ## 652            system        48    2       1                       Hangzhou
    ## 653        regulation        46    3       1                       Hangzhou
    ## 654          proposed        45    4       1                       Hangzhou
    ## 655           systems        38    5       1                       Hangzhou
    ## 656               use        24    6       1                       Hangzhou
    ## 657      requirements        21    7       1                       Hangzhou
    ## 658                eu        20    8       1                       Hangzhou
    ## 659            market        17    9       1                       Hangzhou
    ## 660            public        14   10       1                       Hangzhou
    ## 661                ai       132    1       1              Hoffmann-La-Roche
    ## 662              data       102    2       1              Hoffmann-La-Roche
    ## 663           systems        55    3       1              Hoffmann-La-Roche
    ## 664        regulation        52    4       1              Hoffmann-La-Roche
    ## 665            system        40    5       1              Hoffmann-La-Roche
    ## 666               act        36    6       1              Hoffmann-La-Roche
    ## 667           medical        36    6       1              Hoffmann-La-Roche
    ## 668            health        35    8       1              Hoffmann-La-Roche
    ## 669                eu        31    9       1              Hoffmann-La-Roche
    ## 670            within        29   10       1              Hoffmann-La-Roche
    ## 671                ai        24    1       1                  Hogan-Lovells
    ## 672        regulation        21    2       1                  Hogan-Lovells
    ## 673             draft        18    3       1                  Hogan-Lovells
    ## 674           systems        15    4       1                  Hogan-Lovells
    ## 675         providers        13    5       1                  Hogan-Lovells
    ## 676             hogan        13    5       1                  Hogan-Lovells
    ## 677           lovells        13    5       1                  Hogan-Lovells
    ## 678              data        10    8       1                  Hogan-Lovells
    ## 679             users         9    9       1                  Hogan-Lovells
    ## 680                eu         8   10       1                  Hogan-Lovells
    ## 681                ai        19    1       1              Hospital-Pharmacy
    ## 682           systems        18    2       1              Hospital-Pharmacy
    ## 683              data        10    3       1              Hospital-Pharmacy
    ## 684               use         7    4       1              Hospital-Pharmacy
    ## 685            health         7    4       1              Hospital-Pharmacy
    ## 686      requirements         6    6       1              Hospital-Pharmacy
    ## 687          proposal         6    6       1              Hospital-Pharmacy
    ## 688              used         5    8       1              Hospital-Pharmacy
    ## 689        regulation         5    8       1              Hospital-Pharmacy
    ## 690             legal         4   10       1              Hospital-Pharmacy
    ## 691                ai        76    1       1                         Huawei
    ## 692           article        41    2       1                         Huawei
    ## 693           systems        36    3       1                         Huawei
    ## 694            system        33    4       1                         Huawei
    ## 695             shall        28    5       1                         Huawei
    ## 696         high-risk        22    6       1                         Huawei
    ## 697        artificial        17    7       1                         Huawei
    ## 698      intelligence        17    7       1                         Huawei
    ## 699           digital        17    7       1                         Huawei
    ## 700            market        17    7       1                         Huawei
    ## 701                ai        72    1       1                            IBM
    ## 702           systems        34    2       1                            IBM
    ## 703        regulation        18    3       1                            IBM
    ## 704           article        18    3       1                            IBM
    ## 705            system        16    5       1                            IBM
    ## 706         high-risk        15    6       1                            IBM
    ## 707        commission        15    6       1                            IBM
    ## 708             tools        15    6       1                            IBM
    ## 709               use        14    9       1                            IBM
    ## 710               ibm        14    9       1                            IBM
    ## 711                ai        46    1       1                         IDEMIA
    ## 712        regulation        27    2       1                         IDEMIA
    ## 713            idemia        24    3       1                         IDEMIA
    ## 714             draft        23    4       1                         IDEMIA
    ## 715                 |        21    5       1                         IDEMIA
    ## 716          approach        20    6       1                         IDEMIA
    ## 717               use        19    7       1                         IDEMIA
    ## 718                ex        18    8       1                         IDEMIA
    ## 719      requirements        16    9       1                         IDEMIA
    ## 720                eu        16    9       1                         IDEMIA
    ## 721                ai        81    1       1                      Impact-AI
    ## 722                 o        40    2       1                      Impact-AI
    ## 723            impact        19    3       1                      Impact-AI
    ## 724          european        15    4       1                      Impact-AI
    ## 725              code        15    4       1                      Impact-AI
    ## 726               act        14    6       1                      Impact-AI
    ## 727              data        14    6       1                      Impact-AI
    ## 728                ec        13    8       1                      Impact-AI
    ## 729                eu        13    8       1                      Impact-AI
    ## 730           systems        13    8       1                      Impact-AI
    ## 731                ai        19    1       1                       Infineon
    ## 732        definition        13    2       1                       Infineon
    ## 733           systems        10    3       1                       Infineon
    ## 734               act         6    4       1                       Infineon
    ## 735           effects         6    4       1                       Infineon
    ## 736                 >         6    4       1                       Infineon
    ## 737                 <         6    4       1                       Infineon
    ## 738        technology         5    8       1                       Infineon
    ## 739          infineon         5    8       1                       Infineon
    ## 740        regulation         4   10       1                       Infineon
    ## 741                ai       133    1       1     Information-Accountability
    ## 742              data       114    2       1     Information-Accountability
    ## 743              gdpr        62    3       1     Information-Accountability
    ## 744        regulation        52    4       1     Information-Accountability
    ## 745              risk        44    5       1     Information-Accountability
    ## 746        processing        36    6       1     Information-Accountability
    ## 747         knowledge        33    7       1     Information-Accountability
    ## 748       assessments        25    8       1     Information-Accountability
    ## 749            impact        23    9       1     Information-Accountability
    ## 750           article        23    9       1     Information-Accountability
    ## 751                ai        16    1       1                        InkedIn
    ## 752           systems        14    2       1                        InkedIn
    ## 753               job        12    3       1                        InkedIn
    ## 754        definition         8    4       1                        InkedIn
    ## 755               may         7    5       1                        InkedIn
    ## 756            hiring         7    5       1                        InkedIn
    ## 757          linkedin         7    5       1                        InkedIn
    ## 758               act         6    8       1                        InkedIn
    ## 759       fundamental         6    8       1                        InkedIn
    ## 760            rights         5   10       1                        InkedIn
    ## 761                ai        27    1       1                          Intel
    ## 762           systems        11    2       1                          Intel
    ## 763        regulation         9    3       1                          Intel
    ## 764            safety         7    4       1                          Intel
    ## 765          proposed         6    5       1                          Intel
    ## 766         providers         6    5       1                          Intel
    ## 767         component         6    5       1                          Intel
    ## 768              used         5    8       1                          Intel
    ## 769            system         5    8       1                          Intel
    ## 770             annex         5    8       1                          Intel
    ## 771           article        34    1       1                    Intesa-Bank
    ## 772                ai        25    2       1                    Intesa-Bank
    ## 773                 o        25    2       1                    Intesa-Bank
    ## 774           systems        22    4       1                    Intesa-Bank
    ## 775              data        19    5       1                    Intesa-Bank
    ## 776               aia        17    6       1                    Intesa-Bank
    ## 777             annex        14    7       1                    Intesa-Bank
    ## 778        regulation        12    8       1                    Intesa-Bank
    ## 779               iii        12    8       1                    Intesa-Bank
    ## 780            credit        11   10       1                    Intesa-Bank
    ## 781                ai       109    1       1                          JEITA
    ## 782           systems        63    2       1                          JEITA
    ## 783           article        44    3       1                          JEITA
    ## 784              data        42    4       1                          JEITA
    ## 785         high-risk        31    5       1                          JEITA
    ## 786                eu        28    6       1                          JEITA
    ## 787               use        26    7       1                          JEITA
    ## 788              risk        25    8       1                          JEITA
    ## 789       information        25    8       1                          JEITA
    ## 790                 ▪        25    8       1                          JEITA
    ## 791                ai        66    1       1                         Johner
    ## 792        regulation        37    2       1                         Johner
    ## 793           medical        29    3       1                         Johner
    ## 794            system        25    4       1                         Johner
    ## 795              risk        23    5       1                         Johner
    ## 796     manufacturers        23    5       1                         Johner
    ## 797           devices        20    7       1                         Johner
    ## 798            johner        20    7       1                         Johner
    ## 799        validation        19    9       1                         Johner
    ## 800            device        19    9       1                         Johner
    ## 801                ai        27    1       1                Johnson-Johnson
    ## 802               mdr        17    2       1                Johnson-Johnson
    ## 803              ivdr        17    2       1                Johnson-Johnson
    ## 804           medical        12    4       1                Johnson-Johnson
    ## 805        regulation        11    5       1                Johnson-Johnson
    ## 806               aia        11    5       1                Johnson-Johnson
    ## 807              data         9    7       1                Johnson-Johnson
    ## 808          guidance         9    7       1                Johnson-Johnson
    ## 809      requirements         8    9       1                Johnson-Johnson
    ## 810        regulatory         8    9       1                Johnson-Johnson
    ## 811                ai        33    1       1                        KMD-NEC
    ## 812               kmd        20    2       1                        KMD-NEC
    ## 813          proposal        14    3       1                        KMD-NEC
    ## 814         solutions        13    4       1                        KMD-NEC
    ## 815            market         9    5       1                        KMD-NEC
    ## 816           systems         8    6       1                        KMD-NEC
    ## 817        regulation         8    6       1                        KMD-NEC
    ## 818           article         8    6       1                        KMD-NEC
    ## 819             title         8    6       1                        KMD-NEC
    ## 820              data         7   10       1                        KMD-NEC
    ## 821        artificial        52    1       1                       Lewiatan
    ## 822      intelligence        52    1       1                       Lewiatan
    ## 823           systems        25    3       1                       Lewiatan
    ## 824              risk        18    4       1                       Lewiatan
    ## 825            system        18    4       1                       Lewiatan
    ## 826            rights        15    6       1                       Lewiatan
    ## 827               may        15    6       1                       Lewiatan
    ## 828              term        15    6       1                       Lewiatan
    ## 829       fundamental        13    9       1                       Lewiatan
    ## 830              para        12   10       1                       Lewiatan
    ## 831                ai        23    1       1                 Liberty-Global
    ## 832               sdn        21    2       1                 Liberty-Global
    ## 833          networks        19    3       1                 Liberty-Global
    ## 834           network        15    4       1                 Liberty-Global
    ## 835               nfv        15    4       1                 Liberty-Global
    ## 836           liberty        14    6       1                 Liberty-Global
    ## 837      intelligence        13    7       1                 Liberty-Global
    ## 838            global        13    7       1                 Liberty-Global
    ## 839              code        12    9       1                 Liberty-Global
    ## 840        artificial        11   10       1                 Liberty-Global
    ## 841                ai        37    1       1                            LNE
    ## 842           article        20    2       1                            LNE
    ## 843        assessment        14    3       1                            LNE
    ## 844           systems        13    4       1                            LNE
    ## 845      requirements        10    5       1                            LNE
    ## 846            bodies        10    5       1                            LNE
    ## 847          notified        10    5       1                            LNE
    ## 848            system         9    8       1                            LNE
    ## 849               etc         9    8       1                            LNE
    ## 850               use         8   10       1                            LNE
    ## 851                ai        97    1       1                     Mastercard
    ## 852            system        28    2       1                     Mastercard
    ## 853               act        27    3       1                     Mastercard
    ## 854         biometric        23    4       1                     Mastercard
    ## 855              data        18    5       1                     Mastercard
    ## 856           systems        17    6       1                     Mastercard
    ## 857       authorities        16    7       1                     Mastercard
    ## 858              risk        15    8       1                     Mastercard
    ## 859      requirements        14    9       1                     Mastercard
    ## 860       individuals        14    9       1                     Mastercard
    ## 861                ai       108    1       1                       Mediaset
    ## 862           systems        53    2       1                       Mediaset
    ## 863            system        36    3       1                       Mediaset
    ## 864         high-risk        26    4       1                       Mediaset
    ## 865               art        22    5       1                       Mediaset
    ## 866               use        20    6       1                       Mediaset
    ## 867               act        18    7       1                       Mediaset
    ## 868                eu        18    7       1                       Mediaset
    ## 869          proposal        17    9       1                       Mediaset
    ## 870            market        16   10       1                       Mediaset
    ## 871                ai        17    1       1               Medical-Oncology
    ## 872        healthcare        11    2       1               Medical-Oncology
    ## 873           systems         9    3       1               Medical-Oncology
    ## 874               use         8    4       1               Medical-Oncology
    ## 875           medical         8    4       1               Medical-Oncology
    ## 876          research         8    4       1               Medical-Oncology
    ## 877                eu         7    7       1               Medical-Oncology
    ## 878        regulation         7    7       1               Medical-Oncology
    ## 879             rules         6    9       1               Medical-Oncology
    ## 880     professionals         6    9       1               Medical-Oncology
    ## 881           medical        30    1       1                        Medtech
    ## 882                ai        23    2       1                        Medtech
    ## 883               aia        18    3       1                        Medtech
    ## 884              data        15    4       1                        Medtech
    ## 885        technology        15    4       1                        Medtech
    ## 886         providers        13    6       1                        Medtech
    ## 887               mdr        11    7       1                        Medtech
    ## 888              gdpr        10    8       1                        Medtech
    ## 889            europe        10    8       1                        Medtech
    ## 890           article        10    8       1                        Medtech
    ## 891                ai       115    1       1                      Medtronic
    ## 892        regulation        89    2       1                      Medtronic
    ## 893               mdr        65    3       1                      Medtronic
    ## 894      requirements        47    4       1                      Medtronic
    ## 895           medical        46    5       1                      Medtronic
    ## 896              ivdr        41    6       1                      Medtronic
    ## 897          proposed        39    7       1                      Medtronic
    ## 898           devices        38    8       1                      Medtronic
    ## 899           article        34    9       1                      Medtronic
    ## 900     manufacturers        33   10       1                      Medtronic
    ## 901                ai        18    1       1                          MERCK
    ## 902        regulatory         9    2       1                          MERCK
    ## 903              data         8    3       1                          MERCK
    ## 904          approach         5    4       1                          MERCK
    ## 905             avoid         5    4       1                          MERCK
    ## 906        regulation         5    4       1                          MERCK
    ## 907            public         4    7       1                          MERCK
    ## 908           ethical         4    7       1                          MERCK
    ## 909               act         4    7       1                          MERCK
    ## 910                eu         4    7       1                          MERCK
    ## 911                ai       182    1       1                      Microsoft
    ## 912           systems        63    2       1                      Microsoft
    ## 913            system        57    3       1                      Microsoft
    ## 914               act        52    4       1                      Microsoft
    ## 915               use        41    5       1                      Microsoft
    ## 916           article        40    6       1                      Microsoft
    ## 917             risks        36    7       1                      Microsoft
    ## 918              also        30    8       1                      Microsoft
    ## 919        technology        30    8       1                      Microsoft
    ## 920              data        26   10       1                      Microsoft
    ## 921           systems        33    1       1                  Moje-Panvisto
    ## 922                ai        24    2       1                  Moje-Panvisto
    ## 923            public        20    3       1                  Moje-Panvisto
    ## 924          european        14    4       1                  Moje-Panvisto
    ## 925        artificial        13    5       1                  Moje-Panvisto
    ## 926      intelligence        13    5       1                  Moje-Panvisto
    ## 927          proposal        12    7       1                  Moje-Panvisto
    ## 928         high-risk        11    8       1                  Moje-Panvisto
    ## 929               use        10    9       1                  Moje-Panvisto
    ## 930          database        10    9       1                  Moje-Panvisto
    ## 931                ai        12    1       1                         MoveEU
    ## 932               use         8    2       1                         MoveEU
    ## 933          proposal         6    3       1                         MoveEU
    ## 934           believe         5    4       1                         MoveEU
    ## 935       fundamental         4    5       1                         MoveEU
    ## 936              gdpr         4    5       1                         MoveEU
    ## 937         high-risk         4    5       1                         MoveEU
    ## 938          european         3    8       1                         MoveEU
    ## 939                eu         3    8       1                         MoveEU
    ## 940         therefore         3    8       1                         MoveEU
    ## 941                ai       108    1       1                            NEC
    ## 942               nec        57    2       1                            NEC
    ## 943        regulation        52    3       1                            NEC
    ## 944             draft        51    4       1                            NEC
    ## 945            system        50    5       1                            NEC
    ## 946           article        36    6       1                            NEC
    ## 947      requirements        28    7       1                            NEC
    ## 948           systems        21    8       1                            NEC
    ## 949              user        18    9       1                            NEC
    ## 950          provider        18    9       1                            NEC
    ## 951                ai        67    1       1                          Nokia
    ## 952             nokia        45    2       1                          Nokia
    ## 953        regulation        32    3       1                          Nokia
    ## 954           systems        23    4       1                          Nokia
    ## 955          approach        19    5       1                          Nokia
    ## 956          european        17    6       1                          Nokia
    ## 957      requirements        15    7       1                          Nokia
    ## 958        regulatory        15    7       1                          Nokia
    ## 959             given        15    7       1                          Nokia
    ## 960          proposal        14   10       1                          Nokia
    ## 961                ai       147    1       1             Not-for-profit-law
    ## 962           systems        89    2       1             Not-for-profit-law
    ## 963               act        58    3       1             Not-for-profit-law
    ## 964            rights        56    4       1             Not-for-profit-law
    ## 965             human        40    5       1             Not-for-profit-law
    ## 966              ecnl        35    6       1             Not-for-profit-law
    ## 967              risk        33    7       1             Not-for-profit-law
    ## 968          affected        28    8       1             Not-for-profit-law
    ## 969            groups        26    9       1             Not-for-profit-law
    ## 970          european        25   10       1             Not-for-profit-law
    ## 971                ai       168    1       1                       Novartis
    ## 972               use        68    2       1                       Novartis
    ## 973              data        62    3       1                       Novartis
    ## 974           systems        59    4       1                       Novartis
    ## 975          novartis        56    5       1                       Novartis
    ## 976           ethical        36    6       1                       Novartis
    ## 977        principles        29    7       1                       Novartis
    ## 978             human        27    8       1                       Novartis
    ## 979       information        26    9       1                       Novartis
    ## 980      intelligence        23   10       1                       Novartis
    ## 981                ai        16    1       1                         Onfido
    ## 982                eu         7    2       1                         Onfido
    ## 983         biometric         7    2       1                         Onfido
    ## 984        innovation         6    4       1                         Onfido
    ## 985       definitions         6    4       1                         Onfido
    ## 986          proposal         5    6       1                         Onfido
    ## 987               use         5    6       1                         Onfido
    ## 988        commission         5    6       1                         Onfido
    ## 989            onfido         5    6       1                         Onfido
    ## 990          european         4   10       1                         Onfido
    ## 991              data        27    1       1                    Open-Future
    ## 992                ai        24    2       1                    Open-Future
    ## 993         biometric        15    3       1                    Open-Future
    ## 994               act        13    4       1                    Open-Future
    ## 995        protection        10    5       1                    Open-Future
    ## 996        definition         9    6       1                    Open-Future
    ## 997          training         9    6       1                    Open-Future
    ## 998            facial         7    8       1                    Open-Future
    ## 999               use         6    9       1                    Open-Future
    ## 1000            users         6    9       1                    Open-Future
    ## 1001               ai        24    1       1                         OpenAI
    ## 1002          systems        13    2       1                         OpenAI
    ## 1003              can        11    3       1                         OpenAI
    ## 1004              use         9    4       1                         OpenAI
    ## 1005           system         9    4       1                         OpenAI
    ## 1006     applications         7    6       1                         OpenAI
    ## 1007        technical         7    6       1                         OpenAI
    ## 1008         research         7    6       1                         OpenAI
    ## 1009         proposal         6    9       1                         OpenAI
    ## 1010         european         6    9       1                         OpenAI
    ## 1011               ai        43    1       1                         Orange
    ## 1012               eu        15    2       1                         Orange
    ## 1013           orange        15    2       1                         Orange
    ## 1014          systems        10    4       1                         Orange
    ## 1015      obligations         9    5       1                         Orange
    ## 1016     requirements         8    6       1                         Orange
    ## 1017         proposal         8    6       1                         Orange
    ## 1018         approach         8    6       1                         Orange
    ## 1019          ethical         7    9       1                         Orange
    ## 1020              act         7    9       1                         Orange
    ## 1021               ai        27    1       1                           PGEU
    ## 1022       healthcare        14    2       1                           PGEU
    ## 1023          systems        13    3       1                           PGEU
    ## 1024             data        11    4       1                           PGEU
    ## 1025           health        11    4       1                           PGEU
    ## 1026         european        10    6       1                           PGEU
    ## 1027             pgeu         9    7       1                           PGEU
    ## 1028      pharmacists         8    8       1                           PGEU
    ## 1029         proposal         7    9       1                           PGEU
    ## 1030            users         7    9       1                           PGEU
    ## 1031               ai        18    1       1                        Philips
    ## 1032             data        15    2       1                        Philips
    ## 1033     requirements        11    3       1                        Philips
    ## 1034          medical        11    3       1                        Philips
    ## 1035            human         8    5       1                        Philips
    ## 1036         proposed         8    5       1                        Philips
    ## 1037           system         8    5       1                        Philips
    ## 1038         training         8    5       1                        Philips
    ## 1039           health         8    5       1                        Philips
    ## 1040          patient         8    5       1                        Philips
    ## 1041               ai        89    1       1                           RELX
    ## 1042          article        34    2       1                           RELX
    ## 1043           system        31    3       1                           RELX
    ## 1044          systems        28    4       1                           RELX
    ## 1045       regulation        25    5       1                           RELX
    ## 1046        high-risk        21    6       1                           RELX
    ## 1047               eu        20    7       1                           RELX
    ## 1048             data        15    8       1                           RELX
    ## 1049             risk        15    8       1                           RELX
    ## 1050             also        14   10       1                           RELX
    ## 1051          systems        68    1       1          Renaissance-Numerique
    ## 1052         european        64    2       1          Renaissance-Numerique
    ## 1053               ai        64    2       1          Renaissance-Numerique
    ## 1054       regulation        63    4       1          Renaissance-Numerique
    ## 1055      authorities        48    5       1          Renaissance-Numerique
    ## 1056          article        39    6       1          Renaissance-Numerique
    ## 1057       artificial        36    7       1          Renaissance-Numerique
    ## 1058     intelligence        36    7       1          Renaissance-Numerique
    ## 1059             text        32    9       1          Renaissance-Numerique
    ## 1060             also        31   10       1          Renaissance-Numerique
    ## 1061               ai        23    1       1                         ResMed
    ## 1062           resmed        16    2       1                         ResMed
    ## 1063             data        12    3       1                         ResMed
    ## 1064      legislation         9    4       1                         ResMed
    ## 1065         european         7    5       1                         ResMed
    ## 1066            urges         7    5       1                         ResMed
    ## 1067           ensure         6    7       1                         ResMed
    ## 1068       commission         6    7       1                         ResMed
    ## 1069            board         6    7       1                         ResMed
    ## 1070         proposed         5   10       1                         ResMed
    ## 1071               ai        12    1       1                         sanofi
    ## 1072             data         9    2       1                         sanofi
    ## 1073         evidence         7    3       1                         sanofi
    ## 1074       regulation         5    4       1                         sanofi
    ## 1075             risk         4    5       1                         sanofi
    ## 1076              can         4    5       1                         sanofi
    ## 1077             free         4    5       1                         sanofi
    ## 1078            error         4    5       1                         sanofi
    ## 1079         proposed         3    9       1                         sanofi
    ## 1080              use         3    9       1                         sanofi
    ## 1081               ai        75    1       1                            SAP
    ## 1082          systems        36    2       1                            SAP
    ## 1083                ▪        30    3       1                            SAP
    ## 1084               eu        20    4       1                            SAP
    ## 1085             data        20    4       1                            SAP
    ## 1086        high-risk        19    6       1                            SAP
    ## 1087     requirements        18    7       1                            SAP
    ## 1088         proposal        14    8       1                            SAP
    ## 1089             also        14    8       1                            SAP
    ## 1090           system        14    8       1                            SAP
    ## 1091               ai        17    1       1                          SAZKA
    ## 1092         gambling        16    2       1                          SAZKA
    ## 1093               sg        15    3       1                          SAZKA
    ## 1094            shall        12    4       1                          SAZKA
    ## 1095              use        10    5       1                          SAZKA
    ## 1096     requirements         9    6       1                          SAZKA
    ## 1097             data         9    6       1                          SAZKA
    ## 1098     applications         8    8       1                          SAZKA
    ## 1099             risk         8    8       1                          SAZKA
    ## 1100            sazka         7   10       1                          SAZKA
    ## 1101          systems        55    1       1                     SellaGroup
    ## 1102               ai        45    2       1                     SellaGroup
    ## 1103             used        23    3       1                     SellaGroup
    ## 1104       regulatory        20    4       1                     SellaGroup
    ## 1105       regulation        18    5       1                     SellaGroup
    ## 1106               di        18    5       1                     SellaGroup
    ## 1107        high-risk        17    7       1                     SellaGroup
    ## 1108            sella        15    8       1                     SellaGroup
    ## 1109              use        14    9       1                     SellaGroup
    ## 1110          persons        14    9       1                     SellaGroup
    ## 1111               ai        95    1       1                           SEMI
    ## 1112                |        33    2       1                           SEMI
    ## 1113             data        31    3       1                           SEMI
    ## 1114             semi        31    3       1                           SEMI
    ## 1115           europe        28    5       1                           SEMI
    ## 1116         industry        23    6       1                           SEMI
    ## 1117               eu        22    7       1                           SEMI
    ## 1118        standards        22    7       1                           SEMI
    ## 1119     technologies        21    9       1                           SEMI
    ## 1120              use        21    9       1                           SEMI
    ## 1121           errors         4    1       1                           SICK
    ## 1122          article         4    1       1                           SICK
    ## 1123         bayesian         4    1       1                           SICK
    ## 1124     requirements         3    4       1                           SICK
    ## 1125             data         3    4       1                           SICK
    ## 1126       techniques         3    4       1                           SICK
    ## 1127          methods         3    4       1                           SICK
    ## 1128             sick         3    4       1                           SICK
    ## 1129               ag         3    4       1                           SICK
    ## 1130        paragraph         3    4       1                           SICK
    ## 1131               ai        84    1       1                        Siemens
    ## 1132           system        38    2       1                        Siemens
    ## 1133          systems        34    3       1                        Siemens
    ## 1134          article        28    4       1                        Siemens
    ## 1135             data        20    5       1                        Siemens
    ## 1136       regulation        20    5       1                        Siemens
    ## 1137             risk        18    7       1                        Siemens
    ## 1138     requirements        17    8       1                        Siemens
    ## 1139        high-risk        15    9       1                        Siemens
    ## 1140         specific        14   10       1                        Siemens
    ## 1141               ai        30    1       1                 Siemens-Energy
    ## 1142         proposal        13    2       1                 Siemens-Energy
    ## 1143             data        12    3       1                 Siemens-Energy
    ## 1144           energy        11    4       1                 Siemens-Energy
    ## 1145       regulation        10    5       1                 Siemens-Energy
    ## 1146           europe        10    5       1                 Siemens-Energy
    ## 1147          systems         9    7       1                 Siemens-Energy
    ## 1148         european         8    8       1                 Siemens-Energy
    ## 1149      legislation         6    9       1                 Siemens-Energy
    ## 1150              use         6    9       1                 Siemens-Energy
    ## 1151               ai        54    1       1           Siemens-Healthineers
    ## 1152          medical        49    2       1           Siemens-Healthineers
    ## 1153             data        31    3       1           Siemens-Healthineers
    ## 1154          devices        23    4       1           Siemens-Healthineers
    ## 1155         software        22    5       1           Siemens-Healthineers
    ## 1156           system        22    5       1           Siemens-Healthineers
    ## 1157              act        19    7       1           Siemens-Healthineers
    ## 1158     requirements        17    8       1           Siemens-Healthineers
    ## 1159              may        15    9       1           Siemens-Healthineers
    ## 1160         proposed        14   10       1           Siemens-Healthineers
    ## 1161               ai        30    1       1                            Sky
    ## 1162          systems        17    2       1                            Sky
    ## 1163      obligations         8    3       1                            Sky
    ## 1164             risk         6    4       1                            Sky
    ## 1165       definition         6    4       1                            Sky
    ## 1166        high-risk         6    4       1                            Sky
    ## 1167     requirements         5    7       1                            Sky
    ## 1168      development         5    7       1                            Sky
    ## 1169     applications         5    7       1                            Sky
    ## 1170             data         5    7       1                            Sky
    ## 1171               ai        77    1       1                         SPLUNK
    ## 1172        high-risk        21    2       1                         SPLUNK
    ## 1173            users        15    3       1                         SPLUNK
    ## 1174          systems        13    4       1                         SPLUNK
    ## 1175        providers        13    4       1                         SPLUNK
    ## 1176          article        13    4       1                         SPLUNK
    ## 1177              use        11    7       1                         SPLUNK
    ## 1178           system        11    7       1                         SPLUNK
    ## 1179       definition        10    9       1                         SPLUNK
    ## 1180     requirements         9   10       1                         SPLUNK
    ## 1181               ai        37    1       1              Standard-Chartred
    ## 1182               eu        12    2       1              Standard-Chartred
    ## 1183          systems        11    3       1              Standard-Chartred
    ## 1184       regulation        10    4       1              Standard-Chartred
    ## 1185       definition         8    5       1              Standard-Chartred
    ## 1186     requirements         7    6       1              Standard-Chartred
    ## 1187       regulatory         7    6       1              Standard-Chartred
    ## 1188          believe         7    6       1              Standard-Chartred
    ## 1189         proposed         6    9       1              Standard-Chartred
    ## 1190              use         6    9       1              Standard-Chartred
    ## 1191           rights        98    1       1                      ThinkTech
    ## 1192         european        80    2       1                      ThinkTech
    ## 1193            human        77    3       1                      ThinkTech
    ## 1194            legal        68    4       1                      ThinkTech
    ## 1195         proposal        61    5       1                      ThinkTech
    ## 1196        available        51    6       1                      ThinkTech
    ## 1197          article        50    7       1                      ThinkTech
    ## 1198        standards        44    8       1                      ThinkTech
    ## 1199       commission        43    9       1                      ThinkTech
    ## 1200     surveillance        41   10       1                      ThinkTech
    ## 1201         children        13    1       1                          Thorn
    ## 1202            child        12    2       1                          Thorn
    ## 1203           online        10    3       1                          Thorn
    ## 1204     technologies         8    4       1                          Thorn
    ## 1205       technology         7    5       1                          Thorn
    ## 1206          protect         7    5       1                          Thorn
    ## 1207       artificial         6    7       1                          Thorn
    ## 1208           sexual         6    7       1                          Thorn
    ## 1209            abuse         6    7       1                          Thorn
    ## 1210     intelligence         5   10       1                          Thorn
    ## 1211               ai        38    1       1                     Trilateral
    ## 1212         research        15    2       1                     Trilateral
    ## 1213          systems        13    3       1                     Trilateral
    ## 1214       regulatory        12    4       1                     Trilateral
    ## 1215          support        11    5       1                     Trilateral
    ## 1216       trilateral        11    5       1                     Trilateral
    ## 1217         european         9    7       1                     Trilateral
    ## 1218      development         9    7       1                     Trilateral
    ## 1219           impact         9    7       1                     Trilateral
    ## 1220     requirements         8   10       1                     Trilateral
    ## 1221               ai        67    1       1                         Twilio
    ## 1222           twilio        38    2       1                         Twilio
    ## 1223           system        23    3       1                         Twilio
    ## 1224          systems        20    4       1                         Twilio
    ## 1225        high-risk        17    5       1                         Twilio
    ## 1226         software        15    6       1                         Twilio
    ## 1227              use        12    7       1                         Twilio
    ## 1228           number        12    7       1                         Twilio
    ## 1229        customers        11    9       1                         Twilio
    ## 1230        providers        11    9       1                         Twilio
    ## 1231               ai        85    1       1                         Unipol
    ## 1232          systems        60    2       1                         Unipol
    ## 1233        high-risk        31    3       1                         Unipol
    ## 1234             also        22    4       1                         Unipol
    ## 1235               eu        19    5       1                         Unipol
    ## 1236              aia        18    6       1                         Unipol
    ## 1237     requirements        17    7       1                         Unipol
    ## 1238             data        16    8       1                         Unipol
    ## 1239              use        16    8       1                         Unipol
    ## 1240          certain        16    8       1                         Unipol
    ## 1241               ai        45    1       1                     Volkswagen
    ## 1242             data        34    2       1                     Volkswagen
    ## 1243          systems        24    3       1                     Volkswagen
    ## 1244           system        21    4       1                     Volkswagen
    ## 1245            shall        18    5       1                     Volkswagen
    ## 1246        high-risk        17    6       1                     Volkswagen
    ## 1247          article        15    7       1                     Volkswagen
    ## 1248              may        11    8       1                     Volkswagen
    ## 1249       definition        11    8       1                     Volkswagen
    ## 1250              use         9   10       1                     Volkswagen
    ## 1251               ai         6    1       1                      Wikimedia
    ## 1252         software         6    1       1                      Wikimedia
    ## 1253             data         5    3       1                      Wikimedia
    ## 1254     organisation         5    3       1                      Wikimedia
    ## 1255           biases         5    3       1                      Wikimedia
    ## 1256              one         4    6       1                      Wikimedia
    ## 1257          article         4    6       1                      Wikimedia
    ## 1258        authority         4    6       1                      Wikimedia
    ## 1259        wikimedia         4    6       1                      Wikimedia
    ## 1260           public         3   10       1                      Wikimedia
    ## 1261               ai        99    1       1                    Women-in-AI
    ## 1262          systems        66    2       1                    Women-in-AI
    ## 1263              art        31    3       1                    Women-in-AI
    ## 1264              act        26    4       1                    Women-in-AI
    ## 1265              use        25    5       1                    Women-in-AI
    ## 1266            women        23    6       1                    Women-in-AI
    ## 1267          austria        22    7       1                    Women-in-AI
    ## 1268              may        21    8       1                    Women-in-AI
    ## 1269           rights        19    9       1                    Women-in-AI
    ## 1270         proposed        18   10       1                    Women-in-AI
    ## 1271               ai       104    1       1                        Workday
    ## 1272             data        52    2       1                        Workday
    ## 1273           system        42    3       1                        Workday
    ## 1274              act        40    4       1                        Workday
    ## 1275          systems        31    5       1                        Workday
    ## 1276     requirements        27    6       1                        Workday
    ## 1277          workday        21    7       1                        Workday
    ## 1278          product        19    8       1                        Workday
    ## 1279       definition        18    9       1                        Workday
    ## 1280        deployers        17   10       1                        Workday
    ## 1281               ai       114    1       1                           Zvei
    ## 1282          article        70    2       1                           Zvei
    ## 1283           system        46    3       1                           Zvei
    ## 1284             data        34    4       1                           Zvei
    ## 1285          product        33    5       1                           Zvei
    ## 1286          systems        33    5       1                           Zvei
    ## 1287       definition        26    7       1                           Zvei
    ## 1288      legislation        25    8       1                           Zvei
    ## 1289      obligations        24    9       1                           Zvei
    ## 1290       regulation        23   10       1                           Zvei

Groups the dfm by type_actor

``` r
toks_aiact3 <- toks_aiact2 %>%
dfm_group(groups = type_actor)
```

Keyness analysis

``` r
tstat_key <- textstat_keyness(toks_aiact3, measure = "chi2")
tplot_key <- textplot_keyness(tstat_key, n = 20)
tplot_key
```

![](sentiment_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

token object based on the first token object

``` r
toks_aiact4 <- toks_aiact %>%
tokens_remove(stopwords("en"), padding = T )
```

Collocation analysis

``` r
col_aiact <- textstat_collocations(toks_aiact4, size = 2:3)
```

Looks for multiword expressions and creates a new token object from this

``` r
toks_aiact5 <- tokens(col_aiact$collocation)
toks_aiact6 <- tokens(toks_aiact4) %>%
tokens_compound(pattern = as.phrase(toks_aiact5))
toks_aiact6
```

    ## Tokens consisting of 129 documents and 3 docvars.
    ## SAZKA - Business :
    ##  [1] "SAZKA_Group"             "submission"             
    ##  [3] ""                        ""                       
    ##  [5] "EC"                      "public_consultation"    
    ##  [7] ""                        "Artificial_Intelligence"
    ##  [9] "–"                       "ethical"                
    ## [11] ""                        "legal_requirements"     
    ## [ ... and 1,383 more ]
    ## 
    ## Allied-Startups - NGO :
    ##  [1] "Feedback_reference"                     
    ##  [2] "F2662175"                               
    ##  [3] "Submitted"                              
    ##  [4] ""                                       
    ##  [5] "13"                                     
    ##  [6] "July_2021_Submitted"                    
    ##  [7] ""                                       
    ##  [8] "Manon"                                  
    ##  [9] "Tabaczynsky"                            
    ## [10] "User_type_Non-governmental_organisation"
    ## [11] "("                                      
    ## [12] "NGO"                                    
    ## [ ... and 491 more ]
    ## 
    ## Civil-Liberties - NGO :
    ##  [1] "RECOMMENDATIONS"                     ""                                   
    ##  [3] ""                                    "COMMISSION’S_AI_REGULATION_PROPOSAL"
    ##  [5] "12"                                  "JULY_2021"                          
    ##  [7] ""                                    "CIVIL_LIBERTIES_UNION"              
    ##  [9] ""                                    "EUROPE"                             
    ## [11] "Recommendations"                     ""                                   
    ## [ ... and 4,173 more ]
    ## 
    ## Google - Business :
    ##  [1] "Consultation"       ""                   ""                  
    ##  [4] "EU_AI_Act_Proposal" "Google’s"           "submission"        
    ##  [7] "July"               "15"                 ","                 
    ## [10] "2021_Table"         ""                   "Contents"          
    ## [ ... and 5,102 more ]
    ## 
    ## PGEU - NGO :
    ##  [1] "PGEU"                         "feedback"                    
    ##  [3] ""                             ""                            
    ##  [5] "European_Commission_Proposal" ""                            
    ##  [7] ""                             "EU_Regulation"               
    ##  [9] ""                             "Artificial_Intelligence"     
    ## [11] ""                             "Pharmaceutical"              
    ## [ ... and 908 more ]
    ## 
    ## SICK - Business :
    ##  [1] "Feedback_reference"              "F2662771"                       
    ##  [3] "Submitted"                       ""                               
    ##  [5] "22_July_2021_Submitted"          ""                               
    ##  [7] "Volker"                          "Schaber"                        
    ##  [9] "User_type_Company"               "/"                              
    ## [11] "business_Organisation"           "SICK_AG_Organisation_size_Large"
    ## [ ... and 232 more ]
    ## 
    ## [ reached max_ndoc ... 123 more documents ]

Removes punctuation characters, punctuation, and symbols from previous
token object and trim it by including only terms that appear at least
three times

``` r
toks_aiact7 <- tokens(toks_aiact6, remove_punct = T, remove_symbols = T) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 3)
toks_aiact7
```

    ## Document-feature matrix of: 129 documents, 10,033 features (93.33% sparse) and 3 docvars.
    ##                        features
    ## docs                         sazka_group submission ec public_consultation
    ##   SAZKA - Business       469           3          1  2                   1
    ##   Allied-Startups - NGO  234           0          0  0                   0
    ##   Civil-Liberties - NGO 1629           0          0  0                   0
    ##   Google - Business     2134           0          1  0                   0
    ##   PGEU - NGO             381           0          0  0                   0
    ##   SICK - Business         93           0          0  0                   0
    ##                        features
    ## docs                    artificial_intelligence ethical legal_requirements
    ##   SAZKA - Business                            2       1                  4
    ##   Allied-Startups - NGO                       1       1                  1
    ##   Civil-Liberties - NGO                       0       0                  0
    ##   Google - Business                           0       0                  0
    ##   PGEU - NGO                                  4       0                  0
    ##   SICK - Business                             1       1                  1
    ##                        features
    ## docs                    ec_proposal artificial_intelligence_act
    ##   SAZKA - Business                2                           1
    ##   Allied-Startups - NGO           0                           4
    ##   Civil-Liberties - NGO           0                           0
    ##   Google - Business               0                           0
    ##   PGEU - NGO                      0                           0
    ##   SICK - Business                 0                           0
    ## [ reached max_ndoc ... 123 more documents, reached max_nfeat ... 10,023 more features ]

Wordfish analysis

``` r
aiact_wf <- textmodel_wordfish(toks_aiact7, dir = c(6,5))
textplot_scale1d(aiact_wf, groups = aiact_wf$type_actor)
```

![](sentiment_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
summary(aiact_wf)
```

    ## 
    ## Call:
    ## textmodel_wordfish.dfm(x = toks_aiact7, dir = c(6, 5))
    ## 
    ## Estimated Document Positions:
    ##                                              theta       se
    ## SAZKA - Business                           2.14692 0.187300
    ## Allied-Startups - NGO                     -0.26631 0.082029
    ## Civil-Liberties - NGO                      0.69097 0.065873
    ## Google - Business                         -0.56546 0.017352
    ## PGEU - NGO                                -0.32277 0.056949
    ## SICK - Business                           -0.74468 0.060843
    ## sanofi - Business                         -0.36558 0.065391
    ## Co-operative-Banks - Business             -0.62136 0.017987
    ## Siemens - Business                        -0.59998 0.019566
    ## BlackBerry - Business                     -0.51554 0.052895
    ## Not-for-profit-law - NGO                  -0.27035 0.024387
    ## Eurocities - NGO                           1.66824 0.127249
    ## Hangzhou - Business                       -0.50608 0.021937
    ## European-Disability - NGO                 -0.39783 0.066837
    ## EUCOPE - Business                          0.31809 0.067884
    ## Eurosmart - Business                      -0.32105 0.035088
    ## Orange - Business                         -0.40922 0.041618
    ## Bayer - Business                          -0.52011 0.056431
    ## Information-Accountability - NGO           0.10797 0.038336
    ## ResMed - Business                         -0.76896 0.032287
    ## IDEMIA - Business                         -0.43824 0.026966
    ## Hoffmann-La-Roche - Business              -0.49991 0.016076
    ## Intesa-Bank - Business                    -0.65102 0.022195
    ## Hospital-Pharmacy - NGO                   -0.23761 0.074651
    ## European-Radiology - NGO                  -0.13444 0.105856
    ## Biogen - Business                         -0.39547 0.065857
    ## OpenAI - Business                          0.12340 0.093383
    ## Workday - Business                        -0.30665 0.025752
    ## EDRi - NGO                                -0.67685 0.009505
    ## SAP - Business                            -0.41747 0.025724
    ## Democracy-Technology - NGO                -0.14274 0.088026
    ## Wikimedia - NGO                            0.40670 0.152176
    ## 5Rights - NGO                              7.76720 0.043736
    ## Thorn - NGO                                3.43299 0.328145
    ## LNE - Business                            -0.47977 0.036329
    ## AstraZeneca - Business                    -0.27914 0.046018
    ## AlgorithmWatch - NGO                      -0.11177 0.029226
    ## JEITA - Business                          -0.44006 0.020897
    ## NEC - Business                            -0.64490 0.017527
    ## Lewiatan - Business                        0.08629 0.049115
    ## Glovo - Business                           0.51195 0.103797
    ## BEUC - NGO                                 0.02480 0.047200
    ## SPLUNK - Business                         -0.47281 0.032901
    ## ThinkTech - NGO                            1.30785 0.056844
    ## MERCK - Business                          -0.01141 0.084210
    ## BBVA - Business                           -0.68431 0.015159
    ## SEMI - Business                           -0.25276 0.034046
    ## Huawei - Business                         -0.59601 0.020205
    ## Liberty-Global - Business                  2.03508 0.156716
    ## Mediaset - Business                       -0.34598 0.027727
    ## Renaissance-Numerique - NGO               -0.04646 0.026514
    ## ACM - NGO                                  0.22475 0.068586
    ## BMW - Business                             3.08461 0.290029
    ## Bits-of-Freedom - NGO                     -0.37322 0.054165
    ## Access-Now - NGO                          -0.16188 0.018388
    ## Novartis - Business                        0.44982 0.045437
    ## Arthur-Legal - Business                    0.80213 0.066400
    ## EON - Business                            -0.40455 0.046624
    ## KMD-NEC - Business                        -0.26149 0.052932
    ## SellaGroup - Business                     -0.67933 0.018209
    ## DeepMind - Business                       -0.13099 0.035583
    ## RELX - Business                           -0.16517 0.033998
    ## MoveEU - NGO                              -0.26254 0.077147
    ## Standard-Chartred - Business              -0.43857 0.043736
    ## CIO - Business                            -0.07029 0.038388
    ## Fujitsu - Business                        -0.39512 0.023679
    ## Deutsche-Börse - Business                 -0.35323 0.060809
    ## Johnson-Johnson - Business                -0.80893 0.021147
    ## Intel - Business                          -0.78810 0.033879
    ## Twilio - Business                         -0.05782 0.048152
    ## German-Bank - Business                    -0.19457 0.022349
    ## European-Dentists - NGO                   -0.15326 0.086085
    ## AIAL - NGO                                -0.36888 0.060178
    ## Volkswagen - Business                     -0.59268 0.027843
    ## Enel - Business                           -0.29637 0.044593
    ## Zvei - Business                           -0.70218 0.014362
    ## CPME - NGO                                -0.62383 0.030236
    ## Equifax - Business                        -0.04792 0.040683
    ## Center-Data-Innovation - NGO               0.13878 0.065922
    ## Mastercard - Business                     -0.33601 0.032055
    ## Sky - Business                            -0.13196 0.066708
    ## Moje-Panvisto - NGO                       -0.15310 0.059577
    ## EPF - NGO                                  0.35097 0.034246
    ## Medtech - NGO                             -0.86107 0.018041
    ## BSA - Business                            -0.66605 0.017789
    ## Medical-Oncology - NGO                    -0.48668 0.055435
    ## Onfido - Business                          0.22798 0.096747
    ## Siemens-Energy - Business                 -0.45727 0.043859
    ## Future-ofLife - NGO                        0.25285 0.053709
    ## Hogan-Lovells - Business                  -0.48435 0.032679
    ## Edtech-Alliance - NGO                      2.39069 0.241356
    ## Trilateral - Business                      0.09417 0.070460
    ## Microsoft - Business                      -0.29581 0.021555
    ## Nokia - Business                          -0.10905 0.036681
    ## BVI - Business                             0.45400 0.096447
    ## Medtronic - Business                      -0.88020 0.010974
    ## Open-Future - NGO                          0.12320 0.083504
    ## Women-in-AI - NGO                          0.08550 0.039982
    ## Evangelig-Alliance - NGO                   1.65235 0.122530
    ## Infineon - Business                       -0.29832 0.069836
    ## Philips - Business                        -0.55936 0.035558
    ## Unipol - Business                         -0.54488 0.019330
    ## Impact-AI - NGO                           -0.18769 0.038486
    ## Croation-AI - NGO                          0.02700 0.128285
    ## GLEIF - NGO                               -0.20887 0.053009
    ## InkedIn - Business                         0.85983 0.158017
    ## European-AI-Forum - Business              -0.39347 0.054110
    ## Siemens-Healthineers - Business           -0.70002 0.017951
    ## Fraud-Corruption-AI - NGO                 -0.28192 0.108572
    ## Facebook - Business                        0.69483 0.042297
    ## ABB - Business                            -0.73877 0.019280
    ## Future-Society - NGO                       0.40301 0.058637
    ## IBM - Business                            -0.52838 0.027948
    ## Climate-Change-AI - NGO                    0.83024 0.102325
    ## Johner - Business                         -0.64936 0.018604
    ## Avaaz - NGO                                0.23566 0.027873
    ## ALLAI - NGO                                0.16779 0.030168
    ## Better-Finance - NGO                       0.50977 0.089900
    ## Amnesty - NGO                              0.16909 0.043323
    ## Getty-Images - Business                   -0.14424 0.063586
    ## Employment-Confederation - Business       -0.17721 0.047075
    ## Chamber-Commerce-USA - Business           -0.29324 0.033706
    ## CLAIRE-AI-Research - NGO                   0.47179 0.030540
    ## American-Insurance-Association - Business -0.53609 0.054265
    ## Fair-Trials - NGO                          0.53328 0.044672
    ## Croudstrike - Business                     0.56919 0.123048
    ## Digitalcourage - NGO                      -0.67685 0.009505
    ## AI-Austria - NGO                          -0.46648 0.058849
    ## Equinet - NGO                             -0.39308 0.031797
    ## 
    ## Estimated Feature Scores:
    ##               sazka_group submission       ec public_consultation
    ## beta -0.01686      0.1705   -0.08166 -0.08134             -0.1348
    ## psi   7.19269     -3.9200   -2.13327 -0.56470             -1.3410
    ##      artificial_intelligence  ethical legal_requirements ec_proposal
    ## beta                  -0.102  0.07147           -0.08718     0.05044
    ## psi                    1.254 -0.37654           -1.17714    -3.27454
    ##      artificial_intelligence_act       1  short      sg     one europe’s
    ## beta                     -0.8348 -0.1283  0.131  0.1709 0.04951  -0.1117
    ## psi                      -0.1431  1.3505 -2.125 -2.6213 0.64205  -1.3122
    ##      growing lottery companies primarily focused  national   games lotteries
    ## beta  0.2625  0.1706   0.05774   0.03414  0.0461  0.002023  0.4491     0.305
    ## psi  -2.3626 -3.6325   0.31538  -1.98437 -1.5141 -0.655858 -3.5582    -4.319
    ##       online secondary    focus digital gambling   leading  market
    ## beta  0.3181    0.2287 -0.06014  0.2533   0.3054 -0.001661 -0.2807
    ## psi  -1.0698   -2.7514  0.03080 -0.8532  -2.7113 -0.781681  0.8093

Creates new object and applies Lexicoder Sentiment Dictionary

``` r
toks_lsd <- toks_aiact %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
  dfm()
```

gets sentiment score

``` r
aiact_ss <- textstat_polarity(toks_lsd, data_dictionary_LSD2015)
aiact_ss$type_actor <- toks_lsd$type_actor
aiact_ss
```

    ##                                        doc_id    sentiment type_actor
    ## 1                            SAZKA - Business  0.346715604   Business
    ## 2                       Allied-Startups - NGO  0.608247737        NGO
    ## 3                       Civil-Liberties - NGO -0.100083459        NGO
    ## 4                           Google - Business  0.783613270   Business
    ## 5                                  PGEU - NGO  2.470091564        NGO
    ## 6                             SICK - Business  0.243622083   Business
    ## 7                           sanofi - Business  0.788457360   Business
    ## 8               Co-operative-Banks - Business  0.928975891   Business
    ## 9                          Siemens - Business  0.447900151   Business
    ## 10                      BlackBerry - Business  1.894174475   Business
    ## 11                   Not-for-profit-law - NGO  0.178064633        NGO
    ## 12                           Eurocities - NGO  1.122310080        NGO
    ## 13                        Hangzhou - Business  0.864062712   Business
    ## 14                  European-Disability - NGO  0.782274960        NGO
    ## 15                          EUCOPE - Business  1.275542997   Business
    ## 16                       Eurosmart - Business  0.829962648   Business
    ## 17                          Orange - Business  0.698509124   Business
    ## 18                           Bayer - Business  1.075355427   Business
    ## 19           Information-Accountability - NGO  0.728345918        NGO
    ## 20                          ResMed - Business  2.226423732   Business
    ## 21                          IDEMIA - Business  0.847297860   Business
    ## 22               Hoffmann-La-Roche - Business  0.888191821   Business
    ## 23                     Intesa-Bank - Business  0.531974448   Business
    ## 24                    Hospital-Pharmacy - NGO  0.396415273        NGO
    ## 25                   European-Radiology - NGO  1.371479275        NGO
    ## 26                          Biogen - Business  1.098612289   Business
    ## 27                          OpenAI - Business  0.946806276   Business
    ## 28                         Workday - Business  1.195462115   Business
    ## 29                                 EDRi - NGO -0.102584812        NGO
    ## 30                             SAP - Business  0.554526718   Business
    ## 31                 Democracy-Technology - NGO  0.025642431        NGO
    ## 32                            Wikimedia - NGO  0.396415273        NGO
    ## 33                              5Rights - NGO  0.521324408        NGO
    ## 34                                Thorn - NGO  0.549504478        NGO
    ## 35                             LNE - Business  0.945642944   Business
    ## 36                     AstraZeneca - Business  1.533377874   Business
    ## 37                       AlgorithmWatch - NGO  0.552957274        NGO
    ## 38                           JEITA - Business  0.450948586   Business
    ## 39                             NEC - Business  1.059281377   Business
    ## 40                        Lewiatan - Business  0.264545819   Business
    ## 41                           Glovo - Business  1.098612289   Business
    ## 42                                 BEUC - NGO  0.237269254        NGO
    ## 43                          SPLUNK - Business  0.871492589   Business
    ## 44                            ThinkTech - NGO  0.232565291        NGO
    ## 45                           MERCK - Business  1.060146008   Business
    ## 46                            BBVA - Business  0.673300884   Business
    ## 47                            SEMI - Business  1.381035804   Business
    ## 48                          Huawei - Business  1.407104361   Business
    ## 49                  Liberty-Global - Business  1.144491894   Business
    ## 50                        Mediaset - Business  0.442736503   Business
    ## 51                Renaissance-Numerique - NGO  0.602035611        NGO
    ## 52                                  ACM - NGO  1.290034363        NGO
    ## 53                             BMW - Business  1.646825445   Business
    ## 54                      Bits-of-Freedom - NGO  0.281565845        NGO
    ## 55                           Access-Now - NGO  0.048834296        NGO
    ## 56                        Novartis - Business  1.455166888   Business
    ## 57                    Arthur-Legal - Business  0.721055969   Business
    ## 58                             EON - Business  0.491594262   Business
    ## 59                         KMD-NEC - Business  0.745593656   Business
    ## 60                      SellaGroup - Business  0.450137548   Business
    ## 61                        DeepMind - Business  1.407027654   Business
    ## 62                            RELX - Business  0.708056935   Business
    ## 63                               MoveEU - NGO  1.283346392        NGO
    ## 64               Standard-Chartred - Business  0.601797402   Business
    ## 65                             CIO - Business  0.800439531   Business
    ## 66                         Fujitsu - Business  0.950389066   Business
    ## 67                  Deutsche-Börse - Business  1.118612955   Business
    ## 68                 Johnson-Johnson - Business  0.946420473   Business
    ## 69                           Intel - Business  0.599118231   Business
    ## 70                          Twilio - Business  1.077778202   Business
    ## 71                     German-Bank - Business  0.431054127   Business
    ## 72                    European-Dentists - NGO  1.782948840        NGO
    ## 73                                 AIAL - NGO  0.411507423        NGO
    ## 74                      Volkswagen - Business  1.053149915   Business
    ## 75                            Enel - Business  0.598313078   Business
    ## 76                            Zvei - Business  0.810388064   Business
    ## 77                                 CPME - NGO  0.971457113        NGO
    ## 78                         Equifax - Business  0.376206146   Business
    ## 79               Center-Data-Innovation - NGO  0.896629687        NGO
    ## 80                      Mastercard - Business  0.900599402   Business
    ## 81                             Sky - Business  1.036091932   Business
    ## 82                        Moje-Panvisto - NGO  0.779475264        NGO
    ## 83                                  EPF - NGO  1.066170267        NGO
    ## 84                              Medtech - NGO  1.015663206        NGO
    ## 85                             BSA - Business  1.201405409   Business
    ## 86                     Medical-Oncology - NGO  0.977251432        NGO
    ## 87                          Onfido - Business  1.288530192   Business
    ## 88                  Siemens-Energy - Business  0.752739278   Business
    ## 89                        Future-ofLife - NGO  0.372675285        NGO
    ## 90                   Hogan-Lovells - Business  1.018166923   Business
    ## 91                      Edtech-Alliance - NGO  1.419520009        NGO
    ## 92                      Trilateral - Business  0.858661619   Business
    ## 93                       Microsoft - Business  0.999741503   Business
    ## 94                           Nokia - Business  0.798766730   Business
    ## 95                             BVI - Business  1.142097401   Business
    ## 96                       Medtronic - Business  1.011056398   Business
    ## 97                          Open-Future - NGO  1.435084525        NGO
    ## 98                          Women-in-AI - NGO  0.431294730        NGO
    ## 99                   Evangelig-Alliance - NGO  0.272129659        NGO
    ## 100                       Infineon - Business  0.955511445   Business
    ## 101                        Philips - Business  0.408967739   Business
    ## 102                         Unipol - Business  0.529694108   Business
    ## 103                           Impact-AI - NGO  1.025049721        NGO
    ## 104                         Croation-AI - NGO  0.847297860        NGO
    ## 105                               GLEIF - NGO  0.945371941        NGO
    ## 106                        InkedIn - Business  0.912361454   Business
    ## 107              European-AI-Forum - Business  0.376949938   Business
    ## 108           Siemens-Healthineers - Business  0.409979788   Business
    ## 109                 Fraud-Corruption-AI - NGO  1.005521866        NGO
    ## 110                       Facebook - Business  0.949360048   Business
    ## 111                            ABB - Business  1.399366443   Business
    ## 112                      Future-Society - NGO  1.332619404        NGO
    ## 113                            IBM - Business  1.031513955   Business
    ## 114                   Climate-Change-AI - NGO  1.133550340        NGO
    ## 115                         Johner - Business  0.450260242   Business
    ## 116                               Avaaz - NGO  0.008403411        NGO
    ## 117                               ALLAI - NGO  0.361712738        NGO
    ## 118                      Better-Finance - NGO  0.888381944        NGO
    ## 119                             Amnesty - NGO  0.256404675        NGO
    ## 120                   Getty-Images - Business  1.370869891   Business
    ## 121       Employment-Confederation - Business  0.663294217   Business
    ## 122           Chamber-Commerce-USA - Business  0.820611821   Business
    ## 123                  CLAIRE-AI-Research - NGO  0.891304304        NGO
    ## 124 American-Insurance-Association - Business  0.729079190   Business
    ## 125                         Fair-Trials - NGO -0.091447487        NGO
    ## 126                    Croudstrike - Business  0.330563800   Business
    ## 127                      Digitalcourage - NGO -0.102584812        NGO
    ## 128                          AI-Austria - NGO  0.842892567        NGO
    ## 129                             Equinet - NGO  1.019362917        NGO

Creates new variable with only Business and NGO sentiment score to
measure mean and median

``` r
aiact_ss_business <- aiact_ss %>%
  filter(type_actor %in% c("Business"))
aiact_ss_ngo <- aiact_ss %>%
  filter(type_actor %in% c("NGO"))
mean(aiact_ss_business$sentiment)
```

    ## [1] 0.8877756

``` r
median(aiact_ss_business$sentiment)
```

    ## [1] 0.8714926

``` r
mean(aiact_ss_ngo$sentiment)
```

    ## [1] 0.7134341

``` r
median(aiact_ss_ngo$sentiment)
```

    ## [1] 0.7539106

plots the sentiment scores

``` r
ggplot(data = aiact_ss, aes(x=sentiment, y=doc_id, color = type_actor, shape = type_actor)) + geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+ labs(x="Sentiment score", y="Documents")+ theme(axis.text.y = element_text(size = 5))
```

![](sentiment_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Gets the 50 most used postitive words

``` r
toks_aiact8 <- tokens(data_corpus_aiact) %>%
tokens_keep(pattern = data_dictionary_LSD2015$positive) %>%
  dfm()
topfeatures(toks_aiact8, n=50)
```

    ##       rights intelligence       ensure       safety   protection   innovation 
    ##         1241          923          640          522          485          442 
    ##      provide         well     relevant      support        right   compliance 
    ##          363          358          332          322          297          278 
    ##      quality     learning  significant    effective       create         like 
    ##          265          261          236          222          208          200 
    ##      ethical         help   principles       credit     ensuring         best 
    ##          194          194          190          188          177          176 
    ##        trust       better     benefits     accuracy  responsible  opportunity 
    ##          166          162          160          159          155          155 
    ##      respect    competent  trustworthy         free      benefit   validation 
    ##          154          154          149          146          143          143 
    ##  coordinated      protect   understand        civil    essential     welcomes 
    ##          139          136          135          134          133          133 
    ##      welcome   accessible      achieve    providing      improve    knowledge 
    ##          132          132          131          124          123          123 
    ##     creating         open 
    ##          120          120

Gets the 50 most used negative words

``` r
toks_aiact9 <- tokens(data_corpus_aiact) %>%
tokens_keep(pattern = data_dictionary_LSD2015$negative) %>%
  dfm()
topfeatures(toks_aiact9, n=50)
```

    ##             risk       artificial            risks        oversight 
    ##             1195              903              638              259 
    ##             harm             bias       prohibited          limited 
    ##              241              226              221              215 
    ##          against      prohibition              too            avoid 
    ##              194              174              171              167 
    ##         criminal   discrimination           errors         affected 
    ##              160              156              141              133 
    ##       risk-based       challenges             lack         critical 
    ##              126              122              118              108 
    ##            harms      uncertainty        difficult          concern 
    ##              108              105              102              100 
    ##        concerned          unclear           burden     implications 
    ##               93               91               91               86 
    ##     prohibitions     unacceptable      limitations          harmful 
    ##               84               83               81               76 
    ##           biases   discriminatory         negative     insufficient 
    ##               65               64               64               63 
    ##      problematic            limit disproportionate        liability 
    ##               63               60               59               55 
    ##      unnecessary           limits        challenge          adverse 
    ##               55               54               53               52 
    ##       impossible           threat       complexity         prohibit 
    ##               51               50               50               49 
    ##         disagree            vague 
    ##               49               48
