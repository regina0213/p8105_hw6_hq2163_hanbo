p8105\_hw6\_hq2163\_hanbo
================
Hanbo Qiu
November 19, 2018

Problem 1
---------

**Import a dataset and make some alternation on some variables:**

``` r
homicides = read_csv(file = "./homicide-data.csv") %>% 
   mutate(city_state = str_c(city, ", ", state),
        resolved = as.numeric(disposition == "Closed by arrest"),
        victim_age = as.numeric(victim_age),
        victim_race = ifelse(victim_race == "White", "white", "non-white"), 
        victim_race = relevel(factor(victim_race),ref = "white")) %>% 
   filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))
## Parsed with column specification:
## cols(
##   uid = col_character(),
##   reported_date = col_integer(),
##   victim_last = col_character(),
##   victim_first = col_character(),
##   victim_race = col_character(),
##   victim_age = col_character(),
##   victim_sex = col_character(),
##   city = col_character(),
##   state = col_character(),
##   lat = col_double(),
##   lon = col_double(),
##   disposition = col_character()
## )
## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
## coercion
```

Create a city\_state variable (e.g. “Baltimore, MD”), and a binary variable indicating whether the homicide is solved. Omit cities Dallas, TX; Phoenix, AZ; and Kansas City, MO and Tulsa, AL. Modifiy victim\_race to have categories white and non-white, with white as the reference category. Modify victim\_age as numeric.

**Fit the model of "Baltimore, MD" :**

``` r
baltimore_df = homicides %>% 
  filter(city_state == "Baltimore, MD" )
fit_logistic = 
  glm(resolved ~ victim_age + victim_sex + victim_race, 
    data = baltimore_df, 
    family = binomial()) 

  confi_interval=confint(fit_logistic) %>% as.data.frame() %>% 
  tail(1) %>% 
  gather(key = CI, value = value ) %>% 
  mutate(CI_OR = exp(value))
## Waiting for profiling to be done...
  
  broom::tidy(fit_logistic) %>% 
  mutate(OR = exp(estimate)) %>% 
  tail(1) %>% 
  select(estimate, OR) %>% 
  knitr::kable(digits = 3)
```

|  estimate|     OR|
|---------:|------:|
|     -0.82|  0.441|

We fitted a logistic regression with resolved vs unresolved as the outcome and victim age, sex and race as predictors for Baltimore, MD. The estimate for solving homicides comparing non-white victims to white victims keeping all other variables fixed is 0.441. The lower level confidence interval of the adjusted odds ratio of is 0.312, the upper level confidence interval is 0.62.

**Fit the model each of the cities :**

``` r
nest_lm_res =
  homicides %>% 
  group_by(city_state) %>% 
  nest() %>% 
  mutate(models = map(data, ~glm(resolved ~ victim_age + victim_sex + victim_race, data = .x, family = binomial())),
         ci = map(.x=models, ~confint(.x)),
         or = map(models, broom::tidy),
         ci = map(ci, broom::tidy)) %>% 
  select(-data,-models) %>% 
  unnest() %>% 
  filter(term == "victim_racenon-white") %>% 
  select(city_state, or = estimate, ci_low = "X2.5..", ci_high = "X97.5..") %>% 
  mutate(or = exp(or), ci_low = exp(ci_low), ci_high = exp(ci_high))
```

**Create a plot that shows the estimated ORs and CIs for each city. Organize cities according to estimated OR, and comment on the plot:**

``` r
nest_lm_res %>% 
  mutate(city_state = fct_reorder(city_state, or)) %>% 
  ggplot(aes(x = city_state, y = or)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) + 
  labs(
    title = "The estimated ORs and CIs for each city",
    x = "City",
    y = "The estimated ORs and CIs",
    caption = "Data from The Washington Post"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="p8105_hw6_hq2163_hanbo_files/figure-markdown_github/unnamed-chunk-4-1.png" width="90%" />

This figure suggests a very wide range in odds ratio (and CI) for solving homicides comparing non-white victims to white victims. -- Tampa, FL is noticeably high and, given the width of the CI, meaning that for every increasing non-white victims comparing to white victims, the OR for solving homicides is higest.

Problem 2
---------

**Import a dataset and make some alternation on some variables:**

``` r
birth_wt = read_csv(file = "./birthweight.csv") %>% 
  mutate(babysex = factor(babysex, labels = c("Male", "Female")),
         frace = factor(frace, levels = c(1, 2 ,3 ,4 ,8 ,9),labels = c("White", "Black", "Asian", "Puerto Rican", "Other", "Unknown")),
         mrace = factor(mrace, levels = c(1, 2 ,3 ,4 ,8),labels = c("White", "Black", "Asian", "Puerto Rican", "Other")),
         malform = factor(malform, labels = c("absent", "present")),
         fincome = fincome * 100) %>% 
  select(bwt, everything())
## Parsed with column specification:
## cols(
##   .default = col_integer(),
##   gaweeks = col_double(),
##   ppbmi = col_double(),
##   smoken = col_double()
## )
## See spec(...) for full column specifications.

apply(birth_wt, 2, function(x){sum(is.na(x))})
##      bwt  babysex    bhead  blength    delwt  fincome    frace  gaweeks 
##        0        0        0        0        0        0        0        0 
##  malform menarche  mheight   momage    mrace   parity  pnumlbw  pnumsga 
##        0        0        0        0        0        0        0        0 
##    ppbmi     ppwt   smoken   wtgain 
##        0        0        0        0
```

Convert some numeric variable to factor. There is no missing value for all variables.

**Use Stepwise regreession methods to build model.**

``` r
mult.fit <- lm(bwt ~ ., data=birth_wt)
step(mult.fit, direction='backward')
## Start:  AIC=48717.83
## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
##     malform + menarche + mheight + momage + mrace + parity + 
##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain
## 
## 
## Step:  AIC=48717.83
## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
##     malform + menarche + mheight + momage + mrace + parity + 
##     pnumlbw + pnumsga + ppbmi + ppwt + smoken
## 
## 
## Step:  AIC=48717.83
## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
##     malform + menarche + mheight + momage + mrace + parity + 
##     pnumlbw + ppbmi + ppwt + smoken
## 
## 
## Step:  AIC=48717.83
## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
##     malform + menarche + mheight + momage + mrace + parity + 
##     ppbmi + ppwt + smoken
## 
##            Df Sum of Sq       RSS   AIC
## - frace     4    124365 320848704 48712
## - malform   1      1419 320725757 48716
## - ppbmi     1      6346 320730684 48716
## - momage    1     28661 320752999 48716
## - mheight   1     66886 320791224 48717
## - menarche  1    111679 320836018 48717
## - ppwt      1    131132 320855470 48718
## <none>                  320724338 48718
## - fincome   1    193454 320917792 48718
## - parity    1    413584 321137922 48721
## - mrace     3    868321 321592659 48724
## - babysex   1    853796 321578134 48727
## - gaweeks   1   4611823 325336161 48778
## - smoken    1   5076393 325800732 48784
## - delwt     1   8008891 328733230 48823
## - blength   1 102050296 422774634 49915
## - bhead     1 106535716 427260054 49961
## 
## Step:  AIC=48711.51
## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
##     malform + menarche + mheight + momage + mrace + parity + 
##     ppbmi + ppwt + smoken
## 
##            Df Sum of Sq       RSS   AIC
## - malform   1      1447 320850151 48710
## - ppbmi     1      6975 320855679 48710
## - momage    1     28379 320877083 48710
## - mheight   1     69502 320918206 48710
## - menarche  1    115708 320964411 48711
## - ppwt      1    133961 320982665 48711
## <none>                  320848704 48712
## - fincome   1    194405 321043108 48712
## - parity    1    414687 321263390 48715
## - babysex   1    852133 321700837 48721
## - gaweeks   1   4625208 325473911 48772
## - smoken    1   5036389 325885093 48777
## - delwt     1   8013099 328861802 48817
## - mrace     3  13540415 334389119 48885
## - blength   1 101995688 422844392 49908
## - bhead     1 106662962 427511666 49956
## 
## Step:  AIC=48709.53
## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
##     menarche + mheight + momage + mrace + parity + ppbmi + ppwt + 
##     smoken
## 
##            Df Sum of Sq       RSS   AIC
## - ppbmi     1      6928 320857079 48708
## - momage    1     28660 320878811 48708
## - mheight   1     69320 320919470 48708
## - menarche  1    116027 320966177 48709
## - ppwt      1    133894 320984044 48709
## <none>                  320850151 48710
## - fincome   1    193784 321043934 48710
## - parity    1    414482 321264633 48713
## - babysex   1    851279 321701430 48719
## - gaweeks   1   4624003 325474154 48770
## - smoken    1   5035195 325885346 48775
## - delwt     1   8029079 328879230 48815
## - mrace     3  13553320 334403471 48883
## - blength   1 102009225 422859375 49906
## - bhead     1 106675331 427525481 49954
## 
## Step:  AIC=48707.63
## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
##     menarche + mheight + momage + mrace + parity + ppwt + smoken
## 
##            Df Sum of Sq       RSS   AIC
## - momage    1     29211 320886290 48706
## - menarche  1    117635 320974714 48707
## <none>                  320857079 48708
## - fincome   1    195199 321052278 48708
## - parity    1    412984 321270064 48711
## - babysex   1    850020 321707099 48717
## - mheight   1   1078673 321935752 48720
## - ppwt      1   2934023 323791103 48745
## - gaweeks   1   4621504 325478583 48768
## - smoken    1   5039368 325896447 48773
## - delwt     1   8024939 328882018 48813
## - mrace     3  13551444 334408523 48881
## - blength   1 102018559 422875638 49904
## - bhead     1 106821342 427678421 49953
## 
## Step:  AIC=48706.02
## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
##     menarche + mheight + mrace + parity + ppwt + smoken
## 
##            Df Sum of Sq       RSS   AIC
## - menarche  1    100121 320986412 48705
## <none>                  320886290 48706
## - fincome   1    240800 321127090 48707
## - parity    1    431433 321317724 48710
## - babysex   1    841278 321727568 48715
## - mheight   1   1076739 321963029 48719
## - ppwt      1   2913653 323799943 48743
## - gaweeks   1   4676469 325562760 48767
## - smoken    1   5045104 325931394 48772
## - delwt     1   8000672 328886962 48811
## - mrace     3  14667730 335554021 48894
## - blength   1 101990556 422876847 49902
## - bhead     1 106864308 427750598 49952
## 
## Step:  AIC=48705.38
## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
##     mheight + mrace + parity + ppwt + smoken
## 
##           Df Sum of Sq       RSS   AIC
## <none>                 320986412 48705
## - fincome  1    245637 321232048 48707
## - parity   1    422770 321409181 48709
## - babysex  1    846134 321832545 48715
## - mheight  1   1012240 321998651 48717
## - ppwt     1   2907049 323893461 48743
## - gaweeks  1   4662501 325648912 48766
## - smoken   1   5073849 326060260 48771
## - delwt    1   8137459 329123871 48812
## - mrace    3  14683609 335670021 48894
## - blength  1 102191779 423178191 49903
## - bhead    1 106779754 427766166 49950
## 
## Call:
## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
##     gaweeks + mheight + mrace + parity + ppwt + smoken, data = birth_wt)
## 
## Coefficients:
##       (Intercept)      babysexFemale              bhead  
##        -6.099e+03          2.856e+01          1.308e+02  
##           blength              delwt            fincome  
##         7.495e+01          4.107e+00          3.180e-03  
##           gaweeks            mheight         mraceBlack  
##         1.159e+01          6.594e+00         -1.388e+02  
##        mraceAsian  mracePuerto Rican             parity  
##        -7.489e+01         -1.007e+02          9.630e+01  
##              ppwt             smoken  
##        -2.676e+00         -4.843e+00
```

'Step' function uses AIC criterion for variable selection and the default option is 'backward'. As a result, it reported a reasonable linear model with variables of babysex, bhead, blength, delwt, fincome, gaweeks, mheight, mrace, parity, ppwt and smoken.

**show a plot of model residuals against fitted values**

``` r
fit_1 = lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + mheight + mrace + parity + ppwt + smoken, data = birth_wt)

birth_wt%>% 
  modelr::add_predictions(fit_1) %>% 
  modelr::add_residuals(fit_1) %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_hex() +
  geom_hline(aes(yintercept=0), colour="red")
```

<img src="p8105_hw6_hq2163_hanbo_files/figure-markdown_github/unnamed-chunk-7-1.png" width="90%" />

**Compare different models**

``` r
cv_df = 
  crossv_mc(birth_wt, 100) %>% 
  mutate(train = map(train, as_tibble),
         teat = map(test, as_tibble))

cv_df = 
  cv_df %>% 
  mutate(lin_1    = map(train, ~lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + mheight + mrace + parity + ppwt + smoken, data = .x)),
         lin_2    = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
         nonlin_2 = map(train, ~gam(bwt ~ s(blength) + s(gaweeks), data = .x)),
         lin_3    = map(train, ~lm(bwt ~ bhead * blength * babysex, data = .x)),
         nonlin_3 = map(train, ~gam(bwt ~ te(bhead, blength, by = babysex), data = .x))) %>% 
  mutate(rmse_lin_1    = map2_dbl(lin_1, test, ~rmse(model = .x, data = .y)),
         rmse_lin_2    = map2_dbl(lin_2 , test, ~rmse(model = .x, data = .y)),
         rmse_nonlin_2 = map2_dbl(nonlin_2, test, ~rmse(model = .x, data = .y)),
         rmse_lin_3    = map2_dbl(lin_3 , test, ~rmse(model = .x, data = .y)),
         rmse_nonlin_3 = map2_dbl(nonlin_3 , test, ~rmse(model = .x, data = .y)))
```

**Compare different models by rmse**

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

<img src="p8105_hw6_hq2163_hanbo_files/figure-markdown_github/unnamed-chunk-9-1.png" width="90%" />

When we applied the cross-validated prediction error to compare these models, we found that model lin\_1 which was built based on the Stepwise regreession methods have the leaset residuls,meaning fitting the best. model 2 is the one using length at birth and gestational age as predictors. We used both linear and nonlinear to fit it. Both of them have large residuals.The model using head circumference, length, sex, and all interactions (including the three-way interaction) seems to have acceptable risiduals, but the CI of risidual is too wide which means a large deviation of predication.
