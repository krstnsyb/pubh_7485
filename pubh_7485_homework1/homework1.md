PUBH 7485 Homework 1
================

``` r
# Read in data
load(here("data", "OPT_Study_PUBH7485_8485_2022.Rdata"))
```

### **Question 1:**

Summarize the differences between the treatment and control for the
covariates.

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
myVars <- c(
  "Clinic", "Age", "Black", "White", "Nat.Am",
  "Asian", "Hisp", "Education", "Public.Asstce", "Hypertension",
  "Diabetes", "BL.Diab.Type", "BMI", "Use.Tob", "BL.Cig.Day", "Use.Alc",
  "BL.Drks.Day", "Drug.Add", "Prev.preg", "N.prev.preg", "Live.PTB",
  "Any.stillbirth", "Spont.ab", "Induced.ab", "Any.live.ptb.sb.sp.ab.in.ab",
  "N.living.kids", "N.qualifying.teeth", "BL.GE", "BL..BOP", "BL.PD.avg",
  "BL..PD.4", "BL..PD.5", "BL.CAL.avg", "BL..CAL.2", "BL..CAL.3",
  "BL.Calc.I", "BL.Pl.I", "Birth.outcome", "Preg.ended...37.wk",
  "GA.at.outcome", "Birthweight"
)

# Vector of categorical variables that need transformation
catVars <- c(
  "Clinic", "Group", "Black", "White", "Nat.Am",
  "Asian", "Hisp", "Education", "Public.Asstce", "Hypertension",
  "Diabetes", "BL.Diab.Type", "Use.Tob", "Use.Alc", "Drug.Add", "Prev.preg", "Live.PTB",
  "Any.stillbirth", "Spont.ab", "Induced.ab", "Any.live.ptb.sb.sp.ab.in.ab", "Birth.outcome",
  "Preg.ended...37.wk"
)

# Change variable labels
opt_causal <- set_variable_labels(opt_causal,
  Clinic = "Clinic location",
  Age = "Age (y)",
  Black = "Race: Black",
  White = "Race: White",
  Nat.Am = "Race: Native American",
  Asian = "Race: Asian",
  Hisp = "Ethnicity: Hispanic",
  Education = "Level of Education",
  Public.Asstce = "Received public assistance for delivery",
  Hypertension = "Chronic hypertension at baseline",
  Diabetes = "Diabetes at baseline",
  BL.Diab.Type = "Baseline diabetes type (for those with diabetes)",
  BMI = "Body mass index",
  Use.Tob = "History of tobacco use",
  BL.Cig.Day = "# cigarettes per day",
  Use.Alc = "History of alcohol use",
  BL.Drks.Day = "# drinks per day",
  Drug.Add = "History of drug addiction",
  Prev.preg = "Any previous pregnancy",
  N.prev.preg = "# previous pregnancies",
  Live.PTB = "Previous live preterm birth",
  Any.stillbirth = "Previous stillbirth",
  Spont.ab = "Previous spontaneous abortion",
  Induced.ab = "Previous induced abortion",
  Any.live.ptb.sb.sp.ab.in.ab = "Previous live pre-term birth, stillbirth,
or spontaneous/induced abortion",
  N.living.kids = "# of living children",
  N.qualifying.teeth = "# of qualifying teeth",
  BL.GE = "Whole-mouth average gingival index",
  BL..BOP = "Fraction of sites bleeding
on probing",
  BL.PD.avg = "Whole-mouth average pocket depth",
  BL..PD.4 = "Fraction of sites with pocket depth ≥ 4mm",
  BL..PD.5 = "Fraction of sites with pocket depth ≥ 5mm",
  BL.CAL.avg = "Whole-mouth average clinical attachment level",
  BL..CAL.2 = "Fraction of sites with clinical attachment level ≥ 2 mm",
  BL..CAL.3 = "Fraction of sites with clinical attachment level ≥ 3 mm",
  BL.Calc.I = "Whole-mouth average calculus index",
  BL.Pl.I = "Whole-mouth average plaque index",
  Birth.outcome = "Birth outcome",
  Preg.ended...37.wk = "Pregnancy ended before 37 weeks",
  GA.at.outcome = "Gestational age at outcome (d)",
  Birthweight = "Birth weight (g)"
)

# Create Table 1
tab1 <- CreateTableOne(vars = myVars, strata = "Group", data = opt_causal, factorVars = catVars, test = FALSE)

tab1Mat <- print(tab1, smd = TRUE, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, varLabels = TRUE)

## Save to CSV
write.csv(tab1Mat, file = "homework1_table1.csv")
```

### **Question 2:**

Find the unadjusted average treatment effect, standard error, and 95%
confidence intervals for the outcomes of interest.

``` r
# Outcome: Pregnancy ended < 37 weeks
## ATE
## Change group to numeric to calculate proportions
opt_causal_num <- opt_causal %>%
  mutate(Preg.ended...37.wk_num = fct_recode(Preg.ended...37.wk, "0" = "No ", "1" = "Yes")) %>%
  mutate(Preg.ended...37.wk_num = as.numeric(as.character(Preg.ended...37.wk_num)))

## Unadjusted ATE
p1 <- mean(opt_causal_num$Preg.ended...37.wk_num[opt_causal_num$Group == "T"])
p0 <- mean(opt_causal_num$Preg.ended...37.wk_num[opt_causal_num$Group == "C"])
p1 - p0
```

    ## [1] -0.0327

``` r
## Standard error
n1 <- table(opt_causal_num$Group)[2]
n0 <- table(opt_causal_num$Group)[1]
SE <- sqrt(p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0)
SE
```

    ##      T 
    ## 0.0276

``` r
## 95% CI
CI_unadj <- p1 - p0 + c(-1, 1) * qnorm(0.975) * SE
print(p1 - p0 + c(-1, 1) * qnorm(0.975) * SE, digits = 3)
```

    ## [1] -0.0867  0.0213

``` r
# Outcome: Birth weight
## Unadjusted ATE
ATE <- (mean(opt_causal$Birthweight[opt_causal$Group == "T"])) - (mean(opt_causal$Birthweight[opt_causal$Group == "C"], na.rm = TRUE))

## SE
SE <- sqrt(
  (sd(opt_causal$Birthweight[opt_causal$Group == "T"])^2 / n1) +
    (sd(opt_causal$Birthweight[opt_causal$Group == "C"], na.rm = TRUE)^2 / n0)
)
SE
```

    ##    T 
    ## 55.6

``` r
## CI
opt_causal$Group_rev <- factor(opt_causal$Group, levels = rev(levels(opt_causal$Group)))

t.test(opt_causal$Birthweight ~ opt_causal$Group_rev, conf.level = 0.95, mu = 0, alternative = "two.sided", var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  opt_causal$Birthweight by opt_causal$Group_rev
    ## t = 1, df = 442, p-value = 0.2
    ## alternative hypothesis: true difference in means between group T and group C is not equal to 0
    ## 95 percent confidence interval:
    ##  -31.2 187.9
    ## sample estimates:
    ## mean in group T mean in group C 
    ##            3259            3181

## **Question 3:**

Estimate the causal treatment effect using regression adjustment.

``` r
# Function to impute missing values in continuous variables
imp_med <- function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
}

# Functions to impute missing values in categorical variables
calc_mode <- function(x) {
  distinct_values <- unique(x)
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}

imp_cat <- function(x) {
  if_else(is.na(x), calc_mode(x), x)
}

# Create vector of continuous variables
cont_vars <- c(
  "Age", "BMI", "BL.Cig.Day", "BL.Drks.Day", "N.prev.preg", "N.living.kids",
  "N.qualifying.teeth", "BL.GE", "BL..BOP", "BL.PD.avg",
  "BL..PD.4", "BL..PD.5", "BL.CAL.avg", "BL..CAL.2", "BL..CAL.3",
  "BL.Calc.I", "BL.Pl.I", "Birthweight"
)

# Create vector of categorical variables
cat_vars <- c(
  "Clinic", "Black", "White", "Nat.Am", "Asian", "Hisp", "Education",
  "Public.Asstce", "Hypertension", "Diabetes", "BL.Diab.Type", "Use.Tob",
  "Use.Alc", "Drug.Add", "Prev.preg", "Live.PTB", "Any.stillbirth", "Spont.ab",
  "Induced.ab", "Any.live.ptb.sb.sp.ab.in.ab", "Preg.ended...37.wk"
)

# Impute median for continuous variables
for (i in cont_vars) {
  opt_causal[[i]] <- imp_med(opt_causal[[i]])
}

# Impute most common category for continuous variables
for (i in cat_vars) {
  opt_causal[[i]] <- imp_cat(opt_causal[[i]])
}

# Check for missing data
opt_causal %>%
  is.na() %>%
  colSums()
```

    ##                         PID                      Clinic 
    ##                           0                           0 
    ##                       Group                         Age 
    ##                           0                           0 
    ##                       Black                       White 
    ##                           0                           0 
    ##                      Nat.Am                       Asian 
    ##                           0                           0 
    ##                        Hisp                   Education 
    ##                           0                           0 
    ##               Public.Asstce                Hypertension 
    ##                           0                           0 
    ##                    Diabetes                BL.Diab.Type 
    ##                           0                           0 
    ##                         BMI                     Use.Tob 
    ##                           0                           0 
    ##                  BL.Cig.Day                     Use.Alc 
    ##                           0                           0 
    ##                 BL.Drks.Day                    Drug.Add 
    ##                           0                           0 
    ##                   Prev.preg                 N.prev.preg 
    ##                           0                           0 
    ##                    Live.PTB              Any.stillbirth 
    ##                           0                           0 
    ##                    Spont.ab                  Induced.ab 
    ##                           0                           0 
    ## Any.live.ptb.sb.sp.ab.in.ab               N.living.kids 
    ##                           0                           0 
    ##          N.qualifying.teeth                       BL.GE 
    ##                           0                           0 
    ##                     BL..BOP                   BL.PD.avg 
    ##                           0                           0 
    ##                    BL..PD.4                    BL..PD.5 
    ##                           0                           0 
    ##                  BL.CAL.avg                   BL..CAL.2 
    ##                           0                           0 
    ##                   BL..CAL.3                   BL.Calc.I 
    ##                           0                           0 
    ##                     BL.Pl.I               Birth.outcome 
    ##                           0                           0 
    ##          Preg.ended...37.wk               GA.at.outcome 
    ##                           0                           0 
    ##                 Birthweight                  Preg.ended 
    ##                           0                           0 
    ##                   Group_rev 
    ##                           0

### **3.a:**

Regression models with main effect terms for the covariates and
treatment.

``` r
model1_preg <- glm(Preg.ended...37.wk ~ Group + Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I, data = opt_causal, family = binomial(link = "logit"))

# Set up for ATE, SE, CI
data_trt <- data_ctr <- opt_causal
data_trt$Group <- "T"
data_ctr$Group <- "C"
pred1 <- predict(model1_preg, newdata = data_trt, type = "response")
pred0 <- predict(model1_preg, newdata = data_ctr, type = "response")
ATE <- mean(pred1 - pred0)
set.seed(100)
B <- 100
ATE_boot <- NULL
n <- nrow(opt_causal)

# Bootstrap
for (i in 1:B) {
  opt_causal_boot <- opt_causal[sample(1:n, n, replace = TRUE), ]
  model1_preg_boot <- glm(Preg.ended...37.wk ~ Group + Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I, data = opt_causal_boot, family = binomial(link = "logit"))
  data_trt_boot <- opt_causal_boot
  data_trt_boot$Group <- "T"
  data_ctr_boot <- opt_causal_boot
  data_ctr_boot$Group <- "C"
  pred1_boot <- predict(model1_preg_boot,
    newdata = data_trt_boot,
    type = "response"
  )
  pred0_boot <- predict(model1_preg_boot,
    newdata = data_ctr_boot,
    type = "response"
  )
  ATE_boot <- c(ATE_boot, mean(pred1_boot - pred0_boot))
}

# Function to print results
get_results <- function() {
  print("Average Treatment Effect")
  print(ATE, digits = 3)
  SE <- sd(ATE_boot)
  print("Bootstrap SE")
  print(SE, digits = 3)
  print("Bootstrap Normal 95% CI")
  CI <- ATE + c(-1, 1) * qnorm(0.975) * SE
  print(ATE + c(-1, 1) * qnorm(0.975) * SE, digits = 3)
}

get_results()
```

    ## [1] "Average Treatment Effect"
    ## [1] -0.0315
    ## [1] "Bootstrap SE"
    ## [1] 0.0305
    ## [1] "Bootstrap Normal 95% CI"
    ## [1] -0.0912  0.0283

``` r
# Create main effects model for Birth weight outcome
model1_bw <- glm(Birthweight ~ Group + Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I, data = opt_causal)

# Set up for ATE, SE, CI
data_trt <- data_ctr <- opt_causal
data_trt$Group <- "T"
data_ctr$Group <- "C"
pred1 <- predict(model1_bw, newdata = data_trt, type = "response")
pred0 <- predict(model1_bw, newdata = data_ctr, type = "response")
ATE <- mean(pred1 - pred0)
set.seed(100)
B <- 100
ATE_boot <- NULL
n <- nrow(opt_causal)

# Bootstrap
for (i in 1:B) {
  opt_causal_boot <- opt_causal[sample(1:n, n, replace = TRUE), ]
  model1_bw_boot <- glm(Birthweight ~ Group + Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I, data = opt_causal_boot)
  data_trt_boot <- opt_causal_boot
  data_trt_boot$Group <- "T"
  data_ctr_boot <- opt_causal_boot
  data_ctr_boot$Group <- "C"
  pred1_boot <- predict(model1_bw_boot,
    newdata = data_trt_boot,
    type = "response"
  )
  pred0_boot <- predict(model1_bw_boot,
    newdata = data_ctr_boot,
    type = "response"
  )
  ATE_boot <- c(ATE_boot, mean(pred1_boot - pred0_boot))
}

get_results()
```

    ## [1] "Average Treatment Effect"
    ## [1] 91.9
    ## [1] "Bootstrap SE"
    ## [1] 54
    ## [1] "Bootstrap Normal 95% CI"
    ## [1] -14.1 197.8

### **3.b**

Add in nonlinear terms for the continuous covariates but no
interactions.

``` r
# Pregnancy outcome
model2_preg <- glm(Preg.ended...37.wk ~ Group + Clinic + I(Age^2) + Black + White + Nat.Am + Education + Public.Asstce + I(BMI^2) + Hypertension + Diabetes + I(BL.Cig.Day^2) + I(BL.Drks.Day^2) + I(N.prev.preg^2) + Any.live.ptb.sb.sp.ab.in.ab + I(N.qualifying.teeth^2) + I(BL.GE^2) + I(BL..BOP^2) + I(BL.PD.avg^2) + I(BL.CAL.avg^2) + I(BL.Calc.I^2) + I(BL.Pl.I^2), data = opt_causal, family = binomial(link = "logit"))

# Function for bootstrap
# Set up for ATE, SE, CI
data_trt <- data_ctr <- opt_causal
data_trt$Group <- "T"
data_ctr$Group <- "C"
pred1 <- predict(model2_preg, newdata = data_trt, type = "response")
pred0 <- predict(model2_preg, newdata = data_ctr, type = "response")
ATE <- mean(pred1 - pred0)
set.seed(100)
B <- 100
ATE_boot <- NULL
n <- nrow(opt_causal)

# Bootstrap
for (i in 1:B) {
  opt_causal_boot <- opt_causal[sample(1:n, n, replace = TRUE), ]
  model2_preg_boot <- glm(Preg.ended...37.wk ~ Group + Clinic + I(Age^2) + Black + White + Nat.Am + Education + Public.Asstce + I(BMI^2) + Hypertension + Diabetes + I(BL.Cig.Day^2) + I(BL.Drks.Day^2) + I(N.prev.preg^2) + Any.live.ptb.sb.sp.ab.in.ab + I(N.qualifying.teeth^2) + I(BL.GE^2) + I(BL..BOP^2) + I(BL.PD.avg^2) + I(BL.CAL.avg^2) + I(BL.Calc.I^2) + I(BL.Pl.I^2), data = opt_causal_boot, family = binomial(link = "logit"))
  data_trt_boot <- opt_causal_boot
  data_trt_boot$Group <- "T"
  data_ctr_boot <- opt_causal_boot
  data_ctr_boot$Group <- "C"
  pred1_boot <- predict(model2_preg_boot,
    newdata = data_trt_boot,
    type = "response"
  )
  pred0_boot <- predict(model2_preg_boot,
    newdata = data_ctr_boot,
    type = "response"
  )
  ATE_boot <- c(ATE_boot, mean(pred1_boot - pred0_boot))
}

get_results()
```

    ## [1] "Average Treatment Effect"
    ## [1] -0.033
    ## [1] "Bootstrap SE"
    ## [1] 0.0306
    ## [1] "Bootstrap Normal 95% CI"
    ## [1] -0.093  0.027

``` r
# Birth weight outcome
model2_bw <- glm(Birthweight ~ Group + Clinic + I(Age^2) + Black + White + Nat.Am + Education + Public.Asstce + I(BMI^2) + Hypertension + Diabetes + I(BL.Cig.Day^2) + I(BL.Drks.Day^2) + I(N.prev.preg^2) + Any.live.ptb.sb.sp.ab.in.ab + I(N.qualifying.teeth^2) + I(BL.GE^2) + I(BL..BOP^2) + I(BL.PD.avg^2) + I(BL.CAL.avg^2) + I(BL.Calc.I^2) + I(BL.Pl.I^2), data = opt_causal)

# Function for bootstrap
# Set up for ATE, SE, CI
data_trt <- data_ctr <- opt_causal
data_trt$Group <- "T"
data_ctr$Group <- "C"
pred1 <- predict(model2_bw, newdata = data_trt, type = "response")
pred0 <- predict(model2_bw, newdata = data_ctr, type = "response")
ATE <- mean(pred1 - pred0)
set.seed(100)
B <- 100
ATE_boot <- NULL
n <- nrow(opt_causal)

# Bootstrap
for (i in 1:B) {
  opt_causal_boot <- opt_causal[sample(1:n, n, replace = TRUE), ]
  model2_bw_boot <- glm(Birthweight ~ Group + Clinic + I(Age^2) + Black + White + Nat.Am + Education + Public.Asstce + I(BMI^2) + Hypertension + Diabetes + I(BL.Cig.Day^2) + I(BL.Drks.Day^2) + I(N.prev.preg^2) + Any.live.ptb.sb.sp.ab.in.ab + I(N.qualifying.teeth^2) + I(BL.GE^2) + I(BL..BOP^2) + I(BL.PD.avg^2) + I(BL.CAL.avg^2) + I(BL.Calc.I^2) + I(BL.Pl.I^2), data = opt_causal_boot)
  data_trt_boot <- opt_causal_boot
  data_trt_boot$Group <- "T"
  data_ctr_boot <- opt_causal_boot
  data_ctr_boot$Group <- "C"
  pred1_boot <- predict(model2_bw_boot,
    newdata = data_trt_boot,
    type = "response"
  )
  pred0_boot <- predict(model2_bw_boot,
    newdata = data_ctr_boot,
    type = "response"
  )
  ATE_boot <- c(ATE_boot, mean(pred1_boot - pred0_boot))
}

get_results()
```

    ## [1] "Average Treatment Effect"
    ## [1] 92.8
    ## [1] "Bootstrap SE"
    ## [1] 55.7
    ## [1] "Bootstrap Normal 95% CI"
    ## [1] -16.3 201.9

### **3.c**

Regression model with interactions between the covariates and treatment;
no nonlinear terms.

``` r
# Create interaction model for pregnancy outcome
model3_preg <- glm(Preg.ended...37.wk ~ Group * (Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I), data = opt_causal, family = binomial(link = "logit"))

# Set up for ATE, SE, CI
data_trt <- data_ctr <- opt_causal
data_trt$Group <- "T"
data_ctr$Group <- "C"
pred1 <- predict(model3_preg, newdata = data_trt, type = "response")
pred0 <- predict(model3_preg, newdata = data_ctr, type = "response")
ATE <- mean(pred1 - pred0)
set.seed(100)
B <- 100
ATE_boot <- NULL
n <- nrow(opt_causal)

# Bootstrap
for (i in 1:B) {
  opt_causal_boot <- opt_causal[sample(1:n, n, replace = TRUE), ]
  model3_preg_boot <- glm(Preg.ended...37.wk ~ Group + Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I, data = opt_causal_boot, family = binomial(link = "logit"))
  data_trt_boot <- opt_causal_boot
  data_trt_boot$Group <- "T"
  data_ctr_boot <- opt_causal_boot
  data_ctr_boot$Group <- "C"
  pred1_boot <- predict(model3_preg_boot,
    newdata = data_trt_boot,
    type = "response"
  )
  pred0_boot <- predict(model3_preg_boot,
    newdata = data_ctr_boot,
    type = "response"
  )
  ATE_boot <- c(ATE_boot, mean(pred1_boot - pred0_boot))
}

get_results()
```

    ## [1] "Average Treatment Effect"
    ## [1] -0.0265
    ## [1] "Bootstrap SE"
    ## [1] 0.0305
    ## [1] "Bootstrap Normal 95% CI"
    ## [1] -0.0863  0.0332

``` r
# Create interaction model for birth weight outcome
model3_bw <- glm(Birthweight ~ Group * (Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I), data = opt_causal)

# Set up for ATE, SE, CI
data_trt <- data_ctr <- opt_causal
data_trt$Group <- "T"
data_ctr$Group <- "C"
pred1 <- predict(model3_bw, newdata = data_trt, type = "response")
pred0 <- predict(model3_bw, newdata = data_ctr, type = "response")
ATE <- mean(pred1 - pred0)
set.seed(100)
B <- 100
ATE_boot <- NULL
n <- nrow(opt_causal)

# Bootstrap
for (i in 1:B) {
  opt_causal_boot <- opt_causal[sample(1:n, n, replace = TRUE), ]
  model3_bw_boot <- glm(Birthweight ~ Group * (Clinic + Age + Black + White + Nat.Am + Education + Public.Asstce + BMI + Hypertension + Diabetes + BL.Cig.Day + BL.Drks.Day + N.prev.preg + Any.live.ptb.sb.sp.ab.in.ab + N.qualifying.teeth + BL.GE + BL..BOP + BL.PD.avg + BL.CAL.avg + BL.Calc.I + BL.Pl.I), data = opt_causal_boot)
  data_trt_boot <- opt_causal_boot
  data_trt_boot$Group <- "T"
  data_ctr_boot <- opt_causal_boot
  data_ctr_boot$Group <- "C"
  pred1_boot <- predict(model3_bw_boot,
    newdata = data_trt_boot,
    type = "response"
  )
  pred0_boot <- predict(model3_bw_boot,
    newdata = data_ctr_boot,
    type = "response"
  )
  ATE_boot <- c(ATE_boot, mean(pred1_boot - pred0_boot))
}

get_results()
```

    ## [1] "Average Treatment Effect"
    ## [1] 76.1
    ## [1] "Bootstrap SE"
    ## [1] 60.7
    ## [1] "Bootstrap Normal 95% CI"
    ## [1] -43 195
