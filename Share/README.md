Obtaining the population median trajectory

The following code loads and plots the population median (50th-centile) trajectory of pars opercularis surface area lateralization across the lifespan.

source("100.common-variables.r")
source("101.common-functions.r")

source("300.variables.r")
source("301.functions.r")

FIT <- readRDS("Share/FIT_Chinese_SA_parsopercularis.rds")

POP.CURVE.LIST <- list(
  AgeTransformed = seq(
    log(280),
    log(365 * 100 + 280),
    length.out = 2^10
  ),
  Sex = factor(
    c("Female", "Male"),
    levels = c("Female", "Male")
  )
)

desired_ticks <- c(0, 1, 2, 18, 35, 80)

tick_positions <- log((desired_ticks * 365) + 280)

POP.CURVE.RAW <- do.call(
  what = expand.grid,
  args = POP.CURVE.LIST
)

CURVE <- Apply.Param(
  NEWData = POP.CURVE.RAW,
  FITParam = FIT$param
)

female_curve <- CURVE[CURVE$Sex == "Female", ]
female_curve <- female_curve[order(female_curve$AgeTransformed), ]

plot(
  PRED.m500.pop ~ AgeTransformed,
  data = female_curve,
  type = "l",
  xaxt = "n",
  xlab = "Age (years)"
)

axis(
  side = 1,
  at = tick_positions,
  labels = desired_ticks,
  cex.axis = 1.5
)
