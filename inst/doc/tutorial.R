## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE--------------------------------------------------------
#  if(!require("remotes")) { install.packages("remotes") }
#  remotes::install_github(repo = "KWB-R/kwb.hantush", dependencies = TRUE)

## ------------------------------------------------------------------------
library(kwb.hantush)

## ------------------------------------------------------------------------
### Comparision of R results to all 
### eight benchmark models in one plot
plotModelComparison()

### Comparision of R results to all 
### eight benchmark models in multiple plots (one plot for each model)
plotModelComparison(layout=c(1,1))

## ------------------------------------------------------------------------
baseProps <- baseProperties( time = 2^(0:6), ## day, for 6 different times !
                             infiltrationRate = 1, ## meter / day
                             basinWidth = 10, ## meter
                             basinLength = 50, ## meter
                             horizConductivity = 10, ## meter / day
                             iniHead = 10, ## meter
                             specificYield = 0.2)

## ------------------------------------------------------------------------
res <- hantushDistancesBaseProps(baseProps = baseProps)

## ------------------------------------------------------------------------
cols <- length(unique(res$dat[[res$changedBaseProp.Name]]))
mainTxt <- sprintf("Changed baseProperty: %s", res$changedBaseProp.Name)
xyplot(WLincrease ~ x,
       groups=res$dat[[res$changedBaseProp.Name]],
       data=res$dat,
       type="b",
       auto.key=list(columns=cols),
       main=mainTxt)


