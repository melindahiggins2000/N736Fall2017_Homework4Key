﻿* Encoding: UTF-8.
* ==========================================.
* N736 HOMEWORK 04 - Answer Key
*
* by Melinda Higgins, PhD
* updated 10/31/2018
* ==========================================.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT cesd
  /METHOD=ENTER indtot
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)
  /CASEWISE PLOT(ZRESID) OUTLIERS(3)
  /SAVE PRED ZPRED COOK RESID ZRESID.

GRAPH 
  /SCATTERPLOT(BIVAR)=ZPR_1 WITH ZRE_1 
  /MISSING=LISTWISE.

GRAPH 
  /SCATTERPLOT(BIVAR)=id WITH COO_1 
  /MISSING=LISTWISE.

GRAPH 
  /SCATTERPLOT(BIVAR)=id WITH ZRE_1 
  /MISSING=LISTWISE.

GRAPH 
  /SCATTERPLOT(BIVAR)=indtot WITH cesd 
  /MISSING=LISTWISE.

* recode character race into numeric coded variable.
AUTORECODE VARIABLES=racegrp 
  /INTO racenum 
  /PRINT.

* optional - it is ok to keep all 4 races.
RECODE racenum (1=1) (4=4) (2 thru 3=2) INTO race3.
EXECUTE.

UNIANOVA cesd BY racenum
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=racenum(SIDAK) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(racenum) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=racenum.

GRAPH
  /ERRORBAR(CI 95)=cesd BY racenum.

GRAPH
  /ERRORBAR(STDDEV 1)=cesd BY racenum.
