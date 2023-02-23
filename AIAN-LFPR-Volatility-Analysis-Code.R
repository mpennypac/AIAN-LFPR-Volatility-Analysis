
"## read in our data
dataset1 = read.csv('earnings-income-by-race.csv')
head(dataset1)
## from this dataset all I need is the Native.Lands column and geoid, since some data
## is a bit weird in it and the other dataset that has better (more consistent) entries
## doesn't include the native lands column, so we gotta merge em
land_names = dataset1$Native.Lands
GEOID = dataset1$GEOID
MeanIncomePerCapita = dataset1$MeanIncomePerCapita
MeanIncomePerCapita[MeanIncomePerCapita=='-'] = NA
MeanIncomePerCapita[MeanIncomePerCapita=='N'] = NA

dataset1 = cbind(land_names, GEOID, MeanIncomePerCapita)
head(dataset1)

dataset2 = read.csv('povertyraceemploymentage.csv')
head(dataset2)
## now just merge the datasets
dataset2 = merge(dataset2, dataset1, by='GEOID')
head(dataset2)

## Now I have to eliminate any row where land_names doesn't include a state abbreviation,
## then merge the rows by state... but let's just worry about the row deletion for now

dataset2 = dataset2[grep('AL|AK|AZ|CA|CO|CT|DE|DC|FL|GA|HI|ID|IL|IN|IA|KS|KY|LA|ME|MD|MA|MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|OH|OK|OR|PA|RI|SC|SD|TN|TX|UT|VT|VA|WA|WV|WI|WY',dataset2$land_names),]
head(dataset2$land_names)
## Well... I'm a bit of a bozo! Apparently they've all got a state abbrevation! So just gotta group them.
## So what I can do is just take the last two letters of each land_names entry and put them in a new column!
## Nice.
library(stringr)
dataset2$state_abb1 = str_sub(dataset2$land_names,start=-10)
if (dataset2$state_abb1[3] != '-') {
  dataset2$state_abb1 = NA
}
dataset2$state_abb2 = str_sub(dataset2$land_names,start=-6)
dataset2$state_abb3 = str_sub(dataset2$land_names,start=-2)

## Now let's just look at the average of all available columns by state
averages = aggregate(dataset2,list(dataset2$state_abb),FUN=mean)
barplot(averages$PovSurveyPop,main='PovSurveyPop',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$PercentBelowPovLevel,main='PercentBelowPovLevel',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$Under18YrsPop,main='Under18YrsPop',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$X18to34Pop,main='X18to34Pop',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$X35to64Pop,main='X35to64Pop',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$Over65Pop,main='Over65Pop',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$MalePercentBelowPovLevel,main='MalePercentBelowPovLevel',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$FemalePercentBelowPovLevel,main='FemalePercentBelowPovLevel',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$WhitePercentBelowPovLevel,main='WhitePercentBelowPovLevel',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$AIANPercentBelowPovLevel,main='AIANPercentBelowPovLevel',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$NHPIPercentBelowPovLevel,main='NHPIPercentBelowPovLevel',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$LabForce16andOver,main='LabForce16andOver',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$Employed16andOver,main='Employed16andOver',xlab='State',names.arg=averages$Group.1,cex.names=0.45)
barplot(averages$Unemployed16andOver,main='Unemployed16andOver',xlab='State',names.arg=averages$Group.1,cex.names=0.45)


state_count = table(dataset2$state_abb)
print(averages$PovSurveyPop)
barplot(state_count,cex.names = 0.5)

## Ok... so the averages were really just me getting a bit distracted but I guess they gave me
## some idea of what the data looks like. What I really wanted to do was see if there are
## statistical differences between the states for each variable in the dataset, so
## to find that out I'll just regress each variable (by reservation, so from dataset2) on 
## a state category variable and all other variables in the dataset

## convert state_abb into factor variable
dataset2$state_abb.f = factor(dataset2$state_abb)
is.factor(dataset2$state_abb.f)
## first let's run the regressions of each variable just by state, and note the significantly different
## states for each regression (the control state is Alaska)
summary(lm(PovSurveyPop ~ state_abb.f, data=dataset2))
## at 10% level we have Louisiana and Utah (both higher than AK), at 1% level we have Oklahoma and North Carolina
## (both higher than AK)
summary(lm(PercentBelowPovLevel ~ state_abb.f, data=dataset2))
## at 10% level we have Nevada (slightly higher than AK), 5% level we have South Dakota (higher), Oklahoma (lower), 
## Oregon (higher), and Maine (higher), and 1% level we have Arizona (higher) and Hawaii (lower)
summary(lm(Under18YrsPop ~ state_abb.f, data=dataset2))
## sorry to say it, but this stuff just didn't pan out. The data is weird (as in it's difficult to parse out what
## state each entry should be considered in) and state-level policy analysis doesn't really make sense 
## for Indian reservations since they're federally designated. Oh well! The search for good data continues..."

AIANlabforce = read.csv('AIAN Labor Force Participation Rate.csv')
colnames(AIANlabforce)[2] = 'AIANlabforce'
time = c(1:nrow(AIANlabforce))
plot(time,AIANlabforce$AIANlabforce,type='l',col='blue',main='AIAN Labor Force Participation Rate',xlab='Months since January 2000',ylab='AIAN LFPR (%)')
## appears to be a big break in the data around early 2009 for some reason? Could just
## be due to the financial crisis, so let's bring in national labor force participation rate
NATlabforce = read.csv('National Labor Force Participation Rate.csv')
colnames(NATlabforce)[2] = 'NATlabforce'
plot(time,AIANlabforce$AIANlabforce,type='l',col='blue',main='AIAN and National Labor Force Participation Rate',xlab='Months since January 2000',ylab='LFPR (%)')
lines(time,NATlabforce$NATlabforce,col='red')
legend(200,68,legend=c('American Indian','National'),col=c('blue','red'),lty=1:1)
## well... that's just weird. It appears AIAN labor force participation rate is much, much more volatile
## as well as being generally lower than the national participation rate. I think I'll have to meditate on
## what exactly I can even look into to explain why that might be...
## Firstly, let's just do an F-test of the difference in variances to be certain
## they are statistically different enough for us to care!
AIANvar = var(AIANlabforce$AIANlabforce)
NATvar = var(NATlabforce$NATlabforce)
cat('P-VAL FOR DIFFERENCE IN VARIANCES:', pf( (AIANvar/NATvar), 275, 275, lower.tail=FALSE))
## Yup! Extremely statistically significantly different! Obviously, but good to check!
## Now the goal is to, using these two time series, construct a new time series
## of the difference in volatilities, then come up with two possible causes of the difference
## (that we have data for), and compare the effects of each in possibly an ARDL analysis,
## but I haven't determined yet what method makes the most sense there.

## Step 1: time series of volatility difference
## What I'm going to do here is construct a model of the mean for each time series
## (using the handy-dandy auto.arima function), obtain the residual time series for each,
## then take the absolute value of the difference between those two residual time series.
## The logic here is that, since volatility is /basically/ the distance between each observation
## and the time series's average, we just take the difference between each observation and
## the average prediction (i.e. the residuals) and we've obtained a time series of the volatilities.
## Then, since we're interested in the /difference/ in volatilities, just take the distance between
## them with 1-dimensional norm/distance function (i.e. absolute value function).
library(forecast)
AIANmodel = auto.arima(AIANlabforce$AIANlabforce)
AIANvol = AIANmodel$residuals
plot(time,AIANvol,type='l',main='AIAN Volatility Series',xlab='Months since January 2000',ylab='% Difference Between Predicted and Actual LFPR')
NATmodel = auto.arima(NATlabforce$NATlabforce)
NATvol = NATmodel$residuals
plot(time,NATvol,type='l',main='National Volatility Series',xlab='Months since January 2000',ylab='% Difference Between Predicted and Actual LFPR')


volDiff = abs(AIANvol - NATvol)
plot(time,volDiff,type='l',main='volDiff Series',xlab='Months since January 2000',ylab='Diff in % Diff Between Predicted and Actual LFPRs')

## Step 2: what affects the difference in volatilities?
## What I'm looking for here are variables that fall into one of the two following categories;
## a) Something that makes life less predictable for AIAN than non AIAN Americans, or
## b) Something that makes life more predictable for non AIAN Americans that AIAN don't have access to.
## These sound almost the same, except the second category is /much/ easier to find
## data on, since it would be variables that I would hypothesize affect non AIAN Americans
## but somehow isn't found in reservations (mainly, although I'm nearly 100% sure this data
## that I'm working with contains AIAN that don't live on reservations!).
## However, if I just focus on category a) variables, it simplifies my analysis massively, since
## there are exactly two category a) variables I have access to! AIAN unemployment and
## AIAN employment-population ratio... actually never mind I guess that second one invalidates
## that idea since that would just be 100-(the original time series I started with here).

## So the variables I've come down on are actually casino ppi (potentially cat a) )
## and national average hourly earnings in the private sector (potentially cat b) ).
## We'll take the volatilities of both, with the idea that if either actually affect
## the voldiff, it'll be a positive relationship.
PAHE = read.csv('privavghourlyearnings.csv')
colnames(PAHE)[2] = 'PAHE'
## make sure we labelled the files right with a plot!
plot(1:length(PAHE$PAHE),PAHE$PAHE,type='l')
## looks good!
## now just get volatility the same way we did for the last couple
PAHEmodel = auto.arima(PAHE$PAHE)
PAHEvol = PAHEmodel$residuals
CPPI = read.csv('casinoppi.csv')
colnames(CPPI)[2] = 'CPPI'
CPPI$CPPI = as.numeric(CPPI$CPPI)
## unfortunately had to impute only July 2020 obs, used mean imputation since it's quick and easy,
## obviously implicitly assumes that the missing-ness is completely random though,
## but might not be so unreasonable considering all observations around it are present.
CPPI$CPPI[which(CPPI$DATE == '2020-07-01')] = (CPPI$CPPI[which(CPPI$DATE == '2020-06-01')] + CPPI$CPPI[which(CPPI$DATE == '2020-08-01')]) / 2
CPPImodel = auto.arima(CPPI$CPPI)
CPPIvol = CPPImodel$residuals

## now just have to adjust the volDiff and CPPIvol series to match the length of PAHEvol,
## they end at the same point (Dec 2022) but begin at different points.
volDiffStart = length(volDiff) - length(PAHEvol) + 1
volDiff = volDiff[volDiffStart:length(volDiff)]
CPPIvolStart = length(CPPIvol) - length(PAHEvol) + 1
CPPIvol = CPPIvol[CPPIvolStart:length(CPPIvol)]

time = c(1:length(volDiff))
plot(time,volDiff,type='l',col='black',main='All Volatility Series',xlab='Months since March 2006',ylab='Volatility in %',ylim=c(min(CPPIvol)-0.5,max(CPPIvol)+0.5))
lines(time,CPPIvol,col='blue')
lines(time,PAHEvol,col='red')
legend(0,8,legend=c('volDiff','Casino Inflation Volatility','Private Average Hourly Earnings Volatility'),col=c('black','blue','red'),lty=1:1)
## that's... not the best sign for my hypothesis! But only an analysis will tell the full story!

## Step 3: Analysis.
## The question now is what methods do I want to use here? It is time series analysis, but
## do I necessarily have to use a very specifically time series analysis method?
## Or will basic OLS do the trick here? I guess it /may/ do the trick, I just have to be
## aware of the assumptions I'm operating under when I use it.
## So here's what we'll do: we'll do an ARDL analysis, using the same, less parsimonious
## but shorter code, method as we did for the second inflation intervention analysis for both
## volDiff on CPPIvol and volDiff on PAHEvol, then compare the best fit of each group.

max_lags = 0:15 ## this is used for both

PAHE_aic_vec = c()
volDiff_lag_vec = c()
PAHE_lag_vec = c()
for (volDiff_lag in max_lags) {
  PAHE_exog = PAHEvol
  for (PAHE_lag in max_lags) {
    if (PAHE_lag == 1) {
      new_col = PAHEvol[(PAHE_lag+1):length(volDiff)]
      PAHE_exog = PAHE_exog[1:(length(volDiff)-PAHE_lag)]
      PAHE_exog = cbind(PAHE_exog, new_col)
    }
    if (PAHE_lag > 1) {
      new_col = PAHEvol[(PAHE_lag+1):length(volDiff)]
      PAHE_exog = PAHE_exog[1:(length(volDiff)-PAHE_lag),]
      PAHE_exog = cbind(PAHE_exog, new_col)
    }
    test_model = arima(volDiff[1:(length(volDiff)-PAHE_lag)], order = c(volDiff_lag,0,0),
                      xreg = PAHE_exog, method='ML', optim.control = list(maxit=1000))
    PAHE_aic_vec = c(PAHE_aic_vec, test_model[['aic']])
    volDiff_lag_vec = c(volDiff_lag_vec, volDiff_lag)
    PAHE_lag_vec = c(PAHE_lag_vec, PAHE_lag)
  }
}

min_PAHE_aic = which(PAHE_aic_vec==min(PAHE_aic_vec))
min_volDiff_lag = volDiff_lag_vec[min_PAHE_aic]
min_PAHE_lag = PAHE_lag_vec[min_PAHE_aic]
cat('OPTIMAL ARDL MODEL HAS AIC ', min(PAHE_aic_vec), ', AND THE NUMBER OF LAGS ON volDiff AND PAHEvol
     ARE ', min_volDiff_lag, 'AND ', min_PAHE_lag, ', RESPECTIVELY.')
## appears that volDiff is like... too stationary? Well too stationary in the sense that
## it doesn't require any lags, and once it does include lags on volDiff the AIC drops.
## However, PAHEvol likes more and more lags it seems.
plot(c(1:length(PAHE_aic_vec)),PAHE_aic_vec,type='l',xlab='lags (every 15th lag, volDiff lag increases by 1)')

## now for CPPIvol analysis...
CPPI_aic_vec = c()
volDiff_lag_vec = c()
CPPI_lag_vec = c()
for (volDiff_lag in max_lags) {
  CPPI_exog = CPPIvol
  for (CPPI_lag in max_lags) {
    if (CPPI_lag == 1) {
      new_col = PAHEvol[(CPPI_lag+1):length(volDiff)]
      CPPI_exog = CPPI_exog[1:(length(volDiff)-CPPI_lag)]
      CPPI_exog = cbind(CPPI_exog, new_col)
    }
    if (CPPI_lag > 1) {
      new_col = PAHEvol[(CPPI_lag+1):length(volDiff)]
      CPPI_exog = CPPI_exog[1:(length(volDiff)-CPPI_lag),]
      CPPI_exog = cbind(CPPI_exog, new_col)
    }
    test_model = arima(volDiff[1:(length(volDiff)-CPPI_lag)], order = c(volDiff_lag,0,0),
                       xreg = CPPI_exog, method='ML', optim.control = list(maxit=1000))
    CPPI_aic_vec = c(CPPI_aic_vec, test_model[['aic']])
    volDiff_lag_vec = c(volDiff_lag_vec, volDiff_lag)
    CPPI_lag_vec = c(CPPI_lag_vec, CPPI_lag)
  }
}

min_CPPI_aic = which(CPPI_aic_vec==min(CPPI_aic_vec))
min_volDiff_lag = volDiff_lag_vec[min_CPPI_aic]
min_CPPI_lag = CPPI_lag_vec[min_CPPI_aic]
cat('OPTIMAL ARDL MODEL HAS AIC ', min(CPPI_aic_vec), ', AND THE NUMBER OF LAGS ON volDiff AND PAHEvol
     ARE ', min_volDiff_lag, 'AND ', min_CPPI_lag, ', RESPECTIVELY.')
plot(c(1:length(CPPI_aic_vec)),CPPI_aic_vec,type='l',xlab='lags (every 15th lag, volDiff lag increases by 1)')
## about the same situation here with the casino PPI volatility, but AIC is marginally higher!

## Ok let's be honest here, why did I do that? Why didn't I keep my word and go with the
## more laborious but clearly superior ARDL method??? I just got lazy, but this is a serious
## analysis! So let's be serious about it. We're gonna redo this ARDL analysis but with the
## more complex (but more parsimonious) setup.

### Casino PPI (CPPI) Analysis ###
## estimate optimal CPPIvol AR model;
max_lags = 1:20
CPPI_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  CPPI_test_model = arima(CPPIvol, order=c(lag,0,0))
  CPPI_aic_vec = c(CPPI_aic_vec,CPPI_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR CPPI: ', which(CPPI_aic_vec==min(CPPI_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(CPPI_aic_vec),CPPI_aic_vec,type='l')
## tested up to 20th, still seems to be 1 as the optimal lag level for CPPI... unexpected!
## store residuals from the optimal CPPI AR estimation
CPPI_model = arima(CPPIvol, order=c(1,0,0))
CPPI_res = CPPI_model[['residuals']]
## "filter" volDiff with CPPI_model coefficients
CPPI_coef = CPPI_model[['coef']]
filt_volDiff = CPPI_coef[1] + CPPI_coef[2]*lag(ts(volDiff),k=1)
## look at cross-correlogram between CPPI_res and filt_volDiff to determine best candidates
## for CPPIvol lags in final model
ccf(filt_volDiff,CPPI_res)
## looks like lags 1, 2, 3, and 11
CPPIvol_lags = cbind(lag(ts(CPPIvol),k=1), lag(ts(CPPIvol),k=2), lag(ts(CPPIvol),k=3), lag(ts(CPPIvol),k=11))[11:length(PAHEvol),1:4]
volDiff_lags_model = lm(volDiff[1:nrow(CPPIvol_lags)] ~ CPPIvol_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looking like... MA component up to lag 9 maybe (from ACF)? and AR components at 
## lags 8, 9, 12, and 35 (from PACF)?
## That's definitely where I'll cut it off, though, we don't have a /ton/ of data here anyway.
## Now just do the final CPPI model with the above parameters, we'll have to construct the
## exogenous matrix though since the lags are weird and we don't want to include /every/ lag
## up to #35, just the ones I listed.
volDiff_lags = cbind(lag(ts(volDiff),k=8), lag(ts(volDiff),k=9), lag(ts(volDiff),k=12), lag(ts(volDiff),k=35))[35:length(volDiff),1:4]
CPPI_model_exog = cbind(volDiff_lags, CPPIvol_lags[1:nrow(volDiff_lags),1:4])
CPPI_model = arima(volDiff[1:nrow(CPPI_model_exog)], order=c(0,0,11), xreg=CPPI_model_exog, optim.control = list(maxit=1000))
print(CPPI_model[['coef']])
print(CPPI_model[['aic']])
print(CPPI_model[['loglik']])
ARDL_CPPI_loglik_1 = CPPI_model[['loglik']]
summary(CPPI_model)
## So... improvement in fit by AIC, let's just check a single lag OLS regression tbh, I
## just want to see if it makes a big difference here
check = lm(volDiff[1:length(volDiff-1)] ~ lag(ts(CPPIvol),k=1))
summary(check)
## Yeah ok, looks worse! I guess the question I have is do I consider changes in CPPIvol
## interventions to the difference in NATvol and AIANvol? Hmmm... yeah I do, that's consistent
## with the intent of the project. Ok, repeat the process now but for PAHE

### Private Average Hourly Earnings (PAHE) Analysis ###
## estimate optimal PAHEvol AR model;
max_lags = 1:20
PAHE_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  PAHE_test_model = arima(PAHEvol, order=c(lag,0,0))
  PAHE_aic_vec = c(PAHE_aic_vec,PAHE_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR PAHE: ', which(PAHE_aic_vec==min(PAHE_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(PAHE_aic_vec),PAHE_aic_vec,type='l')
## tested up to 20th, still seems to be 15 as the optimal lag level for PAHE... unexpected!
## store residuals from the optimal PAHE AR estimation
PAHE_model = arima(PAHEvol, order=c(15,0,0))
PAHE_res = PAHE_model[['residuals']]
## "filter" volDiff with PAHE_model coefficients
PAHE_coef = PAHE_model[['coef']]
filt_volDiff = PAHE_coef[1] + PAHE_coef[2]*lag(ts(volDiff),k=1) + PAHE_coef[3]*lag(ts(volDiff),k=2) + PAHE_coef[4]*lag(ts(volDiff),k=3) +
  PAHE_coef[5]*lag(ts(volDiff),k=4) + PAHE_coef[6]*lag(ts(volDiff),k=5) + PAHE_coef[7]*lag(ts(volDiff),k=6) + 
  PAHE_coef[8]*lag(ts(volDiff),k=7) + PAHE_coef[9]*lag(ts(volDiff),k=8) + PAHE_coef[10]*lag(ts(volDiff),k=9) + 
  PAHE_coef[11]*lag(ts(volDiff),k=12) + PAHE_coef[12]*lag(ts(volDiff),k=11) + PAHE_coef[13]*lag(ts(volDiff),k=12) + 
  PAHE_coef[14]*lag(ts(volDiff),k=13) + PAHE_coef[15]*lag(ts(volDiff),k=14) + PAHE_coef[16]*lag(ts(volDiff),k=15)
## look at cross-correlogram between PAHE_res and filt_volDiff to determine best candidates
## for PAHEvol lags in final model
ccf(filt_volDiff,PAHE_res)
## looks like lags 1, 10, 15, and 19
PAHEvol_lags = cbind(lag(ts(PAHEvol),k=1), lag(ts(PAHEvol),k=10), lag(ts(PAHEvol),k=15), lag(ts(PAHEvol),k=19))[19:length(PAHEvol),1:4]
volDiff_lags_model = lm(volDiff[1:nrow(PAHEvol_lags)] ~ PAHEvol_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looking like... MA component up to lag 11 maybe (from ACF)? and AR components at 
## lags 9, 11, and 35 (from PACF)?
## That's definitely where I'll cut it off, though, we don't have a /ton/ of data here anyway.
## Now just do the final PAHE model with the above parameters, we'll have to construct the
## exogenous matrix though since the lags are weird and we don't want to include /every/ lag
## up to #35, just the ones I listed.
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=11), lag(ts(volDiff),k=35))[35:length(volDiff),1:3]
PAHE_model_exog = cbind(volDiff_lags, PAHEvol_lags[1:nrow(volDiff_lags),1:4])
PAHE_model = arima(volDiff[1:nrow(PAHE_model_exog)], order=c(0,0,11), xreg=PAHE_model_exog, optim.control = list(maxit=1000))
print(PAHE_model[['coef']])
print(PAHE_model[['aic']])
print(PAHE_model[['loglik']])
ARDL_PAHE_loglik = PAHE_model[['loglik']]
summary(PAHE_model)
## So... improvement in fit by AIC, let's just check a single lag OLS regression tbh, I
## just want to see if it makes a big difference here
check = lm(volDiff[1:length(volDiff-1)] ~ lag(ts(PAHEvol),k=1))
summary(check)
### So it appears that in this setup, the casino inflation does contribute more to the
# difference in labor force participation rate volatilities... not exactly the
# result we were hoping for to make the point we wanted to make! However,
# it may be worth our time to simply compare it with a different variable.
# I thought maybe it would make sense to check the effect of AIAN
# Unemployment's volatility on the difference, but I imagine that'd would
# almost certainly play a larger role in the volatility difference simply
# because it has more to do with the labor force environment (like it's just a more
# closely related statistic than casino inflation, not necessarily that casino inflation
# "doesn't have much of an effect compared to other factors" which is what we're going for).
# Buuuut I guess... it still kind of proves my point? Like if AIAN unemployment volatility
# contributes to the volatility difference more than casino inflation, then yeah casino inflation
# (and possibly most other factors affecting casinos) don't really matter as much as
# stereotypes may indicate. Ok, let's go with that then.

plot(c(1:length(volDiff)),volDiff,type='l',col='black',ylim=c(-0.75,5.2),main='volDiff and All Fitted Values',xlab='Months since March 2006',ylab='Diff in % Diff')
lines(c(35:length(volDiff)),fitted(CPPI_model),col='blue')
lines(c(35:length(volDiff)),fitted(PAHE_model),col='red')
legend(0,5.2,legend=c('volDiff','Casino Inflation Volatility Model','Private Average Hourly Earnings Volatility Model'),col=c('black','blue','red'),lty=1:1)

plot(c(1:length(volDiff)),volDiff,type='l',col='black',ylim=c(-0.75,5.2),main='volDiff and Casino Inflation Model Fitted Values Only',xlab='Months since March 2006',ylab='Diff in % Diff')
lines(c(35:length(volDiff)),fitted(CPPI_model),col='blue')
legend(0,5.2,legend=c('volDiff','Casino Inflation Volatility Model'),col=c('black','blue'),lty=1:1)

plot(c(1:length(volDiff)),volDiff,type='l',col='black',ylim=c(-0.75,5.2),main='volDiff and Private Average Hourly Earnings Model Fitted Values Only',xlab='Months since March 2006',ylab='Diff in % Diff')
lines(c(35:length(volDiff)),fitted(PAHE_model),col='red')
legend(0,5.2,legend=c('volDiff','Private Average Hourly Earnings Volatility Model'),col=c('black','red'),lty=1:1)


## just copied and pasted the code to generate the volDiff series;

AIANlabforce = read.csv('AIAN Labor Force Participation Rate.csv')
colnames(AIANlabforce)[2] = 'AIANlabforce'
time = c(1:nrow(AIANlabforce))
plot(time,AIANlabforce$AIANlabforce,type='l',col='blue')
## appears to be a big break in the data around early 2009 for some reason? Could just
## be due to the financial crisis, so let's bring in national labor force participation rate
NATlabforce = read.csv('National Labor Force Participation Rate.csv')
colnames(NATlabforce)[2] = 'NATlabforce'
lines(time,NATlabforce$NATlabforce,col='red')
## well... that's just weird. It appears AIAN labor force participation rate is much, much more volatile
## as well as being generally lower than the national participation rate. I think I'll have to meditate on
## what exactly I can even look into to explain why that might be...
## Firstly, let's just do an F-test of the difference in variances to be certain
## they are statistically different enough for us to care!
AIANvar = var(AIANlabforce$AIANlabforce)
NATvar = var(NATlabforce$NATlabforce)
cat('P-VAL FOR DIFFERENCE IN VARIANCES:', pf( (AIANvar/NATvar), 275, 275, lower.tail=FALSE))
## Yup! Extremely statistically significantly different! Obviously, but good to check!
## Now the goal is to, using these two time series, construct a new time series
## of the difference in volatilities, then come up with two possible causes of the difference
## (that we have data for), and compare the effects of each in possibly an ARDL analysis,
## but I haven't determined yet what method makes the most sense there.

## Step 1: time series of volatility difference
## What I'm going to do here is construct a model of the mean for each time series
## (using the handy-dandy auto.arima function), obtain the residual time series for each,
## then take the absolute value of the difference between those two residual time series.
## The logic here is that, since volatility is /basically/ the distance between each observation
## and the time series's average, we just take the difference between each observation and
## the average prediction (i.e. the residuals) and we've obtained a time series of the volatilities.
## Then, since we're interested in the /difference/ in volatilities, just take the distance between
## them with 1-dimensional norm/distance function (i.e. absolute value function).
library(forecast)
AIANmodel = auto.arima(AIANlabforce$AIANlabforce)
AIANvol = AIANmodel$residuals
NATmodel = auto.arima(NATlabforce$NATlabforce)
NATvol = NATmodel$residuals

volDiff = abs(AIANvol - NATvol)
plot(time,volDiff,type='l')

## Step 2: what affects the difference in volatilities?

## So the variables I've come down on are actually casino ppi,
## and AIAN unemployment rate.
## We'll take the volatilities of both, with the idea that if either actually affect
## the voldiff, it'll be a positive relationship.
AIANunemp = read.csv('AIAN Unemployment Rate.csv')
colnames(AIANunemp)[2] = 'AIANunemp'
## make sure we labelled the files right with a plot!
plot(1:length(AIANunemp$AIANunemp),AIANunemp$AIANunemp,type='l')
## looks good!
## now just get volatility the same way we did for the last couple
AIANunempModel = auto.arima(AIANunemp$AIANunemp)
AIANunempVol = AIANunempModel$residuals
CPPI = read.csv('casinoppi.csv')
colnames(CPPI)[2] = 'CPPI'
CPPI$CPPI = as.numeric(CPPI$CPPI)
## unfortunately had to impute only July 2020 obs, used mean imputation since it's quick and easy,
## obviously implicitly assumes that the missing-ness is completely random though,
## but might not be so unreasonable considering all observations around it are present.
CPPI$CPPI[which(CPPI$DATE == '2020-07-01')] = (CPPI$CPPI[which(CPPI$DATE == '2020-06-01')] + CPPI$CPPI[which(CPPI$DATE == '2020-08-01')]) / 2
CPPImodel = auto.arima(CPPI$CPPI)
CPPIvol = CPPImodel$residuals

## now just have to adjust the volDiff and CPPIvol series to match the length of PAHEvol,
## they end at the same point (Dec 2022) but begin at different points.
volDiffStart = length(volDiff) - length(CPPIvol) + 1
volDiff = volDiff[volDiffStart:length(volDiff)]
AIANunempVolStart = length(AIANunempVol) - length(CPPIvol) + 1
AIANunempVol = AIANunempVol[AIANunempVolStart:length(AIANunempVol)]

time = c(1:length(volDiff))
plot(time,volDiff,type='l',col='black',main='All Volatility Series',xlab='Months since June 2001',ylab='Volatility in %',ylim=c(min(CPPIvol)-0.5,max(AIANunempVol)+0.5))
lines(time,CPPIvol,col='blue')
lines(time,AIANunempVol,col='red')
legend(0,20,legend=c('volDiff','Casino Inflation Volatility','AIAN Unemployment Volatility'),col=c('black','blue','red'),lty=1:1)


## copied and pasted the code to find the best ARDL model for CPPIvol...

max_lags = 1:20
CPPI_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  CPPI_test_model = arima(CPPIvol, order=c(lag,0,0))
  CPPI_aic_vec = c(CPPI_aic_vec,CPPI_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR CPPI: ', which(CPPI_aic_vec==min(CPPI_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(CPPI_aic_vec),CPPI_aic_vec,type='l')
## with more data it appears the best lag level is actually at lag 19! weird!
## store residuals from the optimal CPPI AR estimation
CPPI_model = arima(CPPIvol, order=c(19,0,0))
CPPI_res = CPPI_model[['residuals']]
## "filter" volDiff with CPPI_model coefficients
CPPI_coef = CPPI_model[['coef']]
filt_volDiff = CPPI_coef[1] + CPPI_coef[2]*lag(ts(volDiff),k=1) + CPPI_coef[3]*lag(ts(volDiff),k=2) +
  CPPI_coef[4]*lag(ts(volDiff),k=3) + CPPI_coef[5]*lag(ts(volDiff),k=4) + CPPI_coef[6]*lag(ts(volDiff),k=5) +
  CPPI_coef[7]*lag(ts(volDiff),k=6) + CPPI_coef[8]*lag(ts(volDiff),k=7) + CPPI_coef[9]*lag(ts(volDiff),k=8) +
  CPPI_coef[10]*lag(ts(volDiff),k=9) + CPPI_coef[11]*lag(ts(volDiff),k=10) + CPPI_coef[12]*lag(ts(volDiff),k=11) +
  CPPI_coef[13]*lag(ts(volDiff),k=12) + CPPI_coef[14]*lag(ts(volDiff),k=13) + CPPI_coef[15]*lag(ts(volDiff),k=14) +
  CPPI_coef[16]*lag(ts(volDiff),k=15) + CPPI_coef[17]*lag(ts(volDiff),k=16) + CPPI_coef[18]*lag(ts(volDiff),k=17) +
  CPPI_coef[19]*lag(ts(volDiff),k=18) + CPPI_coef[20]*lag(ts(volDiff),k=19)
## look at cross-correlogram between CPPI_res and filt_volDiff to determine best candidates
## for CPPIvol lags in final model
ccf(filt_volDiff,CPPI_res)
## looks like lags 1, 4, 7, and 12
CPPIvol_lags = cbind(lag(ts(CPPIvol),k=1), lag(ts(CPPIvol),k=4), lag(ts(CPPIvol),k=7), lag(ts(CPPIvol),k=12))[12:length(CPPIvol),1:4]
volDiff_lags_model = lm(volDiff[1:nrow(CPPIvol_lags)] ~ CPPIvol_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looking like... no MA component now (from ACF)? and AR components at 
## lags 6, 7, 8, 9, 12, and 13 (from PACF)?
## That's definitely where I'll cut it off, though, we don't have a /ton/ of data here anyway.
## Now just do the final CPPI model with the above parameters, we'll have to construct the
## exogenous matrix though since the lags are weird and we don't want to include /every/ lag
## up to #14, just the ones I listed.
volDiff_lags = cbind(lag(ts(volDiff),k=6), lag(ts(volDiff),k=7), lag(ts(volDiff),k=8), lag(ts(volDiff),k=9), lag(ts(volDiff),k=12), lag(ts(volDiff),k=13))[14:length(volDiff),1:6]
CPPI_model_exog = cbind(volDiff_lags, CPPIvol_lags[1:nrow(volDiff_lags),1:4])
CPPI_model = arima(volDiff[1:nrow(CPPI_model_exog)], order=c(0,0,0), xreg=CPPI_model_exog, optim.control = list(maxit=1000))
print(CPPI_model[['coef']])
print(CPPI_model[['aic']])
print(CPPI_model[['loglik']])
ARDL_CPPI_loglik_2 = CPPI_model[['loglik']]
summary(CPPI_model)
## So... improvement in fit by AIC, let's just check a single lag OLS regression tbh, I
## just want to see if it makes a big difference here
check = lm(volDiff[1:length(volDiff-1)] ~ lag(ts(CPPIvol),k=1))
summary(check)
## Yeah ok, looks worse! I guess the question I have is do I consider changes in CPPIvol
## interventions to the difference in NATvol and AIANvol? Hmmm... yeah I do, that's consistent
## with the intent of the project. Ok, repeat the process now but for AIANunempVol

max_lags = 1:20
AIANunemp_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  AIANunemp_test_model = arima(AIANunempVol, order=c(lag,0,0))
  AIANunemp_aic_vec = c(AIANunemp_aic_vec,AIANunemp_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR AIAN Unemployment Rate: ', which(AIANunemp_aic_vec==min(AIANunemp_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(AIANunemp_aic_vec),AIANunemp_aic_vec,type='l')
## optimal lag... seems to be at level 1! Weird again!
## store residuals from the optimal AIANunemp AR estimation
AIANunemp_model = arima(AIANunempVol, order=c(1,0,0))
AIANunemp_res = AIANunemp_model[['residuals']]
## "filter" volDiff with AIANunemp_model coefficients
AIANunemp_coef = AIANunemp_model[['coef']]
filt_volDiff = AIANunemp_coef[1] + AIANunemp_coef[2]*lag(ts(volDiff),k=1)
## look at cross-correlogram between CPPI_res and filt_volDiff to determine best candidates
## for CPPIvol lags in final model
ccf(filt_volDiff,AIANunemp_res)
## looks like lags 2, 9, and 15
AIANunempVol_lags = cbind(lag(ts(AIANunempVol),k=2), lag(ts(AIANunempVol),k=9), lag(ts(AIANunempVol),k=15))[15:length(AIANunempVol),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(AIANunempVol_lags)] ~ AIANunempVol_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looking like... no MA component now (from ACF)? and AR components at 
## lags 6, 7, 8, 9, 12, and 35 (from PACF)?
## That's definitely where I'll cut it off, though, we don't have a /ton/ of data here anyway.
## Now just do the final CPPI model with the above parameters, we'll have to construct the
## exogenous matrix though since the lags are weird and we don't want to include /every/ lag
## up to #35, just the ones I listed.
volDiff_lags = cbind(lag(ts(volDiff),k=6), lag(ts(volDiff),k=7), lag(ts(volDiff),k=8), lag(ts(volDiff),k=9), lag(ts(volDiff),k=12), lag(ts(volDiff),k=35))[35:length(volDiff),1:6]
AIANunemp_model_exog = cbind(volDiff_lags, AIANunempVol_lags[1:nrow(volDiff_lags),1:3])
AIANunemp_model = arima(volDiff[1:nrow(AIANunemp_model_exog)], order=c(0,0,0), xreg=AIANunemp_model_exog, optim.control = list(maxit=1000))
print(AIANunemp_model[['coef']])
print(AIANunemp_model[['aic']])
print(AIANunemp_model[['loglik']])
ARDL_AIANunemp_loglik = AIANunemp_model[['loglik']]
summary(AIANunemp_model)
## So... improvement in fit by AIC, let's just check a single lag OLS regression tbh, I
## just want to see if it makes a big difference here
check = lm(volDiff[1:length(volDiff-1)] ~ lag(ts(AIANunempVol),k=1))
summary(check)
print(AIC(check))
## Interesting! This alone gets pretty close to the level of fit for the casino inflation.
## But looks like the unemployment rate has a bigger effect than casino inflation
## in our ARDL analysis. Nice! However, the sign is the opposite of what I'd expect from unemployment volatility...

plot(c(1:length(volDiff)),volDiff,type='l',col='black',ylim=c(0,6.5),main='volDiff and All Fitted Values',xlab='Months since June 2001',ylab='Diff in % Diff')
lines(c(14:length(volDiff)),fitted(CPPI_model),col='blue')
lines(c(35:length(volDiff)),fitted(AIANunemp_model),col='red')
legend(0,6.5,legend=c('volDiff','Casino Inflation Volatility Model','AIAN Unemployment Volatility Model'),col=c('black','blue','red'),lty=1:1)

plot(c(1:length(volDiff)),volDiff,type='l',col='black',ylim=c(0,6.5),main='volDiff and Casino Inflation Model Fitted Values Only',xlab='Months since June 2001',ylab='Diff in % Diff')
lines(c(14:length(volDiff)),fitted(CPPI_model),col='blue')
legend(0,6.5,legend=c('volDiff','Casino Inflation Volatility Model'),col=c('black','blue'),lty=1:1)

plot(c(1:length(volDiff)),volDiff,type='l',col='black',ylim=c(0,6.5),main='volDiff and AIAN Unemployment Model Fitted Values Only',xlab='Months since June 2001',ylab='Diff in % Diff')
lines(c(35:length(volDiff)),fitted(AIANunemp_model),col='red')
legend(0,6.5,legend=c('volDiff','AIAN Unemployment Volatility Model'),col=c('black','red'),lty=1:1)


AIANlabforce = read.csv('AIAN Labor Force Participation Rate.csv')
colnames(AIANlabforce)[2] = 'AIANlabforce'
blackLFPR = read.csv('blackAmericanLFPR.csv')
colnames(blackLFPR)[2] = 'blackLFPR'
time = c(1:nrow(AIANlabforce))
plot(time,AIANlabforce$AIANlabforce,type='l',col='black',main='AIAN and Black American LFPR',xlab='Months since January 2000',ylab='LFPR (%)')
lines(time,blackLFPR$blackLFPR,col='blue')
legend(175,68,legend=c('AIAN LFPR','Black American LFPR'),col=c('black','blue'),lty=1:1)


########################################
### Ok... maybe GARCH-X makes much more sense here... We are modeling volatility
## after all... Let's just give it a whirl and see how it goes

## GARCH-X comparison between casino ppi and average hourly private earnings...
# (copied the code to read in and adjust the data for PAHE and CPPI)
AIANlabforce = read.csv('AIAN Labor Force Participation Rate.csv')
colnames(AIANlabforce)[2] = 'AIANlabforce'
time = c(1:nrow(AIANlabforce))
plot(time,AIANlabforce$AIANlabforce,type='l',col='blue')
## appears to be a big break in the data around early 2009 for some reason? Could just
## be due to the financial crisis, so let's bring in national labor force participation rate
NATlabforce = read.csv('National Labor Force Participation Rate.csv')
colnames(NATlabforce)[2] = 'NATlabforce'
lines(time,NATlabforce$NATlabforce,col='red')
## well... that's just weird. It appears AIAN labor force participation rate is much, much more volatile
## as well as being generally lower than the national participation rate. I think I'll have to meditate on
## what exactly I can even look into to explain why that might be...
## Firstly, let's just do an F-test of the difference in variances to be certain
## they are statistically different enough for us to care!
AIANvar = var(AIANlabforce$AIANlabforce)
NATvar = var(NATlabforce$NATlabforce)
cat('P-VAL FOR DIFFERENCE IN VARIANCES:', pf( (AIANvar/NATvar), 275, 275, lower.tail=FALSE))
## Yup! Extremely statistically significantly different! Obviously, but good to check!
## Now the goal is to, using these two time series, construct a new time series
## of the difference in volatilities, then come up with two possible causes of the difference
## (that we have data for), and compare the effects of each in possibly an ARDL analysis,
## but I haven't determined yet what method makes the most sense there.

## Step 1: time series of volatility difference
## What I'm going to do here is construct a model of the mean for each time series
## (using the handy-dandy auto.arima function), obtain the residual time series for each,
## then take the absolute value of the difference between those two residual time series.
## The logic here is that, since volatility is /basically/ the distance between each observation
## and the time series's average, we just take the difference between each observation and
## the average prediction (i.e. the residuals) and we've obtained a time series of the volatilities.
## Then, since we're interested in the /difference/ in volatilities, just take the distance between
## them with 1-dimensional norm/distance function (i.e. absolute value function).
library(forecast)
AIANmodel = auto.arima(AIANlabforce$AIANlabforce)
AIANvol = AIANmodel$residuals
NATmodel = auto.arima(NATlabforce$NATlabforce)
NATvol = NATmodel$residuals

volDiff = abs(AIANvol - NATvol)
plot(time,volDiff,type='l')

## Step 2: what affects the difference in volatilities?
## What I'm looking for here are variables that fall into one of the two following categories;
## a) Something that makes life less predictable for AIAN than non AIAN Americans, or
## b) Something that makes life more predictable for non AIAN Americans that AIAN don't have access to.
## These sound almost the same, except the second category is /much/ easier to find
## data on, since it would be variables that I would hypothesize affect non AIAN Americans
## but somehow isn't found in reservations (mainly, although I'm nearly 100% sure this data
## that I'm working with contains AIAN that don't live on reservations!).
## However, if I just focus on category a) variables, it simplifies my analysis massively, since
## there are exactly two category a) variables I have access to! AIAN unemployment and
## AIAN employment-population ratio... actually never mind I guess that second one invalidates
## that idea since that would just be 100-(the original time series I started with here).

## So the variables I've come down on are actually casino ppi (potentially cat a) )
## and national average hourly earnings in the private sector (potentially cat b) ).
## We'll take the volatilities of both, with the idea that if either actually affect
## the voldiff, it'll be a positive relationship.
PAHE = read.csv('privavghourlyearnings.csv')
colnames(PAHE)[2] = 'PAHE'
## make sure we labelled the files right with a plot!
plot(1:length(PAHE$PAHE),PAHE$PAHE,type='l')
## looks good!
## now just get volatility the same way we did for the last couple
PAHEmodel = auto.arima(PAHE$PAHE)
PAHEvol = PAHEmodel$residuals
CPPI = read.csv('casinoppi.csv')
colnames(CPPI)[2] = 'CPPI'
CPPI$CPPI = as.numeric(CPPI$CPPI)
## unfortunately had to impute only July 2020 obs, used mean imputation since it's quick and easy,
## obviously implicitly assumes that the missing-ness is completely random though,
## but might not be so unreasonable considering all observations around it are present.
CPPI$CPPI[which(CPPI$DATE == '2020-07-01')] = (CPPI$CPPI[which(CPPI$DATE == '2020-06-01')] + CPPI$CPPI[which(CPPI$DATE == '2020-08-01')]) / 2
CPPImodel = auto.arima(CPPI$CPPI)
CPPIvol = CPPImodel$residuals

## now just have to adjust the volDiff and CPPIvol series to match the length of PAHEvol,
## they end at the same point (Dec 2022) but begin at different points.
volDiffStart = length(volDiff) - length(PAHEvol) + 1
volDiff = volDiff[volDiffStart:length(volDiff)]
CPPIvolStart = length(CPPIvol) - length(PAHEvol) + 1
CPPIvol = CPPIvol[CPPIvolStart:length(CPPIvol)]

time = c(1:length(volDiff))
plot(time,CPPIvol,type='l',col='blue')
lines(time,PAHEvol,col='red')
lines(time,volDiff,col='green')
## that's... not the best sign for my hypothesis! But only an analysis will tell the full story!

## now to use GARCH-X, with volDiff as dependent var and CPPI, then PAHE, as the "X" factor
#install.packages('garchx')
library(garchx)
PAHE_volDiff_model = garchx(volDiff, order=c(1,1),xreg=PAHEvol)
PAHE_volDiff_fitted = PAHE_volDiff_model[['fitted']]
nreps = 1000
PAHE_volDiff_fitted = sqrt(PAHE_volDiff_model[['fitted']]) * rnorm(length(PAHE_volDiff_model[['fitted']]),mean=0,sd=1)
for (rep in (nreps - 1)) {
  PAHE_volDiff_fitted = PAHE_volDiff_fitted + sqrt(PAHE_volDiff_model[['fitted']]) * rnorm(length(PAHE_volDiff_model[['fitted']]),mean=0,sd=1)
}
PAHE_volDiff_fitted = PAHE_volDiff_fitted / nreps
print(logLik(PAHE_volDiff_model))
GARCHX_PAHE_loglik = logLik(PAHE_volDiff_model)
plot(c(1:length(volDiff)),volDiff,type='l')
lines(c(1:length(PAHE_volDiff_fitted)),PAHE_volDiff_fitted,col='purple')

CPPI_volDiff_model = garchx(volDiff, order=c(1,1),xreg=CPPIvol)
print(logLik(CPPI_volDiff_model))
GARCHX_CPPI_loglik1 = logLik(CPPI_volDiff_model)
plot(c(1:length(volDiff)),volDiff,type='l')
lines(c(1:length(CPPI_volDiff_model[['fitted']])),CPPI_volDiff_model[['fitted']],col='red')


cat('-------------------------FIRST ARDL LOG-LIKELIHOOD RATIOS-------------------------\nCPPI:', ARDL_CPPI_loglik_1, '\nPAHE:', ARDL_PAHE_loglik)
cat('-------------------------SECOND ARDL LOG-LIKELIHOOD RATIOS-------------------------\nCPPI:', ARDL_CPPI_loglik_2, '\nAIAN UNEMPLOYMENT:', ARDL_AIANunemp_loglik)
cat('-------------------------FIRST GARCH-X LOG-LIKELIHOOD RATIOS-------------------------\nCPPI:', GARCHX_CPPI_loglik1, '\nPAHE:', GARCHX_PAHE_loglik)