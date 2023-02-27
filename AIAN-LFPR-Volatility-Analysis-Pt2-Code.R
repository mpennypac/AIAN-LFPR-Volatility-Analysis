aian_lfpr = read.csv('AIAN Labor Force Participation Rate.csv')
nat_lfpr = read.csv('National Labor Force Participation Rate.csv')
black_lfpr = read.csv('Black Labor Force Participation Rate.csv')
white_lfpr = read.csv('White Labor Force Participation Rate.csv')
hisp_lfpr = read.csv('Hispanic Labor Force Participation Rate.csv')
asian_lfpr = read.csv('Asian Labor Force Participation Rate.csv')


cat('Cor of AIAN & National', cor(aian_lfpr$labforceparticipation, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & Black', cor(aian_lfpr$labforceparticipation, black_lfpr$LNU01300006),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(black_lfpr$LNU01300006)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & White', cor(aian_lfpr$labforceparticipation, white_lfpr$LNU01300003),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(white_lfpr$LNU01300003)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & Hispanic', cor(aian_lfpr$labforceparticipation, hisp_lfpr$LNU01300009),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(hisp_lfpr$LNU01300009)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & Asian', cor(aian_lfpr$labforceparticipation, asian_lfpr$LNU01332183),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & National', cor(black_lfpr$LNU01300006, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & White', cor(black_lfpr$LNU01300006, white_lfpr$LNU01300003), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(white_lfpr$LNU01300003)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & Hispanic', cor(black_lfpr$LNU01300006, hisp_lfpr$LNU01300009), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(hisp_lfpr$LNU01300009)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & Asian', cor(black_lfpr$LNU01300006, asian_lfpr$LNU01332183), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of White & National', cor(white_lfpr$LNU01300003, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(white_lfpr$LNU01300003)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of White & Hispanic', cor(white_lfpr$LNU01300003, hisp_lfpr$LNU01300009), '\nP-Val for diff in variances', pf((var(white_lfpr$LNU01300003)/var(hisp_lfpr$LNU01300009)), 275, 275, lower.tail=FALSE))
cat('\nCor of White & Asian', cor(white_lfpr$LNU01300003, asian_lfpr$LNU01332183), '\nP-Val for diff in variances', pf((var(white_lfpr$LNU01300003)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of Hispanic & National', cor(hisp_lfpr$LNU01300009, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(hisp_lfpr$LNU01300009)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of Hispanic & Asian', cor(hisp_lfpr$LNU01300009, asian_lfpr$LNU01332183), '\nP-Val for diff in variances', pf((var(hisp_lfpr$LNU01300009)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of Asian & National', cor(asian_lfpr$LNU01332183, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(asian_lfpr$LNU01332183)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
plot(c(1:nrow(aian_lfpr)), aian_lfpr$labforceparticipation, type='l',col='blue',ylim=c(55,70))
lines(c(1:nrow(aian_lfpr)), nat_lfpr$LNU01300000, col='red')
lines(c(1:nrow(aian_lfpr)), black_lfpr$LNU01300006, col='black')
lines(c(1:nrow(aian_lfpr)), white_lfpr$LNU01300003, col='orange')
lines(c(1:nrow(aian_lfpr)), hisp_lfpr$LNU01300009, col='green')
lines(c(1:nrow(aian_lfpr)), asian_lfpr$LNU01332183, col='brown')
legend(0,61.5,legend=c('AIAN','National','Black','White','Hispanic','Asian'),col=c('blue','red','black','orange','green','brown'),lty=1:1)
## well that was uneventful, seems like AIAN LFPR is just seriously unique as far as racial/ethnic groups go
## let's compare things by state then instead


state_lfpr = read.csv('state-level-LFPR.csv')
rep_states = c()

for (i in 1:ncol(state_lfpr)) {
  p_val1 = pf((var(state_lfpr[,i])/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE)
  if (p_val1 < 0.01) {## this tells us if the state LFPR has a similar difference in variance from the national LFPR
    #cat('Sig Dif Btwn Variance of Nat LFPR and LFPR of: ', colnames(state_lfpr)[i], '\nP-Value: ', p_val, '\n')#Correlation with Nat LFPR: ', cor(state_lfpr[,i], nat_lfpr$LNU01300000), '\n')
    p_val2 = pf((var(state_lfpr[,i])/var(aian_lfpr$labforceparticipation)), 275, 275, lower.tail=FALSE)
    if (p_val2 > 0.1) {## this tells us if the state LFPR has a similar level of volatility as AIAN LFPR, which narrows our search for representative populations
      #cat('Insig Dif with Variance of AIAN LFPR\nP-Value: ', p_val, '\n\n')
      #cat('P-Value Between AIAN and State: ', p_val2, '\nSTATE ABBREVIATION: ', colnames(state_lfpr)[i], '\n')
      rep_states = c(rep_states, colnames(state_lfpr)[i])
    }
    #cat('P-Value Between National and State: ', p_val1, '\n------\n')
    
  }
  #cat('Correlation between ', colnames(state_lfpr)[i], ' LFPR and AIAN LFPR: ', cor(state_lfpr[,i], aian_lfpr$labforceparticipation), '\n\n')
}
#print('-----------------------------------------------------------------------------------------')

## here is our (potentially) representative list of states
print(rep_states)


cps_dec22 = read.csv('dec22pub.csv')
print(table(cps_dec22$ptdtrace))
aian_cps_dec22 = cps_dec22[cps_dec22$ptdtrace == 3,]
avg_age = mean(aian_cps_dec22$prtage[!is.na(aian_cps_dec22$prtage)])
age_survey_count = length(aian_cps_dec22$prtage[!is.na(aian_cps_dec22$prtage)])
age_missing_count = length(aian_cps_dec22$prtage[is.na(aian_cps_dec22$prtage)])