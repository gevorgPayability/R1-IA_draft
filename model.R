library(data.table)
library(lubridate)
library(bsts)
library(ggplot2)

## Data
data = fread('~/Python/R1-IA_draft/for_R.csv')
data[, date := as.Date(date)]
stability = data[, .(stability = mean(ordered_product_sales)/sd(ordered_product_sales)), by = mp_sup_key]
setorder(stability, -stability)


seller = 1
s = stability[seller, mp_sup_key]

dt_best = data[mp_sup_key == s]
dt_best
plot(x = dt_best$date, y = dt_best$ordered_product_sales, type = 'l')

seller_30 = 29
s30 = stability[seller_30, mp_sup_key]
dt_worst = data[mp_sup_key == s30]
plot(x = dt_worst$date, y = dt_worst$sales_7_days, type = 'l')

colnames(dt_best)
## model
library(bsts)

dt_model = dt_best[, .(date, log_sales = log(ordered_product_sales), day_of_week, day_of_month, is_month_end, is_month_start, week_of_year, quarter, month)]  
dt_model



train = dt_model[date < '2018-07-01']
validate = dt_model[date >= '2018-07-01' & date < '2018-10-01']
test = dt_model[date > '2018-10-01']

## Optimize for a nseason
 
season = 40
mape = 100

for (i in 3:season) {
      print(i)
      ss_log = AddLocalLinearTrend(list(), train$log_sales)
      ss_log <- AddSeasonal(ss_log, train$log_sales, nseasons = i)
      
      log_model = bsts(log_sales ~ day_of_week + day_of_month + is_month_end + is_month_start + week_of_year + quarter + month,
                       state.specification = ss_log, data = train,  niter = 1000, ping=0)
      burn <- SuggestBurn(0.1, log_model)
      
      p_log = predict(log_model, newdata = validate, burn = burn, quantiles = c(.025, .975))
      
      posterior.interval_log <- data.table(
        true = validate$log_sales,
        pred = p_log$mean,
        lower = as.numeric(p_log$interval[1,]),
        upper = as.numeric(p_log$interval[2,]),
        date = validate$date)
      
      
      current_mape = posterior.interval_log[, mean(abs(pred - true)/true)]
      
      if (current_mape < mape) {
        msg = sprintf('Best mape is %s for nseasons =  %s', current_mape, season)
        print(msg)
        mape <- current_mape
        best_model <- log_model
      }

}

mape
best_model


## best season - 13 with mape = 0.02383885


compare_log = merge(dt_model, posterior.interval_log, by = 'date', all.x = TRUE)
compare_log[is.na(pred), pred := log_sales]


ggplot(compare_log, aes(x = date)) + 
  geom_line(aes(y= log_sales, colour = "sales_7_days"), size=1.2) +
  geom_line(aes(y= pred, colour = "pred"), size=1.2, linetype=2) +
  theme_bw() + 
  theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-12-01")), linetype=2) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.5) 


## Selecting priors
mape = 100
best_fit = list()
for (level_sigma_guess in c(0.01, 0.1, 0.5)) {
  for (slope_mean_guess in c(0.01, 0.1, 0.5)) {
    for (slope_sigma_guess in c(0.01, 0.1, 0.5)) {
      for (ar_sigma_guess in c(0.01, 0.1, 0.5, 1)) {
        
        t = sprintf("level_sigma_guess - %s, slope_mean_guess - %s, slope_sigma_guess -  %s, ar_sigma_guess - %s, season - 13",
                  level_sigma_guess, slope_mean_guess, slope_sigma_guess, ar_sigma_guess)
        print(t)
        
        sdy <- sd(train$log_sales)
        sigma.prior <- SdPrior(sigma.guess=level_sigma_guess*sdy,
                               upper.limit=sdy,
                               sample.size=16)
        slope.mean.prior <- NormalPrior(mu=0,
                                        sigma=slope_mean_guess*sdy)
        slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                               sigma=1*ar_sigma_guess)
        slope.sigma.prior <- SdPrior(sigma.guess=slope_sigma_guess*sdy,
                                     upper.limit=sdy,
                                     sample.size=16)
        
        ss_log = AddSemilocalLinearTrend(list(),
                                train$log_sales,
                                level.sigma.prior=sigma.prior,
                                slope.mean.prior=slope.mean.prior,
                                slope.ar1.prior=slope.ar1.prior,
                                slope.sigma.prior=slope.sigma.prior)
      
        ss_log <- AddSeasonal(ss_log, train$log_sales, nseasons = 13)
        
        log_model = bsts(log_sales ~ day_of_week + day_of_month + is_month_end + is_month_start + week_of_year + quarter + month,
                         state.specification = ss_log, data = train,  niter = 600, ping=0)
        burn <- SuggestBurn(0.1, log_model)
        
        p_log = predict(log_model, newdata = validate, burn = burn, quantiles = c(.025, .975))
        
        current_mape = mean(abs(p_log$mean - validate$log_sales)/validate$log_sales)
        
        if (current_mape < mape) {
          mape = current_mape
          msg = sprintf('Best mape is %s for params =  %s', current_mape, t)
          print(msg)
    
        }
        
        
      }
    }
  }
}


# Best mape is 0.0222151205895486 
# for params =  level_sigma_guess - 0.01, slope_mean_guess - 0.5, slope_sigma_guess -  0.5,
# ar_sigma_guess - 0.5, season - 13


## True model

train = dt_model[date < '2018-10-01']
test = dt_model[date >= '2018-10-01']

sdy <- sd(train$log_sales)
sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                       upper.limit=sdy,
                       sample.size=16)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.5*sdy)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=1*0.5)
slope.sigma.prior <- SdPrior(sigma.guess=0.5*sdy,
                             upper.limit=sdy,
                             sample.size=16)

ss_log = AddSemilocalLinearTrend(list(),
                                 train$log_sales,
                                 level.sigma.prior=sigma.prior,
                                 slope.mean.prior=slope.mean.prior,
                                 slope.ar1.prior=slope.ar1.prior,
                                 slope.sigma.prior=slope.sigma.prior)

ss_log <- AddSeasonal(ss_log, train$log_sales, nseasons = 13)

log_model = bsts(log_sales ~ day_of_week + day_of_month + is_month_end + is_month_start + week_of_year + quarter + month,
                 state.specification = ss_log, data = train,  niter = 5000, ping=0)
burn <- SuggestBurn(0.1, log_model)

p_log = predict(log_model, newdata = test, burn = burn, quantiles = c(.025, .975))

posterior.interval_log <- data.table(
  true = test$log_sales,
  pred = p_log$mean,
  lower = as.numeric(p_log$interval[1,]),
  upper = as.numeric(p_log$interval[2,]),
  date = test$date)


compare_log = merge(dt_model, posterior.interval_log, by = 'date', all.x = TRUE)
compare_log[is.na(pred), pred := log_sales]

compare_log

ggplot(compare_log, aes(x = date)) + 
  geom_line(aes(y= log_sales, colour = "sales"), size=1.2) +
  geom_line(aes(y= pred, colour = "pred"), size=1.2, linetype=2) +
  theme_bw() + 
  theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-10-01")), linetype=2) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.5) 

compare_log

fwrite(compare_log, 'predictions.csv')
saveRDS(p_log, 'model.RDS')
