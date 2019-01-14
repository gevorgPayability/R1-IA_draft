library(data.table)
library(plotly)
options(scipen = 999)

## Data Preparation
data <- data.table( model$distribution )
cls <- gsub('V', 'day_',  colnames(data))
setnames(data, cls)
data <- round( exp(data), 2)
fwrite(data, 'data.csv')


data = fread('data.csv')
## Revenue function
risk_of_suspeniosn <- 1
fraud_risk = 0.005
loan_percent = 0.15
periods = ncol(data)

dt <- copy(data)

N = 1:ncol(dt)
rows = 1:nrow(dt)

for (row in rows) {
  fraud = rbinom(n = 1, size = 1, prob = fraud_risk)
  if (fraud == 1) {
    dt[row, 1:92 := 0]
    next()
  }
  blocked = rbinom(n = 1, size = 1, prob = risk_of_suspeniosn)
  if (blocked == 1) {
    day =  sample(x = 1:periods, size = 1, replace = F)
    dt[row, day:92 := 0]
  }
}

dt
## Assumptions - Seller never pays before 

sales = rowSums(dt)
hist(sales, breaks = 30)
sales_month = sales/ncol(dt)*30
proposed = round(quantile(sales_month, probs = seq(0.2, 0.95, 0.05)), -2)

sales_share_ours = rowSums(dt * loan_percent)
hist(sales_share_ours)

prop_ = proposed[p]
money_lended = prop_ * (1-loan_percent)
our_profit = pmin(sales_share_ours, prop_) - money_lended
hist(our_profit)
mean(our_profit)
 
m = pmin(sales, prop_ * (1 - loan_percent)) - (prop_ * (1 - loan_percent)) + pmin(prop_ * loan_percent, our_profit)
dt_compare[level == prop_, profit := round(mean(m))]
dt_compare[level == prop_, risk := mean(m < 0)]


p = 1


dt_compare = data.table(level = as.factor(proposed))
dt_compare[, profit := 0]
dt_compare[, risk_losing_money := 0]

for (p in seq_along(proposed)) {
 prop_ = proposed[p]
 our_profit = rowSums(dt_bool) * loan_percent/periods*prop_
 m = pmin(sales, prop_ * (1 - loan_percent)) - (prop_ * (1 - loan_percent)) + pmin(prop_ * loan_percent, our_profit)
 dt_compare[level == prop_, profit := round(mean(m))]
 dt_compare[level == prop_, risk := mean(m < 0)]
}

dt_compare
ay <- list(
  
  overlaying = "y",
  side = "right",
  title = "Risk of loosing money"
)


ay <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  overlaying = "y",
  side = "right",
  showticklabels = FALSE,
  showgrid = FALSE,
  hoverformat = ',.1%'
)

t <- list(
  family = "sans serif",
  size = 14,
  color = 'blue')


plot_ly(p = dt_compare, x = ~ level, y = ~ profit, type = 'bar', text = ~ profit) %>% 
  add_lines(p = dt_compare, x = ~ level, y = ~ risk, text = ~ risk, textposition = 'auto', yaxis = 'y2', name= 'risk', line = list(color = 'rgb(205, 12, 24)')) %>% 
  layout(
    title = list("Loan recommendation for a seller", font =t),
    yaxis2 = ay,
    xaxis = list(title="",
                 ticksuffix = '$'),
    yaxis = list(ticksuffix = '$')
  ) %>% 
  layout(showlegend = FALSE)

plot_ly(data = dt_compare, x = ~ level, y = ~ profit, type = 'bar', text = ~ profit) %>% 
  add_lines(x = ~ level, y = ~ risk, text = ~ risk, textposition = 'auto', 
             yaxis = 'y2', name= 'risk', line = list(color = 'rgb(205, 12, 24)')) %>% 
  layout(
    title = list("Loan recommendation for a seller", font =t),
    yaxis2 = ay,
    xaxis = list(title="",
                 ticksuffix = '$'),
    yaxis = list(ticksuffix = '$')
  ) %>% 
  layout(showlegend = FALSE)


  add_lines( x = ~ level, y = ~ risk, text = ~ risk, textposition = 'auto', 
            yaxis = 'y2', name= 'risk', line = list(color = 'rgb(205, 12, 24)')) %>% 
  layout(
    title = list("Loan recommendation for a seller", font =t),
    yaxis2 = ay)

plot_ly(dt_compare , x = ~ level, y = ~ profit, type = 'bar', text = ~ profit) %>% 
  add_lines(p = dt_compare, x = ~level, y = ~risk, text = ~ risk, textposition = 'auto', 
            yaxis = 'y2', name= 'risk', line = list(color = 'rgb(205, 12, 24)')) %>% 
  layout(
    title = list("Loan recommendation for a seller", font =t),
    yaxis2 = ay,
    xaxis = list(title="",
                 ticksuffix = '$'),
    yaxis = list(ticksuffix = '$')
  ) %>% 
  layout(showlegend = FALSE)

plot_ly(dt_compare) %>% 
  add_trace(x = ~ level, y = ~ profit, type = 'bar', text = ~ profit) %>% 
  add_trace(x = ~level, y = ~risk, type = 'scatter', mode = 'lines',  text = ~ risk, textposition = 'auto', 
             yaxis = 'y2', name= 'risk')%>% 
  layout(
    title = list("Loan recommendation for a seller"),
    yaxis2 = ay,
    xaxis = list(title="",
                 ticksuffix = '$'),
    yaxis = list(ticksuffix = '$')
  )


## Do frontu

# Sales time series
# Histogram of total sales, and montjly sales
# Recomended loan
# Valueboxes


predictions = fread('predictions.csv')

predictions


p <- plot_ly() %>%
  add_lines(x = time(USAccDeaths), y = USAccDeaths,
            color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction")



predictions

dt_plot <- predictions[date >= '2017-06-01', .(date, sales = exp(log_sales), predicted = exp(pred), upper = exp(upper), lower = exp(lower))]
fwrite(dt_plot, 'dt_plot.csv')

plot_ly(dt_plot) %>%
  add_lines(x = ~date, y = ~sales,
             name = "observed") %>% 
  add_ribbons(x = ~date, y ~ predicted, ymin = ~lower, ymax = ~upper,  color = I("gray80"),
              name = "95% confidence")



dt_plot <- fread('dt_plot.csv')

dt_plot[between(date, '2018-07-01', '2018-09-31'), sum(sales)/.N*30]
