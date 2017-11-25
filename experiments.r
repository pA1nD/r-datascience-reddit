# Time Series Analysis ----------------------------------------------------
# probably need to convert to dates by this time

# TODO LATER
require(xts)
require(forecast)

time_index <- seq(from = as.POSIXct("2016-12-01 00:00"), 
                  to = as.POSIXct("2016-12-31 23:59"), by = "min")
ts_bin <- xts(df_clean$bin, order.by = time_index)
ts_bin <- xts(df_clean$period_posted, order.by=dates)
ts_bin = ts(df_clean$bin, start=c(01,12,2016), end=c(31,12,2016))
plot(ts_bin)
fit <- stl(ts_bin)
plot(fit)

#ts_bin = data.frame(df_clean$period_posted, df_clean$bin)
#colnames(ts_bin)[colnames(ts_bin)=="df_clean.period_posted"] <- "period_posted"
#colnames(ts_bin)[colnames(ts_bin)=="df_clean.bin"] <- "bin"
#ts_bin = as.ts(ts_bin)
head(ts_bin)
ts.plot(ts_bin)
plot(decompose(ts_bin))