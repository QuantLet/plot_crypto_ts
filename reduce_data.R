library(data.table)
library(lubridate)
library(xts)

setDTthreads(percent = 100)

## ##
setwd("~/Documents/QuantLet/plot_crypto_ts")
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

dshusd <- fread("./data/dshusd.csv")
ltcusd <- fread("./data/ltcusd.csv")
xmrusd <- fread("./data/xmrusd.csv")
DTs <- list(dshusd,ltcusd,xmrusd)
names(DTs) <- c("dshusd", "ltcusd", "xmrusd")

DT_aggregate <- rbindlist(DTs, idcol = "s" )

# get IDs
list_id <- sort(unique(DT_aggregate$id))

# get currencies per exchanges
list_s <- lapply(list_id, function(x) sort(unique(DT_aggregate[id %in% x]$s)))
names(list_s) <- list_id

# preallocate list for the to be created reduced datasets
DT_list <- lapply(1:length(list_s), function(x) vector(mode = "list", length = length(list_s[[x]])))
names(DT_list) <- list_id
for (i in 1:length(DT_list)) {names(DT_list[[i]]) <- list_s[[i]]}


for (i in 1:length(list_id)) { for (j in 1:length(list_s[[i]])) {
print(Sys.time())
print(paste("Aggregating", list_id[[i]], list_s[[i]][[j]]))
DT <- DT_aggregate[id %in% list_id[[i]] & s %in% list_s[[i]][[j]]]

# create time variables
DT[,t := as.POSIXct(DT$t, origin="1970-01-01", tz="UTC", format = "%Y-%m-%dT%H:%M:%OSZ")]
DT[, date := as.Date(DT$t, tz="UTC", format= "%d-%m-%Y")]
DT[, hour := hour(DT$t)]
DT[, minute := minute(DT$t)]
DT[, second := second(DT$t)]

# counters 
count_h <- DT[, .N, hour][order(rank(hour))]
n_hour <- sort(unique(DT$hour))
n_minute <- sort(unique(DT$minute))
n_second <- sort(unique(DT$second))
n_date <- unique(DT[, date])

# reduce data for complete days 
DT <- DT[date >= n_date[2]]
n_date <- unique(DT[, date])

# reduce price time series to 5 minute intervals
ts_p <- xts(DT$p, order.by=DT$t, unique = FALSE)
ts_p <- align.time(ts_p,5*60)
ts_p <- to.minutes5(ts_p)
hist_p <- as.data.table(ts_p)

# reduce volume time series to 5 minute intervals
hist_q <- DT[, list(q=sum(q)), by=cut(t, "5 min")]
hist_q <- copy(hist_q)
hist_q[, index := as.POSIXct(hist_q$cut, tz = "UTC")]

# create a clean data set with 5 minute intervals ### DON'T FORGET TO ADJUST DATE IN ROW BELOW
start <- paste(n_date[1], "00:00:00")
end <- paste(n_date[(length(n_date)-1)], "23:55:00")
DTcl <- seq(as.POSIXct(start, tz="UTC"), as.POSIXct(end, tz="UTC"), by="5 mins")
DTcl <- as.data.table(DTcl)
DTcl[, date := as.Date(DTcl$x, tz="UTC", format= "%d-%m-%Y")]
names(DTcl) <- c("index", "date")

# merge clean data set with aggregated price and volume data
DTcl[hist_p, on = "index", p := i.ts_p.Close]
DTcl[hist_q, on = "index", q := i.q]
names(DTcl) <- c("t", "date", "p", "q")
DTcl <- DTcl[2:nrow(DTcl)]

# add DT to list and write file
DT_list[[i]][[j]] <- DTcl
x <- paste("./data/agg_files/",list_id[i], "_", names(DT_list[[i]])[j], ".csv", sep ="")
fwrite(DTcl, file = x)
writeLines(c("Successful! Proceeding to next iteration", ""))
}}
save(DT_list, file = "./data/agg_files/DT_list.RData")

