library(RPostgres)

connect <- function() {
  dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu',
            port=9737,
            dbname='wrds',
            sslmode='require',
            user='rk540') 
}

getnbbo_mkthrs <- function(wrds, ticker, date) {
  res <- dbSendQuery(wrds, paste0("select * from taqmsec.nbbom_", date, " where sym_root = '", ticker, "' and bid > 0 and ask > 0 and time_m >= '09:30:00' and time_m <= '16:00:00'"));
  data <- dbFetch(res,n=-1);
  dbClearResult(res);
  data
}

getnbbo <- function(wrds, ticker, date) {
  res <- dbSendQuery(wrds, paste0("select * from taqmsec.nbbom_", date, " where sym_root = '", ticker, "' and bid > 0 and ask > 0"));
  data <- dbFetch(res,n=-1);
  dbClearResult(res);
  data
}

getnbbo_mid_spread <- function(wrds, ticker, date) {
  res <- dbSendQuery(wrds, paste0("select * from taqmsec.nbbom_", date, " where sym_root = '", ticker, "' and bid > 0 and ask > 0"));
  data <- dbFetch(res,n=-1);
  dbClearResult(res);
  data
}

getnbbo_mkthrs_mid_spread <- function(wrds, ticker, date) {
  res <- dbSendQuery(wrds, paste0("select *, .5*(bid + ask) as mid, (ask - bid) as spread from taqmsec.nbbom_", date, " where sym_root = '", ticker, "' 
                                  and bid > 0 and ask > 0 and ask > bid and time_m >= '09:30:00' and time_m <= '16:00:00'"));
  data <- dbFetch(res,n=-1);
  dbClearResult(res);
  data
}

returns <- function(ts) {
  nrows <- NROW(ts);
  t1 <- ts[1:(nrows-1)];
  t2 <- ts[2:nrows];
  log(t1/t2)
}

volatility <- function(ts, days) {
  tsl <- NROW(ts);
  nvol <- tsl - days;
  ret <- seq(1, nvol);
  
  for(i in 1:nvol) {
    vecend = i + days;
    ret[i] = sd(ts[i:vecend]);
  }
  
  ret
}

get_taq_quotes_cleaned <- function(wrds, ticker, date) {
  quotes = getnbbo_mkthrs_mid_spread(wrds, ticker, date);
  mutate(quotes, DT = as.POSIXct(time_m), BIDSIZ=bidsiz, OFRSIZ=asksiz, NT=as.numeric(time_m)) %>% data.table::as.data.table() %>% mergeQuotesSameTimestamp(selection="weighted.average")
}

get_taq_trades_cleaned <- function(wrds,ticker, date) {
  res <- dbSendQuery(wrds, paste0("select * from taqmsec.ctm_", date, " where sym_root = '", ticker, "' and price > 0 and time_m >= '09:30:00' and time_m <= '16:00:00'"))  
  data <- dbFetch(res, n=-1)
  dbClearResult(res)
  mutate(data, DT=as.POSIXct(time_m), NT=as.numeric(time_m)) %>% data.table::as.data.table() %>% mergeTradesSameTimestamp(selection="weighted.average")
}
