source('functions.R')
x <- data.frame(Date=as.Date('2025-01-01')+0:4, Adjusted=c(1,2,2,3,4))
r <- calculate_statistics(x,3)
stopifnot(r$summary$Mean==2.4, r$summary$Median==2,
          r$summary$Mode=='2.00', abs(r$summary$SD-sqrt(1.3))<1e-12,
          all(is.na(r$series$SMA[1:2])), abs(tail(r$series$SMA,1)-3)<1e-12,
          identical(price_mode(c(1,1,2,2)),c(1,2)),length(price_mode(1:5))==0)
stopifnot(inherits(try(calculate_statistics(x,0),silent=TRUE),'try-error'),
          inherits(try(load_stock_data('absent.txt'),silent=TRUE),'try-error'))
cat('PASS: mean, median, mode ties, absent mode, SD, SMA and invalid inputs\n')
