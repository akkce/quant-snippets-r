# verify install / load Rblpapi package (R Bloomberg Terminal Core API)
if( ! require(Rblpapi) ) {

	install.packages("Rblpapi")
	library("Rblpapi")
}


# connect to Bloomberg Terminal Core API
con <- blpConnect(
	host = 'localhost',
	port = '8194')

# build a map of the portfolio names to their numbers. The portfolio numbers are availabe in the PRTU screen.
# the order in which these columuns in resutling data frame is important so that the appropriate number matches
# its respective portfolio name. If you add or remove funds, please ensure the coresponding num is removed and
# the correct order is retained to prevent offset errors.

port.ids <- data.frame ( num = c( "xx", "xx" ),
						fund = c("fund1", "fund2"),
						stringsAsFactors = FALSE
					)


port.data <- data.frame( dt = as.character(),
						 prtunum = as.character(), 
						 fund = as.character(),
						 isin = as.character(), # ID_ISIN
						 cusip = as.character(), # IS_CUSIP
						 tic = as.character(), # TICKER
						 position = as.numeric(), #PORTFOLIO_MPOSITION
						 weight = as.numeric(), # PORTFOLIO_MWEIGHT
						 px_last = as.numeric(), # PX_LAST
						 px_yest_cld = as.numeric(), # PX_YEST_CLOSE
						 px_prcent_chg = as.numeric(),
						 market_val = as.numeric(),
						 curr = as.character(), # PORTFOLIO_BASE_CURRENCY
						 name = as.character(), #NAME
						 portname = as.character(), # PORTFOLIO_NAME
						 secdesc = as.character(), # MARKET_SECTOR_DES
						 sector = as.character(), # GICS_SECTOR_NAME
						 industry = as.character(), # GICS_INDUSTRY_NAME
						 type = as.character(), # SECURITY_TYP
						 benchmark = as.character(), # PORTFOLIO_BENCHMARK 
						 last_update_dt = as.character(), # LAST_UPDATE_DT
						 last_update_time = as.character(), # TIME
						 stringsAsFactors = FALSE 
						 )

# start timestamp of Bloomberg API pulls
start.time = Sys.time()

for (num in port.ids$num) {

	print(paste("Pulling PRTU data for ", "TS-CLIENTID-", num, " ...", sep=""))
		
	# Get current fund name to match to prtu id
	port.fund.tmp <- port.ids[port.ids$num == num, "fund"]

	# getPortfolio won't allow multiple fields to be retrieved each call so we have use individual calls and data frames
	# to collect all our desired data and then match things up and append them to port.data. This is probably a pretty
	# dumb approach.

	# get current portfolio position holdings and re-order by security ticker (AAPL US Equity)
	port.tmp <- getPortfolio(paste("TS-CLIENTID-", num, " Client", sep=""), "PORTFOLIO_MPOSITION")
	port.tmp <- port.tmp[order(port.tmp$Security), ]

	# get current portfolio position weights and merge with existing data frame
	port.tmp <- merge(port.tmp, getPortfolio(paste("TS-CLIENTID-", num, " Client", sep=""), "PORTFOLIO_MWEIGHT"), by="Security")

	# make a bdp call to get various descriptive fields - see https://github.com/Rblp/Rblpapi/issues/280 for details
	port.bdp <- bdp(port.tmp$Security, fields = c( "ID_ISIN",
												   "ID_CUSIP",
												   "TICKER",
								  				   "PX_LAST", 
								  				   "PX_YEST_CLOSE",
								  				   "NAME",
								  				   "MARKET_SECTOR_DES",
								  				   "GICS_SECTOR_NAME",
								  				   "GICS_INDUSTRY_NAME",
								  				   "SECURITY_TYP",
								  				   "LAST_UPDATE_DT",
								  				   "LAST_UPDATE"))

	port.bdp$Security <- rownames(port.bdp)
	port.tmp <- merge(port.tmp, port.bdp, by="Security")

	# get descriptive portfolio fields and merge
	port.bdp2 <- bdp(paste("TS-CLIENTID-", num, " Client", sep=""), c("PORTFOLIO_NAME",
																	"PORTFOLIO_BENCHMARK",
																	"PORTFOLIO_BASE_CURRENCY") )
	port.tmp <- merge(port.tmp, port.bdp2)

	# bind to data frame
	port.data <- rbind(port.data, data.frame(
		dt = as.Date(Sys.Date(), format = "%Y-%m-%d"),
		prtunum = num,
		fund = port.fund.tmp,
		isin = port.tmp$ID_ISIN,
		cusip = port.tmp$ID_CUSIP,
		tic = port.tmp$TICKER,
		position = port.tmp$Position,
		weight = port.tmp$Weight,
		px_last = port.tmp$PX_LAST,
		px_yest_cld = port.tmp$PX_YEST_CLOSE,
		px_prcent_chg = ( port.tmp$PX_LAST - port.tmp$PX_YEST_CLOSE ) / port.tmp$PX_YEST_CLOSE,
		market_val = port.tmp$Position * port.tmp$PX_LAST,
		curr = port.tmp$PORTFOLIO_BASE_CURRENCY,
		name = port.tmp$NAME,
		portname = port.tmp$PORTFOLIO_NAME,
		secdesc = port.tmp$MARKET_SECTOR_DES,
		sector = port.tmp$GICS_SECTOR_NAME,
		industry = port.tmp$GICS_INDUSTRY_NAME,
		type = port.tmp$SECURITY_TYP,
		benchmark = port.tmp$PORTFOLIO_BENCHMARK,
		last_update_dt = format(port.tmp$LAST_UPDATE_DT, format="%Y-%m-%d"),
		last_update_time = port.tmp$LAST_UPDATE,
		stringsAsFactors = FALSE
		))
}



# close progress bar and calculate API pull time
end.time = Sys.time()
print(paste("Bloomberg API call:", end.time - start.time))