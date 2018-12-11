# verify install / load Rblpapi package (R Bloomberg Terminal Core API)
if( ! require(Rblpapi) ) {

	install.packages("Rblpapi")
	library("Rblpapi")
}

# connect to Bloomberg Terminal Core API
con <- blpConnect(
	host = 'localhost',
	port = '8194')

# pull a count of the all the Russell 3000 index members
count <- bdp("RAY Index", "COUNT_INDEX_MEMBERS")
print(paste("RAY Index Members:", count))

# INDX_MEMBERS is "batched" out in groups of 2500. We need to combine INDX_MEMBERS and INDX_MEMBERS2
tickers <- c(bds("RAY Index", "INDX_MEMBERS")[,1], bds("RAY Index", "INDX_MEMBERS2")[,1])
print(tickers)

# compare the length of tickers to total number of members before we proceed
if (length(tickers) == count$COUNT_INDEX_MEMBERS) {

	start.time <- Sys.time()

	r3000.df <- bdp(paste(tickers[1:count$COUNT_INDEX_MEMBERS], " Equity", sep=''), fields=c(
			"TICKER",
			"VOLUME", 
			"CURRENT_TRR_1D", 
			"LAST_CLOSE_TRR_1D",
			"PX_LAST",
			"PX_YEST_CLOSE"
			)
		)

	end.time <- Sys.time()

	# print(paste0("Bloomberg BDP call completed in: ", (end.time - start.time), " seconds" ) )

}


# add the current date to the dataframe
cur.date <- format(Sys.Date(), format="%Y-%m-%d")
r3000.df$DATE <- cur.date

# calculate API pull time
print(paste("Bloomberg API call:", end.time - start.time, "seconds"))