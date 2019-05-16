# getMovers: function to pull the current 1 day total return from Bloomberg's API and compare with average 20 day volume and returns that are pulled from ClariFi into
#	DORJNUTRSPRDATA's MySQL database (internal_equities)

getMovers <- function(mdt=as.Date(Sys.Date())) {

	# verify install / load Rblpapi package (R Bloomberg Terminal Core API)
	if( ! require(Rblpapi) ) {

		install.packages("Rblpapi", repos='http://cran.us.r-project.org')
		library("Rblpapi")
	}


	# connect to Bloomberg Terminal Core API
	blpapi.conn <- blpConnect(
		host = 'localhost',
		port = '8194')

	# verify our database connection before proceding
	if ( ! exists("blpapi.conn") ) {

		print("Unable to connect to Bloomberg Desktop API with Rblpapi.")

		mail.msg <- paste("Unable to connect to Bloomberg Desktop API with Rblpapi.", sep='')		
		quit()

	}


	# get Russell 3000 TRR from Bloomberg and write to temporary table

		# pull a count of the all the Russell 3000 index members
		count <- bdp("RAY Index", "COUNT_INDEX_MEMBERS")
		print(paste("RAY Index Members:", count))

		# INDX_MEMBERS is "batched" out in groups of 2500. We need to combine INDX_MEMBERS and INDX_MEMBERS2
		tickers <- c(bds("RAY Index", "INDX_MEMBERS")[,1], bds("RAY Index", "INDX_MEMBERS2")[,1])
		print(tickers)

		# compare the length of tickers to total number of members before we proceed
		if (length(tickers) == count$COUNT_INDEX_MEMBERS) {

			start.time <- Sys.time()

			r3000.trr <- bdp(paste(tickers[1:count$COUNT_INDEX_MEMBERS], " Equity", sep=''), fields=c(
					"TICKER",
					"VOLUME", 
					"CURRENT_TRR_1D", 
					"LAST_CLOSE_TRR_1D",
					"PX_LAST",
					"PX_YEST_CLOSE",
					"AVG_DAILY_VALUE_TRADED_20D",
					"VOLUME_AVG_20D"
					)
				)

			end.time <- Sys.time()	

		}

		print(paste("Bloomberg API call:", end.time - start.time, "seconds"))

		# add the current date to the dataframeVOLUME_VALUE
		r3000.trr$DATE <- format(Sys.Date(), format="%Y-%m-%d")

		# output part of r3000.trr data frame to console so we get some user input
		colnames(r3000.trr) <- c( "tic", "vol", "r", "trr_yest", "px", "px_yest", "adv", "avol", "dt" )
		
		r3000.trr$dv <- r3000.trr$vol * r3000.trr$px
		r3000.trr$r <- r3000.trr$r / 100

		r3000.trr[1:20,]


	r3.movers <- r3000.trr[, c( "tic", "vol", "dv", "avol", "r", "adv")]

	return(r3.movers)

}