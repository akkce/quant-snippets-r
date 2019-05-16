# analyzeMovers - compute the moving stocks in a given day
analyzeMovers <- function(mdt=as.Date(Sys.Date())) {

	source('getMovers.r')

	# get the required data necessary
	r3.movers <- getMovers(mdt)

	# remove any rows with missing data ('NA's)
	print(paste("Removing ", nrow(r3.movers) - nrow(na.omit(r3.movers)), " rows with missing data...", sep=''))
	r3.movers <- na.omit(r3.movers)

	# remove any rows that zero or smaller adv values (average daily dollar volume over the previous 20 days)
	missing.advs <- r3.movers[r3.movers$adv <= 0, "tic" ]
	print(paste("Removing ", length(missing.advs), " rows with missing ADDV...", sep=''))
	
	r3.movers <- r3.movers[!(r3.movers$tic %in% missing.advs), ]

	print(dim(r3.movers))
	print(r3.movers[1:10,])


	# analyze top movers via Local Polynomial Regression Fitting
	r3.movers.lo <- loess(log(dv) ~ log(adv), data=r3.movers, family='symmetric')
	print(r3.movers.lo)

	# extract the residuals
	r3.movers.resids <- resid(r3.movers.lo)
	print(paste("Number of extracted residuals: ", length(r3.movers.resids), sep=''))

	# get median total return
	r3.median.return <- median(r3.movers$r)
	print(paste("Median return: ", r3.median.return, sep=''))

	# get returns difference from median
	r3.movers$rdiff <- r3.movers$r - r3.median.return

	# quantile 25/75 the return difference and residuals
	r3.movers.rdiff.q <- quantile(r3.movers$rdiff, probs=c(0.25, 0.75))
	r3.movers.resids.q <- quantile(r3.movers.resids, probs=c(0.25, 0.75))

	r3.movers.rdiff.q.25 <- r3.movers.rdiff.q[1]
	r3.movers.rdiff.q.75 <- r3.movers.rdiff.q[2]

	r3.movers.resids.q.25 <- r3.movers.resids.q[1]
	r3.movers.resids.q.75 <- r3.movers.resids.q[2]

	r3.movers$rsd <- r3.movers.resids


	# positive movers are in the top quantile
	r3.movers.pos <- r3.movers[(r3.movers$rdiff > r3.movers.rdiff.q.75) & (r3.movers$rsd > r3.movers.resids.q.75), ]
		
	# negative movers are in the bottom quantile
	r3.movers.neg <- r3.movers[(r3.movers$rdiff < r3.movers.rdiff.q.25) & (r3.movers$rsd > r3.movers.resids.q.75), ]

	return(list('posMovers'=r3.movers.pos, 'negMovers'=r3.movers.neg))


}
