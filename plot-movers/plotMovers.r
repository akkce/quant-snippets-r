plotMovers <- function(mdt=as.Date(Sys.Date()), display=TRUE, save=TRUE) {
	
	# verify install / load svglite - used for writing out the scalable plots
	if( ! require(svglite) ) {
		install.packages("svglite", repos='http://cran.us.r-project.org')
		library("svglite")
	}

	# verify install / load  - ggplot2 used for making pretty plots
	if ( ! require(ggplot2) ) {
		install.packages("ggplot2", repos='http://cran.us.r-project.org')
		library("ggplot2")
	}

	# verify install / load - used for scaling
	if ( ! require(scales) ) {
		install.packages("scales", repos='http://cran.us.r-project.org')
		library("scales")
	}


	source('analyzeMovers.r')
	movers <- analyzeMovers(mdt)


	# plot positive movers
	p.pos <- ggplot(data=movers$posMovers, mapping=aes(adv, rdiff, label=tic))
	p.pos <- p.pos + scale_y_continuous(trans=log10_trans(), labels=function(x) {scales::percent(x, 0.1)}) + scale_x_continuous(trans=log10_trans(), labels=function(x) {round(x/1000000, 0)})
	p.pos <- p.pos + geom_text(check_overlap=T, size=4)

	p.pos <- p.pos + labs(x='20 day mean volume $MM', y="Today's excess return", title='Good news stocks')

	if ( display == TRUE ) {
		dev.new()
		print(p.pos)
	}

	if ( save == TRUE ) {
		ggsave(paste("plots/", mdt, "-pos.svg", sep=''), p.pos, device='svg')
	}
	
	



	# plot negative movers
	p.neg <- ggplot(data=movers$negMovers, mapping=aes(adv, -rdiff, label=tic))
	p.neg <- p.neg + scale_y_continuous(trans=log10_trans(), labels=function(x) {scales::percent(x, 0.1)}) + scale_x_continuous(trans=log10_trans(), labels=function(x) {round(x/1000000, 0)})
	p.neg <- p.neg + geom_text(check_overlap=T, size=4)

	p.neg <- p.neg + labs(x='20 day mean volume $MM', y="- Today's excess return", title='Bad news stocks')

	if ( display == TRUE ) {
		dev.new()
		print(p.neg)	
	}

	if ( save == TRUE ) {
		ggsave(paste("plots/", mdt, "-neg.svg", sep=''), p.neg, device='svg')
	}



}