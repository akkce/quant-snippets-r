plotMovers <- function(mdt=as.Date(Sys.Date()), display=TRUE, save=TRUE, email=FALSE) {
	
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



	if ( email == TRUE ) {

		ggsave(paste("plots/", mdt, "-pos.svg", sep=''), p.pos, device='svg')
		ggsave(paste("plots/", mdt, "-neg.svg", sep=''), p.neg, device='svg')

		# verify install / load RDCOMClient - used for accessing Windows COM objects from within R
		if( ! require(RDCOMClient) ) {
			install.packages("RDCOMClient", repos = "http://www.omegahat.net/R") 
			library("RDCOMClient")
		}

		outlook.app <- COMCreate("Outlook.Application")
		
		outlook.mail <- outlook.app$CreateItem(0)

		outlook.mail[["To"]] <- "kevin.elliott@alaska.gov, mark.t.moon@alaska.gov, kevin.liu@alaska.gov, stephanie.pham@alaska.gov, josh.mclin@alaska.gov, kekama.tuiofu@alaska.gov"
		outlook.mail[["subject"]] <- "Russell 3000: Moving Stocks Plots"
		outlook.mail[["body"]] <- paste("Attached: Good news stocks (", mdt, "-pos.svg) and bad news stocks (", mdt, "-neg.svg)", sep='')

		path <- getwd()
		path <- gsub("\\/", "\\\\", path)

		outlook.mail[["Attachments"]]$Add(paste(path, "\\plots\\", mdt, "-pos.svg", sep=''))
		outlook.mail[["Attachments"]]$Add(paste(path, "\\plots\\", mdt, "-neg.svg", sep=''))

		outlook.mail$Send()

		if ( save != TRUE ) {
			file.remove(paste(path, "\\plots\\", mdt, "-pos.svg", sep=''))
			file.remove(paste(path, "\\plots\\", mdt, "-neg.svg", sep=''))
		}

	}


}