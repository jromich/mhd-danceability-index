##################################################
#
# Danceability Index
#
# This code attempts to create a trading model based
# on a danceability rating of the most popular music in
# NYC on a rolling basis.  Popularity is from LastFM
# and danceability is a score of those songs from the
# Echo Nest.
#
# The api functions below were inspired by the
# RLastFM package.
#
###################################################


  #library(RLastFM)
  library(xts)
  library(quantmod)
  library(PerformanceAnalytics)

  # input API keys here
  lastkey <- "xxxxxxxxxxxxxxxxxxxxxxxx"
  echoNestKey <- "xxxxxxxxxxxxxxxxxxxx"

  # how many songs to look at for each week
  topN <- 3
  
  # moving average window
  k <- 5

  ###################################################################
  # functions for retrieving LastFM and Echo Nest data (based on
  # code from the RLastFM package)
  ###################################################################

  getDate <- function(a) as.Date(as.POSIXct(a, origin="1970-01-01", tz="GMT"))

  baseurl.LastFM <- "http://ws.audioscrobbler.com/2.0/"

  geo.getMetroWeeklyChartlist <- function (tag, key = lastkey, parse = TRUE){
      ## Get a list of available chart periods for this metro, expressed as date
      ## ranges which can be sent to the chart services.
      params = list(method = "geo.getmetroweeklychartlist", tag = tag,
          api_key = key)
      ret = getForm(baseurl.LastFM, .params = params)
      doc = xmlParse(ret, asText = TRUE)
      if (parse)
          doc = p.geo.getmetroweeklychartlist(doc)
      return(doc)
  }

  p.geo.getmetroweeklychartlist <- function(a) {
      ## parse chart list
      t(xpathSApply(a, "//chart", xmlAttrs))
  }

  getMetroUniqueTrackChart <-function (country, metro, start = NA, end = NA, key = lastkey, parse = TRUE) {
      ## Get a chart of tracks for a metro
      params = list(method = "geo.getmetrouniquetrackchart", country = country, metro=metro, start = start, end = end, api_key = key)
      params = params[!as.logical(lapply(params, is.na))]
      ret = getForm(baseurl.LastFM, .params = params)
      doc = xmlParse(ret, asText = TRUE)
      if (parse)
          doc =p.getMetroUniqueTrackChart(doc)
      return(doc)
  }

  p.getMetroUniqueTrackChart = function(a) {
    ## parse chart
    list(track = xpathSApply(a, "//track/name", xmlValue),
         artist = xpathSApply(a, "//track/artist/name", xmlValue),
         artmbid = xpathSApply(a, "//track/artist/mbid", xmlValue))
  }

  getEchoSong_name <- function (combined, key = echoNestKey, parse = TRUE) {
    ## get song info from echonest and return a data.frame with tempo, energy, 
    ## danceability, and loudness
    params=list(api_key=key, combined=combined, results=1, bucket="audio_summary", format="xml")
    d <- getForm("http://developer.echonest.com/api/v4/song/search", .params=params)
    doc = xmlParse(d, asText=T)
    data.frame(combined=combined, tempo = xpathSApply(doc, "//tempo", xmlValue),
         energy = xpathSApply(doc, "//energy", xmlValue),
         danceability = xpathSApply(doc, "//danceability", xmlValue),
         loudness = xpathSApply(doc, "//loudness", xmlValue), stringsAsFactors=F)
  }


  ###################################################################
  # get data from LastFM and Echo Nest
  ###################################################################

  # get date ranges
  dates <- geo.getMetroWeeklyChartlist("United States")

  # look up charts for all date ranges (takes a while due to api calls)
  chartDates <- list()
  for ( i in 1:nrow(dates) ){
    chartDates[[i]] <- getMetroUniqueTrackChart("United States", "New York", start = dates[i,1], end = dates[i,2], parse=T)
  }

  # get top N tracks for each date range
  alltracks <- unique(do.call(rbind, lapply(chartDates, function(a) data.frame(a$artist, a$track)[1:topN,] ) ))
  # concatenate artist names and track name
  alltracks <- sort(apply(alltracks, 1, paste, collapse=" "))

  # get echonest song attributes (incl. danceability)
  # use a for loop so that it can be restarted from i if the API rate limit 
  # stops the process
  #songScores <- lapply(alltracks,  getEchoSong_name)
  songScores <- list()
  for (i in 1:length(alltracks)){
    songScores[[i]] <- getEchoSong_name(alltracks[[i]])
    cat(paste("Retrieving:", i, "of", length(alltracks), "-", alltracks[[i]], "\n"))
    Sys.sleep(3)
  }
 
  songScoresDF <- do.call(rbind, songScores)
  songScoresDF[,-1] <- apply(songScoresDF[,-1], c(1,2), as.numeric)
  rownames(songScoresDF) <- alltracks

  ###################################################################
  # create index
  ###################################################################

  getMeanScore <- function(a) {
    # create a key (artist track) and use that to extract song scores and create a mean score
    combined <- data.frame(a$artist, a$track, stringsAsFactors=F)[1:topN,]
    combined <- apply(combined, 1, paste, collapse=" ")
    scores <- songScoresDF[combined,-1]
    apply(scores, 2, mean, na.rm=T)
  }

  # get average score for the popular songs for each date range
  scores <- do.call(rbind, lapply(chartDates, getMeanScore))
  scores <- xts(scores, order.by=getDate(as.numeric(dates[,2])))
  scores <- Reclass(na.locf(na.locf(scores), fromLast=T)) #fill in any gaps

  # create moving averages of scores
  scoreMA <-  as.xts(apply(scores, 2, rollmean, k))

  x11(height=5, width=8, pointsize=9)
  plot(scoreMA[,"danceability"], main="Danceability of Top Songs", ylab="Echo Nest Danceability Score (5 week moving average)")
  savePlot("danceabilty ts", "png")

  # get SPX data
  getSymbols("^GSPC")
  spxRet <- ClCl(GSPC)
  scoreMA <- merge(GSPC$GSPC.Adjusted, scoreMA)
  colnames(scoreMA)[1] <- "SPX"
  scoreMA$SPX <- na.locf( scoreMA$SPX )
  scoreMA <- na.omit(scoreMA)
  scoreMA$"SPX Return" <- lag(scoreMA$SPX / lag(scoreMA$SPX) - 1, -1)

  # plot SPX Return versus danceability score bins
  boxplot(as.numeric(scoreMA$"SPX Return") ~ cut(scoreMA$danceability, br=seq(0.4,.7,.05)), main="Returns by Danceability Intervals", ylab="Return", xlab="Mean Danceability Score");
  abline(h=0, col="blue", lty="dashed")
  savePlot("boxplot", "png")

  # create trading rule (sell if indicator over 0.6)
  pos <- ifelse(scoreMA$danceability > 0.60, -1, 1)
  ret <- pos * scoreMA$"SPX Return"
  names(ret) <- "Danceability Index"

  charts.PerformanceSummary(merge(ret, scoreMA$"SPX Return"),cex.legend=1, main="Cumulative Returns: Dancebility Index", ylog=T)
  savePlot("cum plot", "png")

  table.Returns(ret)
  table.Stats(ret)
  monthly <- na.omit(apply.monthly(ret, function(a) prod(a+1)-1))
  table.AnnualizedReturns(monthly)
  cor(ret, scoreMA$"SPX Return", use="complete.obs")
  min(Drawdowns(ret), na.rm=T)

  x11(height=5, width=8, pointsize=9)
  chart.TimeSeries(cumprod(scoreMA$"SPX Return"+1), col="blue", lwd=2, main="S&P 500 Index", ylab="Returns for a long only SPX investment")
  savePlot("sp", "png")

  # test monthly relationships
  # scoreMonthly <- apply.monthly(scoreMA, mean)
  # plot(coredata(scoreMonthly$"SPX Return") ~ coredata(scoreMonthly$tempo))


