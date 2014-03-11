mhd-danceability-index
======================

This was an R script used in the Boston [musichackday.org](Music Hackday) for a danceability market index for investing.  The idea was to create something similar to the [hemline index](http://en.wikipedia.org/wiki/Hemline_index) using the danceability rating of songs from Echo Nest.  

Musichackday is a weekend hackathon for music tech and the idea is to create something fun related to music.  This project was for fun, not real investing, and is obviously no longer profitable!

For write-ups see:

[Wired.com article]( http://goo.gl/iPvyZO)

[evolver.fm post](http://goo.gl/MmqYEG)

The process:

* use Last FM to find the most popular songs in NYC historically of a period of time

* use the Echo Nest API to get the danceability rating of each song

* create a moving average of how danceable the popular music was in NYC at each time period

* data mine (overfit) to find profitable trading rule (this is not a serious endeavor)

* simulate trading on S&P 500 going long when the indicator is above a threshold and short when below

* profit

