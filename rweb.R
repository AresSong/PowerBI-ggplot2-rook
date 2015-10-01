# PowerBI-ggplot2-rook
# Rook example server to integrate ggplot2 output to Power BI
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



library(Rook)
require("hexbin")
require("XML")
require(grid)
require(ggplot2)
require(gridSVG)

# multiplot example http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# function to show an example single plot using qplot
singlePlot <- function() {

   d <- qplot(carat, data = diamonds, geom = "density", colour= color)
   multiplot(d, cols=1)
}


# function to show an example of multiple plots output from ggplot
multipleGraph <- function() {

p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")

# Second plot
p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=1) +
    ggtitle("Fitted growth curve per diet")

# Third plot
p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
    geom_density() +
    ggtitle("Final weight, by diet")

# Fourth plot
p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
    geom_histogram(colour="black", binwidth=50) +
    facet_grid(Diet ~ .) +
    ggtitle("Final weight, by diet") +
    theme(legend.position="none")    
multiplot(p1,p2,p3, p4, cols=2)

}
#initialize http port
myPort <- 80
myInterface <- "0.0.0.0"
status <- -1

# R 2.15.1 uses .Internal, but the next release of R will use a .Call.
# Either way it starts the web server.
if (as.integer(R.version[["svn rev"]]) > 59600) {
    status <- .Call(tools:::startHTTPD, myInterface, myPort)
} else {
    status <- .Internal(startHTTPD(myInterface, myPort))
}

if (status == 0) {
    unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    assign("httpdPort", myPort, environment(tools:::startDynamicHelp))

 # create http deamon
 s <- Rhttpd$new()
    s$listenAddr <- myInterface
    s$listenPort <- myPort
        
  # add end point for /custom/test for simple graph
  s$add(name = "test",app = 
	  function(env){
		singlePlot()
		#use gridSVG to export our plot to SVG
    mysvg <- grid.export(name=NULL, exportCoords="none", exportMappings="none", exportJS="none", uniqueNames = TRUE, prefix=  as.character(as.numeric(Sys.time())) )
		body = saveXML(mysvg$svg)
		list(
			    status = 200L,
			    headers = list(
 			  	'Content-Type' = 'text/xml',
				'Access-Control-Allow-Origin' =  '*'
    				),
    				body = body
		)
	  })

  # add end point for /custom/multiple for multiple graph
	s$add( name = "multiple", app =
		function(env){
                #use gridSVG to export our plot to SVG
		multipleGraph()
                mysvg <- grid.export(name=NULL, exportCoords="none", exportMappings="none", exportJS="none", uniqueNames = TRUE, prefix= as.character(as.numeric(Sys.time())) )
                body = saveXML(mysvg$svg)

                list(
                            status = 200L,
                            headers = list(
                                'Content-Type' = 'text/xml',
                                'Access-Control-Allow-Origin' =  '*'
                                ),
                                body = body
                )
          })    

    while (TRUE) Sys.sleep(24 * 60 * 60)
}

# If we get here then the web server didn't start up properly
warning("Oops! Couldn't start Rook app")
