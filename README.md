# project-sribu-analysis
Sribu is an Indonesian online platform to connect people who need graphic design services and community of designers from all around the world.
I used this code to create my analysis in [my blogpost](http://pandutruhandito.tumblr.com/post/111245480179/how-designers-irrationality-and-inertia-make-up "How Designers’ Irrationality & Inertia Make Up @Sribudotcom’s Success").

Files included in this repository are:
+ sribu_crawl.R is the script to scrape projects details on Sribu.com. It produces a data frame called Sribu containing 9 variables and 2292 observations
+ sribu_res.R is the script used for analysis. It is not a clean script in the sense that it only produces data that you need. It is pretty much the raw script I used in this exploration
+ sribu.RData is the data collected by sribu_crawl.R. Warning: it does NOT contain the sribu dataframe, you still need to create it

More details about this project can be read in the link above.

