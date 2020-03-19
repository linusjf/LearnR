[How contagious is it?](https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov)

[Corona Virus: Data analysis](https://rdatamining.wordpress.com/2020/03/10/coronavirus-data-analysis-with-r-tidyverse-and-ggplot2/)

[Cumulative fatality rate](http://freerangestats.info/blog/2020/03/17/covid19-cfr)

[Flattening the curve](http://staff.math.su.se/hoehle/blog/2020/03/16/flatteningthecurve.html)

I've collated a few R scripts that analyse the CoronaVirus outbreak using data from the [John Hopkins University repository on GitHub.](https://github.com/CSSEGISandData/COVID-19)


The relevant scripts are worldanalysis.R, plotgrowth.R and the SIRModels. 

The SIRModel simulates the growth of the virus if unchecked. 
It's not gospel but a useful, rudimentary predictor.  

Links to the source articles are above. 

You will need R and Latex (tex-live, tex-live-extras) installed on your machine and a decent internet connection.

Run the worldanalysis.R first;its output is input to the other scripts mentioned above.

(Execute installer.R to download packages needed to run the scripts. 
That's under the root directory, LearnR. 
It helps if you're already familiar with git and GitHub.

The installer.R takes time. 
To save time, you can pull out the install_coronavirus method and run only that. 
That will install all packages needed for the above scripts.)

[Installing Latex](https://www.latex-tutorial.com/installation/)

[Installing R](https://www.datacamp.com/community/tutorials/installing-R-windows-mac-ubuntu)
