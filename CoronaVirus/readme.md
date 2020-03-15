[How contagious is it?](https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov)
[Corona Virus: Data analysis](https://rdatamining.wordpress.com/2020/03/10/coronavirus-data-analysis-with-r-tidyverse-and-ggplot2/)

For those technically oriented, I've collated a few R scripts from the internet that track the CoronaVirus outbreak using data from the John Hopkins Hospital repository on GitHub. 
The relevant scripts are worldanalysis.R, plotgrowth.R and the SIRModels. 
The SIRModel simulates the growth of the virus if unchecked. 
It is not gospel but it's a useful but rudimentary predictor.  
The links to the articles are above. 
You will need R, tex-live, tex-live-extras installed on your machine and a decent internet connection. 
Run the worldanalysis.R first since it's output is required for the other two scripts.

Run installer.R before trying the scripts to download the packages needed to run the scripts. 
That's under the root directory LearnR. 
It helps if you're already familiar with git and GitHub.

The installer.R can take some time. 
If you're inclined, you can pull out only the install_coronavirus method and run only that to save time. 
That will install all the packages needed for the above scripts.
