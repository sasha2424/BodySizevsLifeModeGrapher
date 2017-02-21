# BodySizevsLifeModeGrapher
Creates graphs of body size versus number of unique life modes for marine organisms over different time periods and over different phyla.
In the code the value of "GRAPH" can be changed to graph different Phyla or different time periods
For the following values:

0 - Arthropods
1 - brachiopods
2 - Chordates
3 - Molluscs
4 - All data base data
5 - Different time periods
6 - Combined graph of only 4 main phyla

For setting 5 you also need to specify a start and end time in millions of years:
ex.
StartAge <- 650
EndAge <- 500

The data base only has value going up to around 650 MYA.
Graphing very small time periods may result in funky graphs due to lack of data.

The number of bins can be set to any value but values less than 30 will look weird and large values will take significantly longer to graph.
ex.
NoB <- 150

The max organism size to be considered is decided by default to be everything less than the larger outliers.
A custom max value can be set by:

AM <- 1000000

Where the first letter correspods to the graph type
Set value to NA to use default max calculations

**** ADVANCED ****
To grab the first X percent of the data use the following line of code:
XXX <- .75
AM <- signif(quantile(Spec$max_vol,XXX,na.rm = TRUE),SIG)
******************


The SIG variable is used to regulate the precision of the X-axis label.
if SIG is set to 2 then you may get bin sizes of 2300 or 450 (with 2 sigfigs)
default is SIG <- 1


Contantact me if you need a better description!
sashaivanov2424@gmail.com
