
#Introduction

A viewer for UMI and metrics file from DropSeq analysis. The results of the canal single-cell RNASeq pipeline are used as input to this dashboard viewer. This code uses the shiny framework ( http://shiny.rstudio.com).

It is highly recommended that you launch this package using Rstudio (http://www.rstudio.com) (Windows/Mac) if you do not have access to a shiny server installation.

# Background

Single-cell RNASeq data requires special bioinformatics processing. See the BCBIO example [pipeline](http://bcbio-nextgen.readthedocs.io/en/latest/contents/pipelines.html#single-cell-rna-seq) for how to deal with this data.

# Dependencies

install dependencies using the R console or RStudio:

```
install.packages(c("shiny","shinyjs","rCharts","rjson","d3heatmap","ggplot2","data.table"))
```

You may need to be root/sudo if not using Rstudio.

# Running

Set the path of your data folder in global.R  i.e. change the line using a text editor of your choice.

```
#global.R
#must contain this line that's a  valid folder path to your data
topdir="yourfolderpath"
```

Save the file with these changes. 

Once this is set, you can launch in Rstudio using:


```
runApp("/path/to/this/code",port="2333",launch.browser=TRUE)
```

or you can install the App in a shiny server folder and launch from there.

#Input data

The path has to be valid folder on wherever the app machine is running e.g. on Mac /Users/Username/blabl/



