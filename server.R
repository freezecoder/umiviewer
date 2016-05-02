library(data.table)
library(ggplot2)
library(scales)
library(d3heatmap)
library(rCharts)
library(shinyjs)

#genes=readRDS("allgenes.rds")

shinyServer(function(input, output, session) {

  #Data import and manipulation functions
  source("getData.R",local=TRUE)
  source("mappedreadscatter.R",local=TRUE)
  source("mapped_reads_view.R",local=TRUE)
  source("boxplot_view.R",local=TRUE)
  source("views.R",local=TRUE)
  source("canalqc.R",local=TRUE)
  source("heatmap_view.R",local=TRUE)
  source("stat_summary_view.R",local=TRUE)
  
  
 
  #GEne/DNA tag filters in UI
  output$selui<-renderUI({
    dat=getTable()
    colnms= getColNames()
    fluidRow(
       selectInput("gene","Filter/Select Genes to show:",multiple=T,selected=NULL,as.character(dat$GENE)),
        selectInput("tags","Filter/Select Barcodes to show:",multiple=T,selected=NULL,colnms)
      )
  })

#Hide/show toggle control bar 
shinyjs::onclick("toggleAdvanced",shinyjs::toggle(id = "cppanel", anim = TRUE)) 
	
#High level summary of numbers, what's going on e.g. no. genes, tags, etc
output$tagcount<-renderText({
		dat=filTable()
		if (length(dat)>0) {
		nonprobes=0
		humanprobes=0
		tagcount=length(names(dat))-1
		genecount=length(dat$GENE)
		humanprobes=length(grep("HUMAN",dat$GENE))
		nonprobes=length(grep("HUMAN",dat$GENE,invert=TRUE))
		if (tagcount)
			sprintf("%s DNA tags, %s Genes, %s Human Genes, %s Non-Human Genes",tagcount,genecount,humanprobes,nonprobes)
		}else {
			"0 Tags"
		}
	})

	
 })
