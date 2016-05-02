

#heatmap View
output$heatmap<-renderD3heatmap({
    dat=filTable()
    topn=100
    rn=dat$GENE
    dat=subset(dat,select=-GENE)
    row.names(dat)=rn
   # saveRDS(dat,"sampledat.rds")
    gcount=length(rownames(dat))
    dat=dat[rowSums(dat)>input$mingexp,]
    if (gcount >100) {
      d3heatmap(head(dat,topn),labRow=head(rownames(dat),topn),color="Blues",dendrogram="row")
    }else {
      d3heatmap(dat,labRow=rn,dendrogram="row",color="Blues")
    }
    
   
  })
  
output$ui_heatmap <- renderUI({
		d3heatmapOutput("heatmap")  
  })
  