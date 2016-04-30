
# Full Metrics for RNAseq data. Intended to be used with Drop seq and G&T seq experiments by the Satija Lab 
#Shiwei Zheng and Greg Brittingham
# created 12/3/15

# Set Species = 1 to print species specificity metrics to terminal and saved to working directory.
# example input: Metrics("GregsExperiment", Species = 1, PCT_MRNA = .5, NUM_Transcripts = 75)
# Make sure your working folder contains the .metrics.txt, .reads.txt, and the .qc.txt / .umi.txt files 
# from the SingleCellRNASeqMetricsCollector script (canal.pl pipeline and canal folder), the BAMTAGHistorgram script (in canal folder)
# and the canal.pl pipeline respectively.



plotMetrics = function(ExpName, Species = 1, PCT_MRNA = .4, NUM_Transcripts = 100){
  #Load files - use barcodes for row names for continuity
  Met = read.table(paste(ExpName, ".metrics.txt", sep="") , header=TRUE,sep="\t", row.names = 25)
  Qc = read.table(paste(ExpName, ".qc.txt", sep=""), header=TRUE,sep="\t", row.names = 1)
  Genes = read.table(paste(ExpName, ".umi.txt", sep=""), header=TRUE,sep="\t", row.names = 1)
  
  #Create end to end list of all three files, 
  Data_Frame=list(Met,Qc)
  #Intersect to find barcodes present in all 3 end to end files - should select for "real" barcodes
  BC_Master=Reduce(intersect,lapply(Data_Frame,rownames))
  #Column bind all three files together and select BC list from them
  Full = cbind(Met[BC_Master,], Qc[BC_Master,])
  colnames(Full)[25]="mRNA_READS"
  #order by Transcripts
  Full=Full[rev(order(Full$NUM_TRANSCRIPTS)),]
 #print("Doing Plots")
 #imgfile=paste(ExpName,".qcmetrics",".png",sep="")

 par(mfrow=c(2,3))
  if (Species == 1) { 
    #subset by same BC used in rest of script
    Genes_Sub = Genes[,BC_Master]
    #Grep (pull out) human and mouse gene names from MM9/HG19 reference genome for each BC and sum read counts
    Human = (c(colSums(Genes_Sub[grep("HUMAN*", rownames(Genes_Sub)), ])))
    Mouse = (c(colSums(Genes_Sub[grep("MOUSE*", rownames(Genes_Sub)), ])))
    
    HumanPercentage = c(round((Human/(Mouse+Human)) * 100, digits = 2))
    MousePercentage = c(round((Mouse/(Mouse+Human))* 100, digits = 2))
    
    x = data.frame(HumanPercentage, MousePercentage,Human,Mouse)
    colnames(x) = c("Human_Percentage","Mouse_Percentage","Human_Genes", "Mouse_Genes")
    x = na.omit(x)
    #Most to least overall genes
    x=x[rev(order(x[,3] + x[,4])),]
    #print(x)
   # write.table(x, file = paste(ExpName, "SpeciesIdent.txt",sep="."),sep="\t",quote=F,row.names=F) 
    plot(x$Human_Percentage,x$Mouse_Percentage,xlab="Human Percentage",ylab="Mouse Percentage",main="Human vs Mouse Percentage")
    plot(x$Human_Genes,x$Mouse_Genes,xlab="Human Genes",ylab="Mouse Genes",main="Human vs Mouse Counts")
	hist(x$Human_Percentage,breaks=10,main="Distribution of Human transcripts ")
  }
  #put species metrics before plotting - will print even when plotting fails.
  
  
  # Log of Number of transcripts detected versus alignment rate to mRNA
  plot(log(Full$NUM_TRANSCRIPTS+1), Full$PCT_MRNA_BASES,xlab = "Transcripts",ylab = "Alignment % mRNA")
  #just good looking ones
  Full_Sub=subset(subset(Full,PCT_MRNA_BASES>.4),NUM_TRANSCRIPTS>50)
  plot(Full_Sub$mRNA_READS,Full_Sub$NUM_TRANSCRIPTS,xlab = "mRNA Reads",ylab = "Transcript")
  plot(log(Full_Sub$NUM_TRANSCRIPTS+1),Full_Sub$mRNA_READS/Full_Sub$NUM_TRANSCRIPTS,xlab = "Transcript",ylab = "Reads/Transcripts")
 title(basename(ExpName), outer=FALSE)
  return(x)

 
}


output$canalplot<-renderPlot({
	umi=datafile()
	qc=sub(".umi.txt",".qc.txt",umi)
	met=sub(".umi.txt",".metrics.txt",umi)
	expname=sub(".umi.txt","",umi)
	
	if (file.exists(umi) & file.exists(met) & file.exists(qc)) {
		mydt=plotMetrics(expname,NUM_Transcripts=2)
	}

})

output$canaldt<-renderDataTable({
	canaltable()
})

canaltable<-reactive({
   umi=datafile()
	qc=sub(".umi.txt",".qc.txt",umi)
	met=sub(".umi.txt",".metrics.txt",umi)
	expname=sub(".umi.txt","",umi)
	if (file.exists(umi) & file.exists(met) & file.exists(qc)) {
		mydt=plotMetrics(expname)
		mm=cbind(basename(expname),mydt)
		return(mm)
	}
})


    #Download Handler 
 output$canal_download <- downloadHandler(
 
                 filename = function() {
                    paste('canalCounts.',input$ifile,".",Sys.Date(), '.csv', sep="")
                  },
                 content = function(file) {
						write.table(canaltable(),file,sep=",",row.names=F,quote=F)
				}
 )
