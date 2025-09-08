#### subsampling

#SampleGroup - group with the smaller sample size
#SampleCompare - group with the larger sample size
#ncolumn - the column of the measurement data
#niter - number of sampling iterations
#data - the dataframe in which the data reside

subsample_metric<-function(SampleGroup,SampleCompare,ncolumn,niter,data){
  groupdata<-data[data$Group==SampleGroup,]
  comparedata<-data[data$Group==SampleCompare,]
  output_KS<-matrix(nrow=niter,ncol=2)
  colnames(output_KS)<-c("CHi2","Pvalue")
  output_WSR<-matrix(nrow=niter,ncol=2)
  colnames(output_WSR)<-c("W","Pvalue")
  output_KmS<-matrix(nrow=niter,ncol=2)
  colnames(output_KmS)<-c("D","Pvalue")
  for(i in 1:niter){
    comparesamp<-sample(nrow(comparedata),nrow(groupdata))
    comparenew<-comparedata[comparesamp,]
    output_KS[i,1]<-kruskal.test(groupdata[,ncolumn],comparenew[,ncolumn])$statistic[[1]]
    output_KS[i,2]<-kruskal.test(groupdata[,ncolumn],comparenew[,ncolumn])$p.value
    output_WSR[i,1]<-wilcox.test(groupdata[,ncolumn],comparenew[,ncolumn])$statistic[[1]]
    output_WSR[i,2]<-wilcox.test(groupdata[,ncolumn],comparenew[,ncolumn])$p.value
    output_KmS[i,1]<-ks.test(groupdata[,ncolumn],comparenew[,ncolumn])$statistic[[1]]
    output_KmS[i,2]<-ks.test(groupdata[,ncolumn],comparenew[,ncolumn])$p.value
  }
  mean_KS<-c(mean(output_KS[,1]),mean(output_KS[,2]))
  names(mean_KS)<-c("MeanChi2","MeanPvalue")
  mean_WSR<-c(mean(output_WSR[,1]),mean(output_WSR[,2]))
  names(mean_WSR)<-c("MeanW","MeanPvalue")
  mean_KmS<-c(mean(output_KmS[,1]),mean(output_KmS[,2]))
  names(mean_KmS)<-c("MeanD","MeanPvalue")
  Both_results<-c(mean_KS,mean_WSR,mean_KmS)
  return(Both_results)
}


subsample_aov<-function(SampleGroup,ncolumn,niter,data){
  groupdata<-data[data$Group==SampleGroup,]
  all_groups<-unique(data$Group)
  output_ANOVA<-matrix(nrow=niter,ncol=2)
  colnames(output_ANOVA)<-c("F","Pvalue")
  output_tukey<-list()
  compare_all<-groupdata
  for(i in 1:niter){
    for(n in 1:length(unique(data$Group))){
      focal<-all_groups[n]
      if(focal==SampleGroup){
        next
      }
      comparedata<-data[data$Group==focal,]
      comparesamp<-sample(nrow(comparedata),nrow(groupdata))
      comparenew<-comparedata[comparesamp,]
      compare_all<-rbind(compare_all,comparenew)
    }
    ANOVA_test<-summary(aov(compare_all[,ncolumn] ~ Group, data = compare_all))
    output_ANOVA[i,1:2]<-c(ANOVA_test[[1]][1,4],ANOVA_test[[1]][1,5])
    if(output_ANOVA[i,2]<0.05){
      tukey_res<-TukeyHSD(aov(compare_all[,ncolumn] ~ Group, data = compare_all))
      output_tukey[[i]]<-tukey_res$Group
    }else{
      next
    }
  }
  if(length(output_tukey)>0){
    all_tukey<-Summarize.tukey(output_tukey)
  }
  mean_aov<-c(mean(output_ANOVA[,1]),mean(output_ANOVA[,2]))
  names(mean_aov)<-c("MeanF","MeanPvalue")
  Both_results<-list(mean_aov,all_tukey)
  return(Both_results)
}

Summarize.tukey<-function(results_object){
  results_object<-results_object[lengths(results_object) != 0]
  summary_object<-matrix(ncol=length(results_object),nrow=nrow(results_object[[1]]))#nrow for one tukey test
  i<-1
  for(i in 1:length(results_object)){
    summary_object[,i]<-results_object[[i]][,4]
  }
  rownames(summary_object)<-rownames(results_object[[1]])
  final_summary<-matrix(ncol=2,nrow=nrow(summary_object))
  rownames(final_summary)<-rownames(summary_object)
  colnames(final_summary)<-c("Mean","SD")
  final_summary[,1]<-rowMeans(summary_object)
  final_summary[,2]<-apply(summary_object, 1, sd, na.rm=TRUE)
  return(final_summary)
}
