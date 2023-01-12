###################################################################
#import_raw_data <- to import a txt,csv or excel data file
#containing sonic obervations
#latest update
#
#
###################################################################
import_raw_data<-function(fileName,formatType,has_header=T){
  
  fileFormat = which(formatType==c('table_txt','csv','xlsx'),arr.ind=FALSE)
  
  #print(sprintf("the file format index is:%d",fileFormat))
  
  if(identical(fileFormat,integer(0))){
    stop("please specify one of the following file format types:
         'table_txt','csv','xlsx'")
  }else if (fileFormat==1){
    mydata=read.table(fileName,header=has_header)
    
  }else if(fileFormat==2){
    mydata=read.csv(fileName,header=has_header)
    
  }else if(fileFormat==3){
    mydata=read_excel(fileName,sheet=1,col_names=has_header)
    mydata<-data.table(mydata)
  }else 
    
    return(mydata)
  
}