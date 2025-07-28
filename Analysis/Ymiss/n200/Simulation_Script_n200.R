setwd('/scratch/your account/Analysis/Ymiss/n200')


rr <- 0
for(b2 in c(0.1, 0.37, 0.5)){
  for (num_aux in c(5, 20, 50)){
    for (mr in c(0.15, 0.30, 0.60, 0.80)){
      for (missing_type in c("Y_MCAR", "Y_MAR_X1", "Y_MNAR_Z1", "Y_MNAR_Self")){
        
        rr <- rr + 1 # replication index 
        
        newname <- paste('R',rr,'.R',sep='')
          
        name  <-	paste('b2.',b2,sep='') 
        repfolder<-paste("sed 's/replace1/",name,"/g' Analysis.Y.miss_n200.R > temp.R", sep='')
        system(repfolder)
          
        name  <-	paste('aux',num_aux,sep='') 
        repfolder<-paste("sed 's/replace2/",name,"/g' temp.R > temp0.R", sep='')
        system(repfolder)
        system('rm -f temp.R')
          
        name  <-	paste('mr',mr,sep='') 		
        repfolder<-paste("sed 's/replace3/",name,"/g' temp0.R > temp1.R", sep='')
        system(repfolder)
        system('rm -f temp0.R')
          
        name  <-	paste('mt.',missing_type,sep='') 
        repfolder<-paste("sed 's/replace4/",name,"/g' temp1.R > ",newname, sep='')
        system(repfolder)
        system('rm -f temp1.R')
          
          
        name <- paste('b2',b2,'aux',num_aux,"mr",mr,"mt.",missing_type,sep='')
        write.table(c(rr,name),'record.txt',append=TRUE,row.names=FALSE,col.names=FALSE)
      }
    }
  }
}