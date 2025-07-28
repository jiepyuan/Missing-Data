setwd('/scratch/your account/Analysis/XYmiss/n500')


rr <- 0
for(b2 in c(0.1, 0.37, 0.5)){
  for (num_aux in c(5, 20, 50)){
    for (mr_y in c(0.15, 0.30, 0.60)){
      for (mr_x in c(0.1, 0.3)){
        for (missing_type in c("Y_MCAR_XZ_MCAR", "Y_MCAR_XZ_MNAR", "Y_MAR_X1_XZ_MCAR", "Y_MAR_X1_XZ_MNAR",
                               "Y_MNAR_Z1_XZ_MCAR", "Y_MNAR_Z1_XZ_MNAR", "Y_MNAR_Self_XZ_MCAR", "Y_MNAR_Self_XZ_MNAR")){
          
          
          rr <- rr + 1 # replication index 
        
          newname <- paste('R',rr,'.R',sep='')
          
          name  <-	paste('b2.',b2,sep='') 
          repfolder<-paste("sed 's/replace1/",name,"/g' Analysis.XY.miss_n500.R > temp.R", sep='')
          system(repfolder)
          
          name  <-	paste('aux',num_aux,sep='') 
          repfolder<-paste("sed 's/replace2/",name,"/g' temp.R > temp0.R", sep='')
          system(repfolder)
          system('rm -f temp.R')
          
          name  <-	paste('mry',mr_y,sep='') 		
          repfolder<-paste("sed 's/replace3/",name,"/g' temp0.R > temp1.R", sep='')
          system(repfolder)
          system('rm -f temp0.R')
          
          name  <-	paste('mrx',mr_x,sep='') 
          repfolder<-paste("sed 's/replace4/",name,"/g' temp1.R > temp2.R", sep='')
          system(repfolder)
          system('rm -f temp1.R')
          
          
          name  <-	paste('mt.',missing_type,sep='') 
          repfolder<-paste("sed 's/replace5/",name,"/g' temp2.R > ",newname, sep='')
          system(repfolder)
          system('rm -f temp2.R')
          
          
          name <- paste('b2',b2,'aux',num_aux,"mry",mr_y,"mrx",mr_x,"mt.",missing_type,sep='')
          write.table(c(rr,name),'record.txt',append=TRUE,row.names=FALSE,col.names=FALSE)
        }
      }
    }
  }
}