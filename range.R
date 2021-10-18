val<-function(seq_num,seq1=NULL){
  seq_num<-seq_num[!duplicated(seq_num)] #ektselktmeslksmlkt
  con_num<-c()
  con<-c()
  if(is.null(seq1)){
    for(i in 1:(length(seq_num))){
      # print(i)
      if(i!=length(seq_num)){
        i1=seq_num[i]+1
        if(i1!=seq_num[i+1]){
          
          if(is.null(con)){con_num<-c(con_num,(seq_num[i]))}
          if(!is.null(con)){con_num=c(con_num,paste0(con,'-',(seq_num[i])));con=NULL}
          
        }
        if(i1==seq_num[i+1]){
          if(is.null(con)){con=seq_num[i]}
        }
      }
      
      if(i==length(seq_num)){
        if(!is.null(con)){con_num=c(con_num,paste0(con,'-',(seq_num[i])))}
        if(is.null(con)){con_num<-c(con_num,(seq_num[i]))}
        
      }
    }
  }
  if(!is.null(seq1)){
    for(i in 1:(length(seq_num))){
      # print(i)
      if(i!=length(seq_num)){
        i1=seq_num[i]+1
        if(i1!=seq_num[i+1]){
          if(is.null(con)){con_num<-c(con_num,seq1[seq_num[i]])}
          if(!is.null(con)){con_num=c(con_num,paste0(c(con,seq1[seq_num[i]]),collapse = ''));con=NULL}
          
        }
        if(i1==seq_num[i+1]){
          if(!is.null(con)){con=c(con,seq1[seq_num[i]])}
          if(is.null(con)){con=c(seq1[seq_num[i]])}
        }
      }
      
      if(i==length(seq_num)){
        if(!is.null(con)){con_num=c(con_num,paste0(c(con,seq1[seq_num[i]]),collapse = ''))}
        if(is.null(con)){con_num<-c(con_num,(seq1[seq_num[i]]))}
        
      }
    }
  }
  return(con_num)
} 
