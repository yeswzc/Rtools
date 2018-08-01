Hexadecimal2decimal<-function(data=c("#CC00FF","#FFB400")){
  #data a vector of hexadecimal colour names
  hex2dec<-function(h){
    x1<-c(0:9,letters[1:6])
    a<-substr(h,2,2)
    aa<-which(x1==a)
    if(length(aa)==0){ stop("Error: hNot color name !")}
    b<-substr(h,1,1)
    bb<-which(x1==b)
    if(length(aa)==0){ stop("Error: hNot color name !")}
    y=(aa-1)*16+bb-1
    return(y)
  }
  out<-rep(NA,length(x))
  for(n in 1:length(data) ){
    x=data[n]
    x=gsub("^#","",x)
    if( nchar(x)  !=6 ){stop("Not hexadecimal colour names !") }
    rgb<-rep(NA,3)
    for(i in 1:3){
      j=1+(i-1)*2
      y<-tolower(substr(x,j,j+1))
      #print(y)
      rgb[i]=hex2dec(y)
    }
    rgb<-paste(rgb[1],rgb[2],rgb[3],sep = ",")
    out[n]=rgb
  }
  return(out)
}           

rgbHeatCol<-function(x,n=9,lcol="#FFFFFF"){
  library(gplots)
  Hexadecimal2decimal(colorpanel(n,low = lcol,high =x ))
}

