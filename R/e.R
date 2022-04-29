#' @title SPOUSE (Scatter Plots Over-Viewed Using Summary Ellipses)
#' @description Summary ellipses superimposed on a scatter plot contain all bi-variate summary
#'              statistics for regression analysis. Furthermore, the outer ellipse flags potential
#'              outliers. Multiple groups can be compared in terms of centers and spreads as illustrated
#'              in the examples.
#' @param data1 explanatory variable; numeric vector x
#' @param data2 response variable; numeric vector y
#' @param InEllipse LOGICAL; True (Default) for showing the inner ellipse
#' @param OutEllipse LOGICAL; TRUE (Default) for showing the outer ellipse
#' @param InRect LOGICAL; TRUE (Default) for showing the inner rectangle
#' @param OutRect LOGICAL; TRUE (Default) for showing the outer rectangle
#' @param xlab x-variable label
#' @param ylab y-variable label
#' @param main Main title of the plot.
#' @param coverage the percentage of data that falls inside the outer ellipse
#' @param add adds a summary ellipse of a new data set to an existing plot. Compares multiple
#'            groups.
#' @param wspace the amount of white space around the plot window; negative to suppress details around the boundaries (zoom in);
#'               positive to zoom out. Default is 0.2.
#' @param ylim bounds for the y-axis
#' @param xlim bounds for the x-axis
#' @param pch display symbols for the points in the scatter plot; Use different pch for different groups.
#' @param cex size of the points
#' @param col color of the points
#' @param REGyonx LOGICAL; TRUE (Default) for showing the regression line of y on x.
#' @param REGxony LOGICAL; TRUE (Default) for showing the regression line of x on y.
#' @importFrom graphics lines points segments
#' @importFrom stats cov lm qf sd var
#' @export
#' @return A new plot which shows the ellipses superimposed on top of each other.
#' @examples
#'    x1<-iris3[,"Sepal L.","Setosa"]
#'    y1<-iris3[,"Sepal W.","Setosa"]
#'    x2<-iris3[,"Sepal L.","Versicolor"]
#'    y2<-iris3[,"Sepal W.","Versicolor"]
#'    x3<-iris3[,"Sepal L.","Virginica"]
#'    y3<-iris3[,"Sepal W.","Virginica"]
#'    xlim=c(4,8)
#'    ylim=c(1.5,5)
#'    summaryEllipse(x1,y1,xlim=xlim,ylim=ylim,InEllipse='F',InRect='F',
#'    OutRect='F',REGxony='F',REGyonx='F')
#'    summaryEllipse(x2,y2,add=TRUE,pch=20,col="brown",InEllipse='F',InRect='F',
#'    OutRect='F',REGxony='F',REGyonx='F')
#'    summaryEllipse(x3,y3,add=TRUE,pch=19,col="grey",InEllipse='F',InRect='F',
#'    OutRect='F',REGxony='F',REGyonx='F')
#'    #end of example
summaryEllipse<-function(data1,data2,InEllipse=TRUE,OutEllipse=TRUE,InRect=TRUE,
                          OutRect=TRUE,xlab="X",ylab="Y",main="Summary Ellipse",coverage=0.98,
                          add = FALSE,wspace=0.2,REGyonx=TRUE,REGxony=TRUE,
                          ylim = c(min(data2)-wspace*(max(data2)-min(data2)),max(data2)+wspace*(max(data2)-min(data2))),
                          xlim = c(min(data1)-wspace*(max(data1)-min(data1)),max(data1)+wspace*(max(data1)-min(data1))),
                          pch = 1,cex =0.7,col = "black"
){

  xcenter <- mean(data1) #this is for the x-cordinate of the center of the ellipse
  ycenter <- mean(data2) #this is for the y-cordinate of the center of the ellipse

  x_var <- var(data1) #variance of x
  y_var <- var(data2) #variance of y
  xy_cov <- cov(data1, data2) #covariance of x and y
  cov_matrix <- matrix(c(x_var,xy_cov,xy_cov,y_var),nrow = 2,ncol = 2,byrow = TRUE) #covariance matrix
  eigen_properties <- eigen(cov_matrix) #contains eigen$values and eigen$vectors
  max_eigen_value <- max(eigen_properties$values) #maximum eigen value
  min_eigen_value <- min(eigen_properties$values) #minimum eigen value

  #axis length and angle between the x-axis and the major axis eigen vector
  major_axis_length <- (max_eigen_value^0.5)
  minor_axis_length <- (min_eigen_value^0.5)
  major_axis_eigen_vector <- eigen_properties$vectors[,1]
  theta <- acos(major_axis_eigen_vector[1])
  if(major_axis_eigen_vector[2]<0){
    theta <- theta * (-1)
  }

  #scatter plot
  if (add==F){
    plot(data1,data2,xlim=xlim,ylim=ylim,frame.plot=F,las=1,xlab=xlab,ylab=ylab,main=main,pch=pch,cex=cex,col=col)
  }

  if(add==T){
    points(data1,data2,pch=pch,cex=cex,col=col)
  }

  t <- seq(0, 2*pi, 0.001)

  #Inner Ellipse
  if(InEllipse==T){
    x <- xcenter + major_axis_length * cos(t) * cos(theta) - minor_axis_length * sin(t) * sin(theta)  #check the formula
    y <- ycenter + major_axis_length * sin(theta) * cos(t) + minor_axis_length * cos(theta) * sin(t)  #check the form
    lines(x,y,lty=2,col="green") #Constructing curve for the inner ellipse
  }

  #Outer ellipse
  if(OutEllipse==T){
    t <- seq(0, 2*pi, 0.001)
    d <- sqrt(qf(coverage,2,length(data1)-2))
    x <- xcenter + major_axis_length * d * cos(t) * cos(theta) - minor_axis_length * d * sin(t) * sin(theta)  #check the formula
    y <- ycenter + major_axis_length * d * sin(theta) * cos(t) + minor_axis_length * d * cos(theta) * sin(t)  #check the form
    lines(x,y,lty=1,col="black") #Constructing the curve for the outer ellipse
  }

  #Rectangle for Inner Ellipse
  if(InRect==T){
    segments(x0=xcenter-sd(data1), y0=ycenter-sd(data2), x1=xcenter+sd(data1), y1=ycenter-sd(data2), lty="dotted")
    segments(x0=xcenter-sd(data1), y0=ycenter-sd(data2), x1=xcenter-sd(data1), y1=ycenter+sd(data2), lty="dotted")
    segments(x0=xcenter-sd(data1), y0=ycenter+sd(data2), x1=xcenter+sd(data1), y1=ycenter+sd(data2), lty="dotted")
    segments(x0=xcenter+sd(data1), y0=ycenter-sd(data2), x1=xcenter+sd(data1), y1=ycenter+sd(data2), lty="dotted")
  }

  #Rectangle for the Outer Ellipse
  if(OutRect==T){
    segments(x0=xcenter-d*sd(data1), y0=ycenter-d*sd(data2), x1=xcenter+d*sd(data1), y1=ycenter-d*sd(data2), lty="dotted")
    segments(x0=xcenter-d*sd(data1), y0=ycenter-d*sd(data2), x1=xcenter-d*sd(data1), y1=ycenter+d*sd(data2), lty="dotted")
    segments(x0=xcenter-d*sd(data1), y0=ycenter+d*sd(data2), x1=xcenter+d*sd(data1), y1=ycenter+d*sd(data2), lty="dotted")
    segments(x0=xcenter+d*sd(data1), y0=ycenter-d*sd(data2), x1=xcenter+d*sd(data1), y1=ycenter+d*sd(data2), lty="dotted")
  }

  if(REGyonx==T){
    # regression line of y on x
    fit <- lm(data2~data1)
    yonxintercept <- fit$coefficient[[1]]
    yonxgradient <- fit$coefficient[[2]]
    x2yonx <- xcenter + d * sd(data1)
    y2yonx <- yonxgradient*(x2yonx) + yonxintercept
    x1yonx <- xcenter - d * sd(data1)
    y1yonx <- yonxgradient*(x1yonx) + yonxintercept
    segments(x1yonx,y1yonx,x2yonx,y2yonx,lty = 1)
  }

  if(REGxony==T){
    #regression line of x on y
    fit <- lm(data1~data2)
    xonygradient <- 1/fit$coefficient[[2]]
    xonyintercept <- (0-fit$coefficient[[1]])*xonygradient
    y2xony <- ycenter + d * sd(data2)
    x2xony <- (y2xony-xonyintercept)/xonygradient
    y1xony <- ycenter - d * sd(data2)
    x1xony <- (y1xony-xonyintercept)/xonygradient
    segments(x1xony,y1xony,x2xony,y2xony,lty = 3)
  }

  #x11<-iris3[,"Sepal L.","Setosa"]
  #y11<-iris3[,"Sepal W.","Setosa"]
  #x12<-iris3[,"Sepal L.","Versicolor"]
  #y12<-iris3[,"Sepal W.","Versicolor"]
  #x13<-iris3[,"Sepal L.","Virginica"]
  #y13<-iris3[,"Sepal W.","Virginica"]

}
