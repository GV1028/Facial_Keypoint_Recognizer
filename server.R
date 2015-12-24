library(h2o)
library(readr)
f<-function(test){
  train<-read_csv("data/train.csv")
  h<-h2o.init(max_mem_size = '6g',nthreads=-1)
  testmat<-matrix(test,nrow=1,ncol=9216)
  rownames(testmat)<-NULL
  colnames(testmat)<-sequence(9216)
  trainh2o<-as.h2o(train)
  testh2o<-as.h2o(testmat)
  new<-matrix(nrow=1)
  for(i in 1:30){
    mod<-h2o.glm(x=31:9246,y=i,training_frame = trainh2o,family = "gaussian",solver= "L_BFGS",max_iterations = 200)
    new<-cbind(new,as.data.frame(h2o.predict(mod,testh2o)))
  }
  new$new<-NULL
  
  colnames(new)<-colnames(train)[1:30]
  return (new)
}


library(shiny)
library(EBImage)
shinyServer(function(input, output) {
  data<-reactive({
    file1<-input$inimage
    if(is.null(file1)){return()}
    else
    {
      img<-readImage(input$inimage$datapath,type="JPEG")
      img<-channel(img,"gray")
      img<-resize(img,96,96)
      img<-rotate(img,180)
      
      img
    }
  })
  
  output$image<-renderPlot({
    if(is.null(data())) print("Select file")
    else
    {
      image(data())
      value<-f(channel(data(),"gray"))
      
      points(96-value$left_eye_center_x[1],96-value$left_eye_center_y[1],col="green",pch=10)
      points(96-value$right_eye_center_x[1],96-value$right_eye_center_y[1],col="green",pch=10)
      points(96-value$left_eye_inner_corner_x[1],96-value$left_eye_inner_corner_y[1],col="green",pch=10)
      points(96-value$left_eye_outer_corner_x[1],96-value$left_eye_outer_corner_y[1],col="green",pch=10)
      points(96-value$right_eye_inner_corner_x[1],96-value$right_eye_inner_corner_y[1],col="green",pch=10)
      points(96-value$right_eye_outer_corner_x[1],96-value$right_eye_outer_corner_y[1],col="green",pch=10)
      points(96-value$left_eyebrow_inner_end_x[1],96-value$left_eyebrow_inner_end_y[1],col="green",pch=10)
      points(96-value$left_eyebrow_outer_end_x[1],96-value$left_eyebrow_outer_end_y[1],col="green",pch=10)
      points(96-value$right_eyebrow_inner_end_x[1],96-value$right_eyebrow_inner_end_y[1],col="green",pch=10)
      points(96-value$right_eyebrow_outer_end_x[1],96-value$right_eyebrow_outer_end_y[1],col="green",pch=10)
      points(96-value$nose_tip_x[1],96-value$nose_tip_y[1],col="green",pch=10)
      points(96-value$mouth_left_corner_x[1],96-value$mouth_left_corner_y[1],col="green",pch=10)
      points(96-value$mouth_right_corner_x[1],96-value$mouth_right_corner_y[1],col="green",pch=10)
      points(96-value$mouth_center_top_lip_x[1],96-value$mouth_center_top_lip_y[1],col="green",pch=10)
      points(96-value$mouth_center_bottom_lip_x[1],96-value$mouth_center_bottom_lip_y[1],col="green",pch=10)
    }
    
  })
  
})
