library(geomorph)
land<-readland.tps("landmarks.tps")
gpa<-gpagen(land)
plot3d(gpa$consensus)
C<-gpa$coords
nn<-NULL
reg<-NULL
j<-0
BP<-array(0,c(60,2,9))

for (l in c(6,20,27,1,2,9,3,4,17)) {
  for(i in 1:30){
    D<-C[,,sample(dim(C)[3],dim(C)[3])]
    F<-D
    
    ml<-l#missing landmark
    
    D[ml,,501:535]<-NA
    E<-estimate.missing(D,method = "Reg")
    
    #---------------------------
    
    MY<-matrix( F[ml,,],dim(F)[3],3,byrow = TRUE)
    MX<-F[-ml,,]
    
    mat<-cbind(matrix(MX,535,162,byrow = TRUE),MY)
    mat.df<-as.data.frame(mat)
    
    # Split
    train = mat.df[1:500,]
    test = mat.df[501:535,]
    
    train<-as.matrix(train)
    test<-as.matrix(test)
    
    x_train <- train[,1:162]
    y_train <- train[,163:165]
    x_test <- test[,1:162]
    y_test <- test[,163:165]
    
    
    #---------KERAS-----------
    
    
    library(keras)
    
    #define el modelo
    model <- keras_model_sequential() 
    model %>% 
      layer_dense(units = 80, input_shape = c(162)) %>% 
      layer_dense(units = 10) %>%
      layer_dense(units = 10) %>%
      layer_dense(units = 10) %>%
      layer_dense(units = 3)
    
    
    #compila el modelo
    model %>% compile(
      loss = "mean_squared_error",
      optimizer=optimizer_adam(lr = 0.0001),
      metrics = "mean_squared_error"
    )
    
    
    #entrena el modelo
    
    history <- model %>% fit(
      x_train, y_train, 
      epochs = 50, batch_size = 1, 
      validation_split = 0.2
    )
    
    pp<-predict(model,x_test)
    
    nn<-c(nn,sum((y_test-pp)^2))
    reg<-c(reg,sum((y_test-t(E[l,,501:535]))^2))
    print(c(i,j))
  }
  j<-j+1
  bp<-cbind(c(nn,reg),c(rep(1,30),rep(2,30)))
  BP[,,j]<-bp
  nn<-NULL
  reg<-NULL
}
