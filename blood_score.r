#for replicated blood score calculation;;by using this code 
# it's easy to deploy the model to tons of patiens to gain their blood score
# thus make this model and website useful for patients and doctors

#model of  https://bloodscore4npc.shinyapps.io/shinyblood/
#by lihaoj@sysucc.org.cn


##remember to change the path to your own path
##################include package and function##################
if(TRUE){
  #library(tidyr)
  library(rms)
  library(Hmisc)
  # library(lattice)
  library(survival)
  library(Formula)
  library(ggplot2)
  # library(nomogramEx)  #at least R4.1,to calculate nomogram score,not necessary
  
  #some function
  #remember to change the path to your own path
  if(TRUE){
    
    load("P://npc3/营养指数rulefit机器学习/rulelists.RData")#rules,generate by H2o/rulefit model 
    load("P://npc3/营养指数rulefit机器学习/m2.RData")#cox rerule model:blood score calculation
    load("P://npc3/营养指数rulefit机器学习/model_os.RData")#cox final model:base on OS
    load("P://npc3/营养指数rulefit机器学习/try_nomogram1.RData")#nomogram of final model
  }
  
}

##################replicated our result, and how to use model and sample code##################

##prepare patients infos
#Fill in one or more real patients information (one is shown below) 
#on sample patient below
if(TRUE){
  # patients info
  newdb <- data.frame(
    rule3 = 0, #not necessary to set 
    rule9 = 0, #not necessary to set 
    rule11 = 0,#not necessary to set 
    rule18 = 0,#not necessary to set 
    x5x=1.9,   #'Absolute lymphocyte count (10E9/L)'
    x15x=0.4,  #"Absolute monocyte count (10E9/L)"
    x16x=1.58, #'C-reactive protein (mg/L)',  
    x9x=233,   #'Platelet count (10E9/L)'
    x13x=6.9,  #'White blood cell count (10E9/L)'
    x22x=2.24, #'Neutrophil-to-lymphocyte ratio (N/L)'
    x2x=4,     #T classification
    x6x=3,     #N classification
    test2ebv=2,#EBV DNA level (0,1,2);0 for those <1000 copies/ul;1 for  10>those >=1 *1000, 2for those over 10,000copies/ul
    x10x=60    #patients(years)
  )
  
  # to set some factor
  newdb[,"rule3"]=factor(newdb[,"rule3"],levels=c(0,1) )
  newdb[,"rule9"]=factor(newdb[,"rule9"],levels=c(0,1) )
  newdb[,"rule11"]=factor(newdb[,"rule11"],levels=c(0,1) )
  newdb[,"rule18"]=factor(newdb[,"rule18"],levels=c(0,1) )
  newdb[,"x2x"]=factor(newdb[,"x2x"],levels=c(1,2,3,4) )
  newdb[,"x6x"]=factor(newdb[,"x6x"],levels=c(0,1,2,3) )
  newdb[,"test2ebv"]=factor(newdb[,"test2ebv"],levels=c(0,1,2) )
  # newdb[,"x10x"]=as.integer(45)
  
  
  if(TRUE){
    print(newdb)
    temp_train=newdb
    for(rulei in c(3,9,11,18)){
      # print(rulei)
      
      rulename=paste0(c("rule",rulei),collapse = "")
      temp_train[,rulename]=eval(parse(text=paste0(c("as.factor(ifelse(",rulelists[rulei],",1,0))"),collapse = "")))
      # temp_test[,rulename]=eval(parse(text=paste0(c("as.factor(ifelse(",rulelists2[rulei],",1,0))"),collapse = "")))
      # rule_records=c(rule_records,rulename)
      
      
      
      # rulei=rulei+1
    }
    newdb=temp_train
    print(newdb)
  }
  
  
  
}



#to calculate blood score / final score(linear preditor and nomogram score) 
if(TRUE){
  
  #to calculate blood score 
  newdb$cox_rerule=predict(m2, newdata = newdb)
  print(newdb$cox_rerule) #linear preditor (LP)
  
  #to calculate final score(linear preditor) 
  newdb$final_score=predict(model_os, newdata = newdb)
  print(newdb$final_score)
  
  
  #to calculate final score(nomogram score) 
  #if you are using R>4.1 , set it true
  if(FALSE){
    
    result1=formula_rd(try_nomogram1)
    points=points_cal(result1$formula,newdb[,c("cox_rerule","x2x","x6x","test2ebv","x10x")])
    print(points)
  }
 
  
  
  
}