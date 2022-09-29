


#' Plot the estimates and 95% confidence intervals of random effects
#'
#' @param output input the output object from the model_prep function
#' @param sdrep input the sdrep object
#' @param names list names of each level of the random effects (defaults to states in the example data)
#' @param re_name name of the random effect (defaults to "State")
#'
#' @return plot
#' @export
#'
#' @examples plot_re(output, sdrep, names=c("Michigan","Florida","Connecticut","Kansas","South Dakota"), re_name="State")
plot_re<-function(output, sdrep, names=c("Michigan","Florida","Connecticut","Kansas","South Dakota"), re_name="State"){

if(length(output$parameters$tau_int)>1){

val_names<-names(sdrep$value)
ind<-val_names=="tau_int"

high_conf<-sdrep$value[ind] + qnorm(0.975)*sdrep$sd[ind]
low_conf<-sdrep$value[ind] - qnorm(0.975)*sdrep$sd[ind]
est<-sdrep$value[ind]

int_df<-data.frame(est, low_conf, high_conf, random_effect=names)

p1<-ggplot(int_df, aes(x=reorder(random_effect,est), y=est))+
  geom_point(size=4)+
  geom_segment(aes(x=random_effect,xend=random_effect,y=low_conf,yend=high_conf), data=int_df)+
  geom_hline(yintercept=0, linetype="dashed", colour="red", size=1.25)+
  theme_bw()+xlab(re_name)+ylab(paste(re_name, "+/- 95% CI", sep=" "))+
  ggtitle("Effort Intercept")+
  theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}

if(length(output$parameters$q_dev)>1){
  
  val_names<-names(sdrep$value)
  ind<-val_names=="q_dev"
  
  high_conf<-sdrep$value[ind] + qnorm(0.975)*sdrep$sd[ind]
  low_conf<-sdrep$value[ind] - qnorm(0.975)*sdrep$sd[ind]
  est<-sdrep$value[ind]
  
  int_df<-data.frame(est, low_conf, high_conf, random_effect=names)
  
p2<-ggplot(int_df, aes(x=reorder(random_effect,est), y=est))+
    geom_point(size=4)+
    geom_segment(aes(x=random_effect,xend=random_effect,y=low_conf,yend=high_conf), data=int_df)+
    geom_hline(yintercept=0, linetype="dashed", colour="red", size=1.25)+
    theme_bw()+xlab(re_name)+ylab(paste(re_name, "+/- 95% CI", sep=" "))+
    ggtitle("Catchability")+
    theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}

if(length(output$parameters$intercept)>1){
  
  val_names<-names(sdrep$value)
  ind<-val_names=="intercept"
  
  high_conf<-sdrep$value[ind] + qnorm(0.975)*sdrep$sd[ind]
  low_conf<-sdrep$value[ind] - qnorm(0.975)*sdrep$sd[ind]
  est<-sdrep$value[ind]
  
  int_df<-data.frame(est, low_conf, high_conf, random_effect=names)
  
p3<-ggplot(int_df, aes(x=reorder(random_effect,est), y=est))+
    geom_point(size=4)+
    geom_segment(aes(x=random_effect,xend=random_effect,y=low_conf,yend=high_conf), data=int_df)+
    geom_hline(yintercept=0, linetype="dashed", colour="red", size=1.25)+
    ggtitle("Catch Intercept")+
    theme_bw()+xlab(re_name)+ylab(paste(re_name, "+/- 95% CI", sep=" "))+
    theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}

if(length(output$parameters$tau_int)>1 & length(output$parameters$q_dev)==1 & length(output$parameters$intercept)==1){
  print(p1)
}
if(length(output$parameters$tau_int)==1 & length(output$parameters$q_dev)>1 & length(output$parameters$intercept)==1){
  print(p2)
}
if(length(output$parameters$tau_int)==1 & length(output$parameters$q_dev)==1 & length(output$parameters$intercept)>1){
  print(p3)
}

if(length(output$parameters$tau_int)>1 & length(output$parameters$q_dev)>1 & length(output$parameters$intercept)==1){
  gridExtra::grid.arrange(p1,p2, nrow=1)
}

if(length(output$parameters$tau_int)>1 & length(output$parameters$q_dev)==1 & length(output$parameters$intercept)>1){
  gridExtra::grid.arrange(p1,p3, nrow=1)
}

if(length(output$parameters$tau_int)==1 & length(output$parameters$q_dev)>1 & length(output$parameters$intercept)>1){
  gridExtra::grid.arrange(p2,p3, nrow=1)
}

if(length(output$parameters$tau_int)>1 & length(output$parameters$q_dev)>1 & length(output$parameters$intercept)>1){
  gridExtra::grid.arrange(p1,p2,p3, nrow=2)
}
  
}

