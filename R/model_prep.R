
#' Prepare data and parameters for model run
#'
#' @param n_covars Number of covariates to explain fishing effort (integer)
#' @param E_intercept Binary to determine whether the effort intercept should be treated as a random effect. 0 indicates estimate as fixed effect, 1 indicates estimate as random effect. (binary)
#' @param C_intercept Scalar to determine whether the catch intercept should be estimated and if so if it should be estimated as a random effect. 0 indicates do not estimate, 1 indicates estimate as fixed effect, 2 indicates estimate as random effect. (scalar)
#' @param catchability Binary to determine whether the catchability coefficient should be treated as a random effect. 0 indicates estimate as fixed effect, 1 indicates estimate as random effect. (binary)
#' @param catch Input for desired catch data. No transformations are applied, so log(catch) should be input if that is the desired relationship to test. (numeric)
#' @param effort Input for desired effort data. No transformations are applied, so log(effort) should be input if that is the desired relationship to test. (numeric)
#' @param random_effect Input for random effect data. Should be the same length as catch and effort data. (integer)
#' @param first_covar Input for first covariate data. (numeric)
#' @param second_covar Input for second covariate data. (numeric)
#' @param third_covar Input for third covariate data. Note that current package functionality only allows this covariate to effect the model if not estimating separate Great Lakes waterbody area slopes (numeric)
#' @param gl_switch Binary to determine whether there should be different slopes between Great Lakes and non-Great Lakes waterbody area influence. 0 indicates only one slope, 1 indicates separate slopes (binary)
#' @param gl_area Input for waterbody area data if estimating different slopes between Great Lakes and non-Great Lakes waterbodies. (numeric)
#' @param iGL Binary vector indicating whether a waterbody is from the Great Lakes (1) or not (0). (binary vector)
#'
#' @return list
#' @export
#'
#' @examples
#' model_prep(n_covars = 0, E_intercept = 1, C_intercept = 0, catchability = 0,
#' catch = log(useable_dat$cpd), effort = log(useable_dat$epd), random_effect = useable_dat$istate,
#' first_covar = useable_dat$all_dev, second_covar = useable_dat$forest_wetland,
#' third_covar = useable_dat$Income, gl_area = useable_dat$Area, gl_switch = 0, iGL = useable_dat$GL)
model_prep<-function(n_covars = 0, E_intercept = 1, C_intercept = 0, catchability = 0,
                     catch=0, effort=0, random_effect=0,
                     first_covar=0, second_covar=0,
                     third_covar=0, gl_switch = 0, gl_area=0, iGL=0){
  
  if( any(is.na(c(catch, effort, random_effect, gl_area, first_covar, second_covar, third_covar, iGL))) ) stop('data cannot have NA values')
  
  if(length(catch)<=1) stop("need to include catch data")
  if(length(effort)<=1) stop("need to include effort data")
  
  if(n_covars == 1 & gl_switch == 0 & length(first_covar)<=1) stop("need to include first_covar data")
  if(n_covars == 2 & gl_switch == 0 & (length(first_covar)<=1|length(second_covar)<=1)) stop("need to include first_covar and second_covar data")
  if(n_covars == 3 & gl_switch == 0 & (length(first_covar)<=1|length(second_covar)<=1|length(third_covar)<=1)) stop("need to include first_covar, second_covar, and third_covar data")
  
  if(E_intercept==1 & length(random_effect)<=1) stop("need to include random effect data")
  if(C_intercept==2 & length(random_effect)<=1) stop("need to include random effect data")
  if(catchability==1 & length(random_effect)<=1) stop("need to include random effect data")
  
  if(gl_switch==1 & length(gl_area)<=1) stop("need to include Great Lakes area data in gl_area")
  if(gl_switch==1 & length(iGL)<=1) stop("need to include data identifying Great Lakes waterbodies in iGL")
  
  if(C_intercept < 2){intercept_RE_switch=0}else{
    intercept_RE_switch=1
  }

  if(C_intercept > 0){intercept_switch=1}else{
    intercept_switch=0
  }

  if(min(random_effect)!=0){
    random_effect<-as.numeric(random_effect)-1
  }

  tmb.data<-list(
    log_C=catch,
    log_E=effort,
    gl_area_covar=gl_area,
    first_covar=first_covar,
    second_covar=second_covar,
    third_covar=third_covar,
    random_effect = random_effect,
    iGL = iGL,
    GL_switch = gl_switch,
    effort_switch = E_intercept,
    intercept_switch = intercept_switch,
    q_re_switch = catchability,
    intercept_re_switch = intercept_RE_switch,
    n_covars=n_covars
  )


  if(tmb.data$n_covars==0& gl_switch == 0){tau=rep(1,1)}
  if(tmb.data$n_covars==1& gl_switch == 0){tau=rep(1,1)}
  if(tmb.data$n_covars==2& gl_switch == 0){tau=rep(1,2)}
  if(tmb.data$n_covars==3& gl_switch == 0){tau=rep(1,3)}

  if(tmb.data$n_covars==1& gl_switch == 1){tau=rep(1,1)}
  if(tmb.data$n_covars==2& gl_switch == 1){tau=rep(1,1)}
  if(tmb.data$n_covars==3& gl_switch == 1){tau=rep(1,2)}
  
  
  if(tmb.data$GL_switch==0){tau_gl = rep(1,1)}
  if(tmb.data$GL_switch==1){tau_gl = rep(1,2)}

  if(tmb.data$effort_switch==0){tau_int=1}
  if(tmb.data$effort_switch==1){tau_int = rep(1, length(unique(tmb.data$random_effect)))}

  if(tmb.data$intercept_switch==0){intercept=0}
  if(tmb.data$intercept_switch==1 & tmb.data$intercept_re_switch==0){intercept=0}
  if(tmb.data$intercept_switch==1 & tmb.data$intercept_re_switch==1){intercept=rep(0,  length(unique(tmb.data$random_effect)))}

  if(tmb.data$q_re_switch==0){q_dev=0.1}
  if(tmb.data$q_re_switch==1){q_dev=rep(0.1,  length(unique(tmb.data$random_effect)))}


  parameters<-list(
    #q_mean = 1,
    intercept=intercept,
    q_dev = q_dev,
    tau_int = tau_int,
    tau = tau,
    tau_gl = tau_gl,
    log_sd = rep(log(2),2)
  )


  rname_temp = na.omit(c(ifelse(tmb.data$q_re_switch==1,  "q_dev" ,NA),
                         ifelse(tmb.data$intercept_re_switch==1,  "intercept" ,NA),
                         ifelse(tmb.data$effort_switch==1,  "tau_int" ,NA)))
  rname<-NULL
  if(length(rname_temp)!=0)rname<-rname_temp

  map<-list()

  if(tmb.data$intercept_switch==0){
    map$intercept=factor(NA)
  }

  if(tmb.data$n_covars==0){
    map$tau=factor(NA)
  }
  
  if(tmb.data$n_covars==1 & tmb.data$GL_switch==1){
    map$tau=factor(NA)
  }

  if(tmb.data$GL_switch==0){
    map$tau_gl=factor(NA)
  }

  output_list<-list(
    tmb.data=tmb.data,
    parameters=parameters,
    map=map,
    rname=rname
  )

  return(output_list)
}
