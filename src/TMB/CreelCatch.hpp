
#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template<class Type>
Type CreelCatch(objective_function<Type>* obj){

  DATA_VECTOR(log_C);
  DATA_VECTOR(log_E);
  DATA_VECTOR(gl_area_covar);
  DATA_VECTOR(first_covar);
  DATA_VECTOR(second_covar);
  DATA_VECTOR(third_covar);
  DATA_IVECTOR(random_effect);
  DATA_IVECTOR(iGL);
  DATA_SCALAR(GL_switch);
  DATA_SCALAR(effort_switch);
  DATA_SCALAR(intercept_switch);
  DATA_SCALAR(q_re_switch);
  DATA_SCALAR(intercept_re_switch);
  DATA_SCALAR(n_covars);

  int N = log_C.size();
  Type zero = 0.0;

  PARAMETER_VECTOR(intercept);
  PARAMETER_VECTOR(q_dev);
  PARAMETER_VECTOR(tau_int);
  PARAMETER_VECTOR(tau);
  PARAMETER_VECTOR(tau_gl);
  PARAMETER_VECTOR(log_sd);

  //vector<Type> q(n_state);
  vector<Type> sd = exp(log_sd);
  vector<Type> pred_C(N);
  vector<Type> pred_E(N);
  vector<Type> C_resid(N);
  vector<Type> E_resid(N);
  vector<Type> std_resid_E(N);
  vector<Type> std_resid_C(N);

  using namespace density;
  Type nll = 0.0;


  int it, igl;

  vector<Type> log_pred_C = log(pred_C);
  vector<Type> log_pred_E = log(pred_E);

  for(int n = 0;n < N;++n){
    it = random_effect(n);
    igl = iGL(n);

    if(effort_switch==0){

      if(n_covars==0){
        log_pred_E(n) = tau_int(0);
      }

      if(n_covars==1){
        if(GL_switch==0){
          log_pred_E(n) = tau_int(0)+ tau(0)*first_covar(n);
        }

      if(GL_switch==1){
        if(igl==0){
          log_pred_E(n) = tau_int(0)+ tau_gl(0)*gl_area_covar(n);
        }
        if(igl==1){
          log_pred_E(n) = tau_int(0)+ tau_gl(1)*gl_area_covar(n);
        }
      }
      }

      if(n_covars==2){

        if(GL_switch==0){
          log_pred_E(n) = tau_int(0)+ tau(0)*first_covar(n) + tau(1)*second_covar(n);
        }

        if(GL_switch==1){
        if(igl==0){
          log_pred_E(n) = tau_int(0)+ tau_gl(0)*gl_area_covar(n) + tau(0)*first_covar(n);
        }
        if(igl==1){
          log_pred_E(n) = tau_int(0)+ tau_gl(1)*gl_area_covar(n) + tau(0)*first_covar(n);
        }
      }
      }

      if(n_covars==3){

        if(GL_switch==0){
          log_pred_E(n) = tau_int(0)+ tau(0)*first_covar(n) + tau(1)*second_covar(n)+ tau(2)*third_covar(n);
        }

        if(GL_switch==1){
        if(igl==0){
          log_pred_E(n) = tau_int(0)+ tau_gl(0)*gl_area_covar(n) + tau(0)*first_covar(n)+ tau(1)*second_covar(n);
        }
        if(igl==1){
          log_pred_E(n) = tau_int(0)+ tau_gl(1)*gl_area_covar(n) + tau(0)*first_covar(n)+ tau(1)*second_covar(n);
        }
      }
    }
    }

    if(effort_switch==1){

      if(n_covars==0){
        log_pred_E(n) = tau_int(it);
      }

      if(n_covars==1){

        if(GL_switch==0){
          log_pred_E(n) = tau_int(it)+ tau(0)*first_covar(n);
        }

        if(GL_switch==1){
        if(igl==0){
          log_pred_E(n) = tau_int(it)+ tau_gl(0)*gl_area_covar(n);
        }
        if(igl==1){
          log_pred_E(n) = tau_int(it)+ tau_gl(1)*gl_area_covar(n);
        }
      }
      }

      if(n_covars==2){

        if(GL_switch==0){
          log_pred_E(n) = tau_int(it)+ tau(0)*first_covar(n) + tau(1)*second_covar(n);
        }

        if(GL_switch==1){
        if(igl==0){
          log_pred_E(n) = tau_int(it)+ tau_gl(0)*gl_area_covar(n) + tau(0)*first_covar(n);
        }
        if(igl==1){
          log_pred_E(n) = tau_int(it)+ tau_gl(1)*gl_area_covar(n) + tau(0)*first_covar(n);
        }
      }
      }

      if(n_covars==3){

        if(GL_switch==0){
          log_pred_E(n) = tau_int(it)+ tau(0)*first_covar(n) + tau(1)*second_covar(n) + tau(2)*third_covar(n);
        }

        if(GL_switch==1){
        if(igl==0){
          log_pred_E(n) = tau_int(it)+ tau_gl(0)*gl_area_covar(n) + tau(0)*first_covar(n)+ tau(1)*second_covar(n);
        }
        if(igl==1){
          log_pred_E(n) = tau_int(it)+ tau_gl(1)*gl_area_covar(n) + tau(0)*first_covar(n)+ tau(1)*second_covar(n);
        }
      }
    }
    }


    E_resid(n) = log_E(n) - log_pred_E(n);
    std_resid_E(n) = E_resid(n)/sd(0);

    if(intercept_switch==0){
      if(q_re_switch==0){
        log_pred_C(n) = q_dev(0) * log_pred_E(n);
      }
      if(q_re_switch==1){
        log_pred_C(n) = q_dev(it) * log_pred_E(n);
      }
    }

    if(intercept_switch==1){
      if(intercept_re_switch==0){
        if(q_re_switch==0){
          log_pred_C(n) = intercept(0) + q_dev(0) * log_pred_E(n);
        }
        if(q_re_switch==1){
          log_pred_C(n) = intercept(0) + q_dev(it) * log_pred_E(n);
      }
      }
      if(intercept_re_switch==1){
        if(q_re_switch==0){
          log_pred_C(n) = intercept(it) + q_dev(0) * log_pred_E(n);
        }
        if(q_re_switch==1){
          log_pred_C(n) = intercept(it) + q_dev(it) * log_pred_E(n);
        }
      }
    }


    C_resid(n) = log_C(n) - log_pred_C(n);
    std_resid_C(n) = C_resid(n)/sd(1);
  }

  for(int n = 0;n < N;++n){
    nll -= dnorm(E_resid(n),zero,sd(0),true);
    nll -= dnorm(C_resid(n),zero,sd(1),true);
  }

  REPORT(tau_int);
  REPORT(tau_gl);
  REPORT(intercept);
  REPORT(pred_C);
  REPORT(pred_E);
  REPORT(tau);
  REPORT(q_dev);
  REPORT(sd);
  REPORT(E_resid);
  REPORT(C_resid);
  REPORT(std_resid_E);
  REPORT(std_resid_C);

  ADREPORT(intercept);
  ADREPORT(q_dev);
  ADREPORT(tau);
  ADREPORT(tau_int);
  ADREPORT(tau_gl);

  return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
