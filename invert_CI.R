LR_CI = function( MLE_vector, dataset, bbinom ){
    
    MLE_loglik = -SDT_likelihood( MLE_vector, bbinom, dataset )
    
    loss_fx = function( dprime, MLE_vector, MLE_loglik, dataset ){
        param_vector2 = c(dprime = dprime, MLE_vector[-1])
        loglik = -SDT_likelihood( param_vector2, bbinom, dataset )
        (MLE_loglik - qchisq(0.95,1)/2 - loglik)^2
    }
    
    lower_bound = optimise( f = loss_fx, interval = c(-10,MLE_vector[1]), MLE_vector = MLE_vector, dataset = dataset,
        MLE_loglik = MLE_loglik )
    upper_bound = optimise( f = loss_fx, interval = c(MLE_vector[1], 10), MLE_vector = MLE_vector, dataset = dataset,
        MLE_loglik = MLE_loglik )
    confidence_interval = c( lower_bound$minimum, upper_bound$minimum )
}

wald_CI = function( optim_results ){
    x = optim_results$par[1] + c(-1,1) * 1.96*sqrt(abs(diag(solve(optim_results$hessian))[1]))
    if ( any(is.na(x)) ) stop()
    x
}

wald_bbinom_CI_given_binom = function( binom_wald_CI, bbinom_results ){
    
    se_ = sqrt(abs( diag( solve( bbinom_results$hessian ) ) ) )[1]
    if ( any( is.na(se_) ) ) stop()
    dprime_mle = bbinom_results$par[1]
    
    loss_fx = function(quantile_){
        sum( (binom_wald_CI - (dprime_mle + c(-1,1) * qnorm(quantile_) * se_))^2 )
    }
    
    norm_prob = optim( c("quantile_" = 0.975), loss_fx, method = "Brent",
        lower = 0, upper = 1)
    1-(1-norm_prob$par)*2
}


wald_binom_CI_given_bbinom = function( bbinom_wald_CI, binom_results ){
    
    se_ = sqrt(abs( diag( solve( binom_results$hessian ) ) ) )[1]
    if ( any( is.na(se_) ) ) stop()
    dprime_mle = binom_results$par[1]
    
    loss_fx = function(quantile_){
        sum( (bbinom_wald_CI - (dprime_mle + c(-1,1) * qnorm(quantile_) * se_))^2 )
    }
    
    norm_prob = optim( c("quantile_" = 0.975), loss_fx, method = "Brent",
        lower = 0, upper = 1)
    1-(1-norm_prob$par)*2
}


LR_bbinom_CI_given_binom = function( binom_LR_CI, bbinom_results, dataset ){
    
    MLE_vector = bbinom_results$par
    lower_binom_CI = c(dprime = binom_LR_CI[1], MLE_vector[-1])
    upper_binom_CI = c(dprime = binom_LR_CI[2], MLE_vector[-1])
    
    lower_loglik = -SDT_likelihood( lower_binom_CI, TRUE, dataset )
    upper_loglik = -SDT_likelihood( upper_binom_CI, TRUE, dataset )
    MLE_loglik = -SDT_likelihood( MLE_vector, TRUE, dataset )
    
    lower_loss_fx = function( chisq_prob ){
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - lower_loglik)^2
    }
    upper_loss_fx = function( chisq_prob ){
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - upper_loglik)^2
    }
    loss_fx = function( chisq_prob ){
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - lower_loglik)^2 +
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - upper_loglik)^2
    }
    
    chisq_prob = optimise( f = loss_fx, interval = c(0,1) )
    chisq_prob$minimum
}

LR_binom_CI_given_bbinom = function( bbinom_LR_CI, binom_results, dataset ){
    
    MLE_vector = binom_results$par
    lower_bbinom_CI = c(dprime = bbinom_LR_CI[1], MLE_vector[-1])
    upper_bbinom_CI = c(dprime = bbinom_LR_CI[2], MLE_vector[-1])
    
    lower_loglik = -SDT_likelihood( lower_bbinom_CI, FALSE, dataset )
    upper_loglik = -SDT_likelihood( upper_binom_CI, FALSE, dataset )
    MLE_loglik = -SDT_likelihood( MLE_vector, FALSE, dataset )
    
    lower_loss_fx = function( chisq_prob ){
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - lower_loglik)^2
    }
    upper_loss_fx = function( chisq_prob ){
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - upper_loglik)^2
    }
    loss_fx = function( chisq_prob ){
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - lower_loglik)^2 +
        (MLE_loglik - qchisq(chisq_prob, 1)/2 - upper_loglik)^2
    }
    
    chisq_prob = optimise( f = loss_fx, interval = c(0,1) )
    chisq_prob$minimum
}

do_confidence_intervals = function( dataset ){
    
    param_vector = c("dprime" = 1, "criterion" = 0, "logit_overdispersion" = -10.01)
    try_again = TRUE
    
    while( try_again ){
        bbinom_results = optim( param_vector, SDT_likelihood, dataset = dataset, bbinom = TRUE, hessian = TRUE )
        param_vector[3] = runif( 1, -11,-9 )
        try_ = try( solve( bbinom_results$hessian) )
        if ( class(try_) == "try-error" ){
            try_again = TRUE
        } else{
            try_again = FALSE
        }
    }
    
    bbinom_wald_CI = wald_CI( bbinom_results )
    bbinom_LR_CI = LR_CI( bbinom_results$par, dataset, TRUE )
    
    
    binom_results = optim( param_vector[1:2], SDT_likelihood, dataset = dataset, bbinom = FALSE, hessian = TRUE )
    binom_wald_CI = wald_CI( binom_results )
    binom_LR_CI = LR_CI( binom_results$par, dataset, FALSE )
    
    bbinom_wald_CI_prop_given_binom = wald_bbinom_CI_given_binom( binom_wald_CI, bbinom_results )
    bbinom_LR_CI_prop_given_binom = LR_bbinom_CI_given_binom( binom_LR_CI, bbinom_results, dataset )
    
    binom_wald_CI_prop_given_bbinom = wald_binom_CI_given_bbinom( bbinom_wald_CI, binom_results )
    binom_LR_CI_prop_given_bbinom = LR_binom_CI_given_bbinom( bbinom_LR_CI, binom_results, dataset )
    
    c(
        bbinom_estimates = bbinom_results$par,
        bbinom_wald_CI = bbinom_wald_CI,
        bbinom_LR_CI = bbinom_LR_CI,
        binom_estimates = binom_results$par,
        binom_wald_CI = binom_wald_CI,
        binom_LR_CI = binom_LR_CI,
        bbinom_wald_CI_prop_given_binom = bbinom_wald_CI_prop_given_binom,
        bbinom_LR_CI_prop_given_binom = bbinom_LR_CI_prop_given_binom
        # binom_wald_CI_prop_given_bbinom = binom_wald_CI_prop_given_bbinom,
        # binom_LR_CI_prop_given_bbinom = binom_LR_CI_prop_given_bbinom
    )
}


























