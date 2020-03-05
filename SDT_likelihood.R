SDT_likelihood = function( param_vector, bbinom = FALSE, dataset ){
    
    hits = dataset$hit
    fas = dataset$fa
    n_trials = dataset$trials[1]
    d_prime = param_vector["dprime"]
    criterion = param_vector["criterion"]
    logit_overdispersion = param_vector["logit_overdispersion"]
    overdispersion = exp(logit_overdispersion)/(1+exp(logit_overdispersion))
    
    hit_rates = pnorm( (-2*criterion + d_prime)/2)
    fa_rates = pnorm( (-2*criterion - d_prime)/2)
        
    if ( bbinom ){
        bbinom_a_hit = (1/overdispersion - 1 )*hit_rates
        bbinom_a_fa = (1/overdispersion - 1 )*fa_rates
        bbinom_b_hit = (overdispersion-1)*(hit_rates-1)/overdispersion
        bbinom_b_fa = (overdispersion-1)*(fa_rates-1)/overdispersion
        
        loglik_hits = sum( dbbinom( hits, n_trials, bbinom_a_hit, bbinom_b_hit, log = T ) )
        loglik_fa = sum( dbbinom( fas, n_trials, bbinom_a_fa, bbinom_b_fa, log = T ) )
    } else{
        loglik_hits = sum( dbinom( hits, n_trials, hit_rates, log = T ) )
        loglik_fa = sum( dbinom( fas, n_trials, fa_rates, log = T ) )
    }
    
    negloglik = -sum(loglik_hits, loglik_fa)
    
    negloglik
}