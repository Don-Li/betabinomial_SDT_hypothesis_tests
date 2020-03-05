make_data = function( d_prime, overdispersion, criterion, n_trials, n_replications ){
    
    hit_rates = pnorm( (-2*criterion + d_prime)/2)
    fa_rates = pnorm( (-2*criterion - d_prime)/2)
    
    if ( overdispersion != 0 ){
        bbinom_a_hit = (1/overdispersion - 1 )*hit_rates
        bbinom_a_fa = (1/overdispersion - 1 )*fa_rates
        bbinom_b_hit = (overdispersion-1)*(hit_rates-1)/overdispersion
        bbinom_b_fa = (overdispersion-1)*(fa_rates-1)/overdispersion
        
        hits = rbbinom( n_replications, n_trials, bbinom_a_hit, bbinom_b_hit )
        fa = rbbinom( n_replications, n_trials, bbinom_a_fa, bbinom_b_fa )
    } else{
        hits = rbinom( n_replications, n_trials, hit_rates )
        fa = rbinom( n_replications, n_trials, fa_rates )
    }
    
    data.table(
        hit = hits, fa = fa,
        dprime = d_prime, criterion = criterion,
        overdispersion = overdispersion,
        trials = n_trials,
        replications = n_replications
    )
}
