library( data.table )
library( extraDistr )
library( doParallel )

source( "make_data.R" )
source( "SDT_likelihood.R" )
source( "invert_CI.R" )

set.seed(1)

design_matrix = data.table( expand.grid(
    d_prime = c(0.25, 0.5, 1, 2, 3),
    overdispersion = c(0, 0.01, 0.05, 0.1),
    criterion = c(0, 0.1),
    n_trials = c(8, 16, 32, 64, 128, 256),
    n_replications = c(2, 4, 8, 16, 32)
) )
design_matrix = design_matrix[ order(overdispersion) ]

design_matrix = design_matrix[ d_prime == 1 & criterion == 0 ]

n_simulations = 1000

cl = makeCluster(4, outfile = "out.txt" )
registerDoParallel(cl)

results_matrix = data.table()

# for ( row_ in 1:nrow( design_matrix ) ){
results_matrix = foreach( row_ = 1:nrow( design_matrix ), .packages = c("data.table", "extraDistr") ) %dopar% {
    print( row_/nrow(design_matrix) )
    design_row = design_matrix[ row_ ]
    print( unlist(design_row) )
    
    ci_results = lapply( 1:n_simulations, function(x){
        if ( x %% n_simulations/2 == 5000 ) print(x)
        d_prime = design_row$d_prime
        overdispersion = design_row$overdispersion
        criterion = design_row$criterion
        n_trials = design_row$n_trials
        n_replications = design_row$n_replications
        
        dataset = make_data( d_prime, overdispersion, criterion, n_trials, n_replications )

        extreme_proportions = sum( dataset$hit == dataset$trials, dataset$hit == 0, 
            dataset$fa == dataset$trials, dataset$fa == 0 )
        
        if ( extreme_proportions >= dataset$replications[1] ){
            return( list(extreme_proportions = extreme_proportions) )
        }
        
        ci_results1 = list( extreme_proportions = extreme_proportions )
        
        ci_results = c( ci_results1, do_confidence_intervals( dataset ) )
        ci_results = as.list(ci_results)
        
        ci_results
    } )
    
    ci_results = rbindlist( ci_results, use.names = T, fill = T )
    ci_results[ , names(design_matrix) := design_matrix[row_] ]
    ci_results
    # results_matrix = rbindlist( list( results_matrix, ci_results ), use.names = T, fill = T )
}



results_matrix = rbindlist(results_matrix, use.names = T, fill = T )


# save( results_matrix, file = "raw_results.RData" )




# all_results = design_matrix[ , {
#     params_ = mget( names(design_row ) )
#     print( unlist(params_) )
#     ci_results = lapply( 1:100, function(x){
#         if ( x %% 500 == 0 ) print(x)
#         # d_prime = design_row$d_prime
#         # overdispersion = design_row$overdispersion
#         # criterion = design_row$criterion
#         # n_trials = design_row$n_trials
#         # n_replications = design_row$n_replications
#         
#         dataset = make_data( d_prime, overdispersion, criterion, n_trials, n_replications )
# 
#         extreme_proportions = sum( dataset$hit == dataset$trials, dataset$hit == 0, 
#             dataset$fa == dataset$trials, dataset$fa == 0 )
#         
#         if ( extreme_proportions >= dataset$replications[1] ){
#             return( list(extreme_proportions = extreme_proportions) )
#         }
#         
#         ci_results1 = list( extreme_proportions = extreme_proportions )
#         
#         ci_results = c( ci_results1, do_confidence_intervals( dataset ) )
#         ci_results = as.list(ci_results)
#         
#         ci_results
#     } )
#     
#     ci_results = rbindlist( ci_results, use.names = T, fill = T )
#     
# }, by = c("d_prime", "overdispersion", "criterion", "n_trials", "n_replications") ]
# 
# 
# 
# all_results[ ,{
#     median(bbinom_LR_CI_prop_given_binom, na.rm = T)
#     }, by = c("d_prime", "overdispersion", "criterion", "n_trials", "n_replications") ][,{
#         plot( overdispersion, V1 )
#         for (xx in unique(n_replications) ){
#             lines( overdispersion[d_prime == 2 & n_trials == 8 & n_replications == xx], 
#                 V1[d_prime == 2  & n_trials == 8 & n_replications == xx ] )
#             lines( overdispersion[d_prime == 2 & n_trials == 16 & n_replications == xx], 
#                 V1[d_prime == 2  & n_trials == 16 & n_replications == xx ], col = "red" )
#             lines( overdispersion[d_prime == 2 & n_trials == 32 & n_replications == xx], 
#                 V1[d_prime == 2  & n_trials == 32 & n_replications == xx ], col = "blue" )
#             lines( overdispersion[d_prime == 2 & n_trials == 64 & n_replications == xx], 
#                 V1[d_prime == 2  & n_trials == 64 & n_replications == xx ], col = "green" )
#             lines( overdispersion[d_prime == 2 & n_trials == 128 & n_replications == xx], 
#                 V1[d_prime == 2  & n_trials == 128 & n_replications == xx ], col = "orange" )
#             lines( overdispersion[d_prime == 2 & n_trials == 256 & n_replications == xx], 
#                 V1[d_prime == 2  & n_trials == 256 & n_replications == xx ], col = "yellow" )
#         }
# } ]



