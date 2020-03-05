make_big_line_plot = function( this_prime, dataset, legend, criterion ){
    text_labels = dataset[ d_prime == this_prime & n_trials %in% c(8, 32, 256)][ overdispersion == 0.1, {
        list( x = overdispersion, y = 1-bbinom_wald_CI_prop_given_binom, label = n_replications )
    }, by = c("n_trials") ]
    text_labels = text_labels[ order(y) ]
    text_labels[ , hjust := c(-0.1, -1)[ 1:.N %% 2 + 1] ]
    text_labels[ , label_pad := sprintf("%2.0f", label) ]
    text_labels = rbindlist( list(
        text_labels,
        list( x = 0.1, y = 0.75, label_pad = "Replications", hjust = 0 )
    ), use.names = T, fill = T )
    
    # if ( legend ){
        L = theme( legend.position = c(0.15,0.8), legend.text = element_text(size = 7),
        legend.title = element_text(size = 7), legend.key = element_rect(fill = "transparent",
            colour = "transparent" ), legend.key.height = unit(0.3,"cm"),
        legend.background = element_blank() )
    # } else{
    #     L = theme( legend.position = "none" )
    # }
    
    big_lines = ggplot() +
        geom_point( aes( x = overdispersion, y =1- bbinom_wald_CI_prop_given_binom ), dataset,
            size = 0.5 ) +
        geom_line( aes( x = overdispersion, y = 1-bbinom_wald_CI_prop_given_binom, 
            group = interaction(n_replications, n_trials), 
            linetype = factor(n_trials) ),
            temp_data[ d_prime == this_prime & n_trials %in% c(8, 32, 256) ], alpha = 0.5 ) +
        geom_text( aes( x = x, y = y, label = label_pad, hjust = hjust ), text_labels, size = 2 ) +
        # scale_size_manual( values = c(0.5, 1, 1.5) ) +
        scale_linetype_manual( values = c("solid", "dotted", "dashed" ) ) + 
        guides( linetype = guide_legend(
            title = substitute( paste("Trials | ", italic("d'"), " = ", x1),
                list(x1 = this_prime) ),
            override.aes = (list(alpha=1))),
        shape = guide_legend(title = "n replications") ) +
        scale_x_continuous( name = "Overdispersion", limits = c(0, 0.115) ) +
        scale_y_continuous( name = expression(paste("BB CI Type I error", " | 95% Bin CI")) ) +
        ggtheme +
        labs( title = substitute(
                paste( italic("c"), " = ", x2 ),
                list( x1 = this_prime, x2 = criterion )
        ) ) +
        L
    
    big_lines
}

