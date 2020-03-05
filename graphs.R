library( ggplot2 )
library( data.table )
library( grid )
library( gridExtra )

load( "raw_results.RData" )

ggtheme = theme( axis.text = element_text(size = 7), axis.title = element_text( size = 7 ),
    plot.title = element_text(hjust = 0.5, size = 7)  )

dprime_estimates = results_matrix[ , {
    list(
        bbinom_estimates.dprime = mean( bbinom_estimates.dprime, na.rm = T ),
        binom_estimates.dprime = mean( binom_estimates.dprime, na.rm = T )
    )
}, by = c("d_prime", "overdispersion", "criterion", "n_trials", "n_replications") ]

# ggplot() +
#     geom_point( aes( y = bbinom_estimates.dprime, x = overdispersion, colour = factor(n_trials),
#         shape = factor(n_replications)
#         ), dprime_estimates[criterion==0 & d_prime == 2] ) +
#     geom_line( aes( y = bbinom_estimates.dprime, x = overdispersion,
#         group = interaction(factor(n_trials), factor(n_replications) ),
#         size = factor(n_replications) ),
#         dprime_estimates[criterion==0  & d_prime == 2 & n_replications %in% c(2, 8, 32)] ) +
#     scale_size_manual( values = c(0.5, 1, 1.5) )


estimate_plots = lapply( unique(dprime_estimates$overdispersion), function(x){
    plot_ = ggplot() +
        geom_point( aes( x = bbinom_estimates.dprime, y = binom_estimates.dprime),
            results_matrix[ overdispersion == x ][!is.na(bbinom_estimates.dprime)], size = 0.1, alpha = 0.25 ) +
        scale_x_continuous( name = expression( paste( "Beta-binomial ", italic("d'"), " estimate" ) ),
            limits = c(-2.5, 5), breaks = c(-2,0,2,4) ) +
        scale_y_continuous( name = expression( paste( "Binomial ", italic("d'"), " estimate" ) ),
            limits = c(-2.5, 5), breaks = c(-2,0,2,4) ) +
        geom_abline( slope = 1, alpha = 0.25 ) +
        ggtitle( label = bquote(paste(gamma, " = ", .(x))) ) +
        ggtheme
    plot_
} )

dprime_estimates_binomial_vs_bbinom = arrangeGrob(grobs = estimate_plots)

ggsave( filename = "dprime_estimates_binomial_vs_bbinom.tiff", plot = dprime_estimates_binomial_vs_bbinom, device = "tiff",
    width = 12, height = 12, units = "cm", dpi = 2000, compression = "lzw" )

# dprime_estimates_binomial_vs_bbinom = ggplot() +
#     geom_point( aes( x = bbinom_estimates.dprime, y = binom_estimates.dprime),
#         dprime_estimates[!is.na(bbinom_estimates.dprime)] ) +
#     scale_x_continuous( name = expression( paste( "Beta-binomial ", italic("d'"), " estimate" ) ) ) +
#     scale_y_continuous( name = expression( paste( "Binomial ", italic("d'"), " estimate" ) ) ) +
#     geom_abline( slope = 1 ) +
#     ggtheme
# 
# 
# ggsave( "dprime_estimates_binomial_vs_bbinom.tiff", dprime_estimates_binomial_vs_bbinom, 
#     device = "tiff", width = unit(2, "cm"), height = unit(2,"cm"), dpi = 2000, compression = "lzw" )

##### Wald interval plots ####
wald_CI_lower_c0 = results_matrix[ !is.na(bbinom_wald_CI1) & criterion == 0, {
    list(
        bbinom_wald_CI1 = mean( bbinom_wald_CI1 ),
        bbinom_wald_CI2 = mean( bbinom_wald_CI2 ),
        binom_wald_CI1 = mean( binom_wald_CI1 ),
        binom_wald_CI2 = mean( binom_wald_CI2 ),
        bbinom_wald_CI_prop_given_binom = median(bbinom_wald_CI_prop_given_binom)
    )
}, by = c("d_prime", "overdispersion", "criterion", "n_trials", "n_replications") ]

wald_CI_lower_c1 = results_matrix[ !is.na(bbinom_wald_CI1) & criterion == 0.1, {
    list(
        bbinom_wald_CI1 = mean( bbinom_wald_CI1 ),
        bbinom_wald_CI2 = mean( bbinom_wald_CI2 ),
        binom_wald_CI1 = mean( binom_wald_CI1 ),
        binom_wald_CI2 = mean( binom_wald_CI2 ),
        bbinom_wald_CI_prop_given_binom = median(bbinom_wald_CI_prop_given_binom)
    )
}, by = c("d_prime", "overdispersion", "criterion", "n_trials", "n_replications") ]

temp_data = wald_CI_lower_c0[ overdispersion %in% c(0,0.1) ]

wald_CI_lower_plot_c0 = ggplot() +
    geom_point( aes( x = binom_wald_CI1, y = bbinom_wald_CI1, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin Wald CI lower bound" ) +
    scale_y_continuous( name = "BB Wald CI lower bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = "none" )

wald_CI_upper_plot_c0 = ggplot() +
    geom_point( aes( x = binom_wald_CI2, y = bbinom_wald_CI2, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin Wald CI upper bound" ) +
    scale_y_continuous( name = "BB Wald CI upper bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = c(0.2,0.7), legend.text = element_text(size = 7),
        legend.title = element_text(size = 7), legend.key = element_rect(fill = "transparent",
            colour = "transparent" ), legend.key.height = unit(0.3,"cm"),
        legend.background = element_blank() )


temp_data = wald_CI_lower_c1[ overdispersion %in% c(0,0.1) ]

wald_CI_lower_plot_c1 = ggplot() +
    geom_point( aes( x = binom_wald_CI1, y = bbinom_wald_CI1, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin Wald CI lower bound" ) +
    scale_y_continuous( name = "BB Wald CI lower bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = "none" )

wald_CI_upper_plot_c1 = ggplot() +
    geom_point( aes( x = binom_wald_CI2, y = bbinom_wald_CI2, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin Wald CI upper bound" ) +
    scale_y_continuous( name = "BB  Wald CI upper bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = c(0.2,0.7), legend.text = element_text(size = 7),
        legend.title = element_text(size = 7), legend.key = element_rect(fill = "transparent",
            colour = "transparent" ), legend.key.height = unit(0.3,"cm"),
        legend.background = element_blank() )


c0_plots = grid.arrange( wald_CI_upper_plot_c0, wald_CI_lower_plot_c0, 
    top = textGrob( expression(paste(italic("c"), " = 0")) ), ncol = 1 )
c1_plots = grid.arrange( wald_CI_upper_plot_c1, wald_CI_lower_plot_c1, 
    top = textGrob( expression(paste(italic("c"), " = 0.1")) ), ncol = 1 )



wald_CI_plots = grid.arrange( c0_plots, c1_plots, ncol = 2 )

ggsave( filename = "wald_CI_plots.tiff", plot = wald_CI_plots, device = "tiff",
    width = 12, height = 12, units = "cm", dpi = 2000, compression = "lzw" )

######################################

##### Conditional plots ################
source( "big_line_plot.R" )
temp_data = wald_CI_lower_c0
wald_CI_conditional_alpha_plot_c0_d05 = make_big_line_plot( 0.5, temp_data, TRUE,
    criterion = 0)
wald_CI_conditional_alpha_plot_c0_d2 = make_big_line_plot( 2, temp_data, FALSE,
    criterion = 0)

temp_data = wald_CI_lower_c1
wald_CI_conditional_alpha_plot_c1_d05 = make_big_line_plot( 0.5, temp_data, TRUE,
    criterion = 0.1)
wald_CI_conditional_alpha_plot_c1_d2 = make_big_line_plot( 2, temp_data, FALSE,
    criterion = 0.1 )

wald_conditional_plots = grid.arrange(
    wald_CI_conditional_alpha_plot_c0_d05, wald_CI_conditional_alpha_plot_c0_d2,
    wald_CI_conditional_alpha_plot_c1_d05, wald_CI_conditional_alpha_plot_c1_d2
)
ggsave( filename = "wald_conditional_plots.tiff", plot = wald_conditional_plots, device = "tiff",
    width = 16, height = 16, units = "cm", dpi = 2000, compression = "lzw" )
