LR_CI_lower_c0 = results_matrix[ !is.na(bbinom_LR_CI1) & criterion == 0, {
    list(
        bbinom_LR_CI1 = mean( bbinom_LR_CI1 ),
        bbinom_LR_CI2 = mean( bbinom_LR_CI2 ),
        binom_LR_CI1 = mean( binom_LR_CI1 ),
        binom_LR_CI2 = mean( binom_LR_CI2 ),
        bbinom_LR_CI_prop_given_binom = median(bbinom_LR_CI_prop_given_binom)
    )
}, by = c("d_prime", "overdispersion", "criterion", "n_trials", "n_replications") ]

LR_CI_lower_c1 = results_matrix[ !is.na(bbinom_LR_CI1) & criterion == 0.1, {
    list(
        bbinom_LR_CI1 = mean( bbinom_LR_CI1 ),
        bbinom_LR_CI2 = mean( bbinom_LR_CI2 ),
        binom_LR_CI1 = mean( binom_LR_CI1 ),
        binom_LR_CI2 = mean( binom_LR_CI2 ),
        bbinom_LR_CI_prop_given_binom = median(bbinom_LR_CI_prop_given_binom)
    )
}, by = c("d_prime", "overdispersion", "criterion", "n_trials", "n_replications") ]

temp_data = LR_CI_lower_c0[ overdispersion %in% c(0,0.1)] 

LR_CI_lower_plot_c0 = ggplot() +
    geom_point( aes( x = binom_LR_CI1, y = bbinom_LR_CI1, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin LR CI lower bound" ) +
    scale_y_continuous( name = "BB LR CI lower bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = "none" )

LR_CI_upper_plot_c0 = ggplot() +
    geom_point( aes( x = binom_LR_CI2, y = bbinom_LR_CI2, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin LR CI upper bound" ) +
    scale_y_continuous( name = "BB LR CI upper bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = c(0.2,0.7), legend.text = element_text(size = 7),
        legend.title = element_text(size = 7), legend.key = element_rect(fill = "transparent",
            colour = "transparent" ), legend.key.height = unit(0.3,"cm"),
        legend.background = element_blank() )


temp_data = LR_CI_lower_c1[ overdispersion %in% c(0,0.1)] 

LR_CI_lower_plot_c1 = ggplot() +
    geom_point( aes( x = binom_LR_CI1, y = bbinom_LR_CI1, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin LR CI lower bound" ) +
    scale_y_continuous( name = "BB LR CI lower bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = "none" )

LR_CI_upper_plot_c1 = ggplot() +
    geom_point( aes( x = binom_LR_CI2, y = bbinom_LR_CI2, shape = factor(overdispersion) ), 
        temp_data, size = 0.5 ) +
    geom_abline( slope = 1, alpha = 0.1 ) +
    scale_x_continuous( name = "Bin LR CI upper bound" ) +
    scale_y_continuous( name = "BB  LR CI upper bound" ) +
    scale_shape_manual( name = "Overdispersion", values = c(1,4) ) +
    ggtheme +
    theme( legend.position = c(0.2,0.7), legend.text = element_text(size = 7),
        legend.title = element_text(size = 7), legend.key = element_rect(fill = "transparent",
            colour = "transparent" ), legend.key.height = unit(0.3,"cm"),
        legend.background = element_blank() )


c0_plots = grid.arrange( LR_CI_lower_plot_c0, LR_CI_upper_plot_c0, 
    top = textGrob( expression(paste(italic("c"), " = 0")) ), ncol = 1 )
c1_plots = grid.arrange( LR_CI_lower_plot_c1, LR_CI_upper_plot_c1, 
    top = textGrob( expression(paste(italic("c"), " = 0.1")) ), ncol = 1 )

LR_CI_plots = grid.arrange( c1_plots, c0_plots, ncol = 2 )

ggsave( filename = "LR_CI_plots.tiff", plot = LR_CI_plots, device = "tiff",
    width = 12, height = 12, units = "cm", dpi = 2000, compression = "lzw" )


source( "big_line_plot_LR.R" )

temp_data = LR_CI_lower_c0
LR_CI_conditional_alpha_plot_c0_d05 = make_big_line_plot2( 0.5, temp_data, TRUE,
    criterion = 0)
LR_CI_conditional_alpha_plot_c0_d2 = make_big_line_plot2( 2, temp_data, FALSE,
    criterion = 0)

temp_data = LR_CI_lower_c1
LR_CI_conditional_alpha_plot_c1_d05 = make_big_line_plot2( 0.5, temp_data, TRUE,
    criterion = 0.1)
LR_CI_conditional_alpha_plot_c1_d2 = make_big_line_plot2( 2, temp_data, FALSE,
    criterion = 0.1 )

LR_conditional_plots = grid.arrange(
    LR_CI_conditional_alpha_plot_c0_d05, LR_CI_conditional_alpha_plot_c0_d2,
    LR_CI_conditional_alpha_plot_c1_d05, LR_CI_conditional_alpha_plot_c1_d2
)
ggsave( filename = "LR_conditional_plots.tiff", plot = LR_conditional_plots, device = "tiff",
    width = 16, height = 16, units = "cm", dpi = 2000, compression = "lzw" )
