compute_ad_stocks <- function(df){
    #set adstock tv rate
    set_rate_x1 <- 0.5
    set_memory <- 4
    get_adstock_x1 <- rep(set_rate_x1, set_memory+1) ^ c(0:set_memory)
    
    #adstocked tv
    ads_x1 <- stats::filter(c(rep(0, set_memory), df$tv_spend), get_adstock_x1, method="convolution")
    ads_x1 <- ads_x1[!is.na(ads_x1)]
    
    new_df <-
        cbind(df, ads_x1) %>%
        rename(
              tv_stock = ads_x1,
              #x2_stock = ads_x2
              ) %>%
        tail(-4)
    
    return(new_df)
}