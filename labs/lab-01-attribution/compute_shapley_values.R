
shapley_attribution <- function(data){
# adapted from https://trevorwithdata.com/attribution-theory-the-two-best-models-for-algorithmic-marketing-attribution-implemented-in-apache-spark-and-r/
data_seq <- 
    data %>%
    group_by(cookie) %>%
    # data are arranged in time so skipping this step
    # arrange(hit_time_gmt) %>%
    mutate(order_seq = ifelse(conversion > 0, 1, NA)) %>%
    mutate(order_seq = lag(cumsum(ifelse(is.na(order_seq), 0, order_seq)))) %>%
    mutate(order_seq = ifelse((row_number() == 1) & (conversion > 0), 
                          -1, ifelse(row_number() == 1, 0, order_seq))) %>% 
    ungroup()

seq_summaries <-
    data_seq %>%
    group_by(cookie, order_seq) %>%
    summarize(
        facebook_touches = max(ifelse(channel == "Facebook",1,0)),
        instagram_touches = max(ifelse(channel == "Instagram",1,0)),
        display_touches = max(ifelse(channel == "Online Display",1,0)),
        video_touches = max(ifelse(channel == "Online Video",1,0)),
        paid_search_touches = max(ifelse(channel == "Paid Search",1,0)),
        conversions = sum(conversion)
    ) %>% 
    ungroup()

# Sum up the number of sequences and conversions
# for each combination of marketing channels
conv_rates <- 
    seq_summaries %>%
    group_by(facebook_touches,
             instagram_touches,
             display_touches,
             paid_search_touches,
             video_touches ) %>%
    summarize(
        conversions = sum(conversions),
        total_sequences = n()
    )

# number of marketing channels
number_of_channels <- 5

# The coalitions function is a handy function from the GameTheoryALlocation
# library that creates a binary matrix to which you can fit your
# characteristic function (more on this in a bit) 
touch_combos <- as.data.frame(coalitions(number_of_channels)$Binary)
names(touch_combos) <- c("Facebook","Instagram","Online Display",
                        "Online Video","Paid Search")

# Now I'll join my previous summary results with the binary matrix
# the GameTheoryAllocation library built for me.
touch_combo_conv_rate = left_join(touch_combos, conv_rates, 
                                  by = c(
                                      "Facebook"="facebook_touches",
                                      "Instagram" = "instagram_touches",
                                      "Online Display" = "display_touches",
                                      "Paid Search" = "paid_search_touches",
                                      "Online Video" = "video_touches"
                                  )
)

# Finally, I'll fill in any NAs with 0
touch_combo_conv_rate = touch_combo_conv_rate %>%
    mutate_all(funs(ifelse(is.na(.),0,.))) %>%
    mutate(
        conv_rate = ifelse(total_sequences > 0, conversions/total_sequences, 0)
    )    

# Building Shapley Values for each channel combination

shap_vals = as.data.frame(coalitions(number_of_channels)$Binary)
names(shap_vals) = c("Facebook","Instagram","Online Display",
                     "Online Video","Paid Search")
coalition_mat = shap_vals
shap_vals[2^number_of_channels,] = Shapley_value(touch_combo_conv_rate$conv_rate, game="profit")

for(i in 2:(2^number_of_channels-1)){
    if(sum(coalition_mat[i,]) == 1){
        shap_vals[i,which(shap_vals[i,]==1)] = touch_combo_conv_rate[i,"conv_rate"]
    }else if(sum(coalition_mat[i,]) > 1){
        if(sum(coalition_mat[i,]) < number_of_channels){
            channels_of_interest = which(coalition_mat[i,] == 1)
            char_func = data.frame(rates = touch_combo_conv_rate[1,"conv_rate"])
            for(j in 2:i){
                if(sum(coalition_mat[j,channels_of_interest])>0 & 
                   sum(coalition_mat[j,-channels_of_interest])==0)
                    char_func = rbind(char_func,touch_combo_conv_rate[j,"conv_rate"])
            }
            shap_vals[i,channels_of_interest] = 
                Shapley_value(char_func$rates, game="profit")
        }
    }
}

# Apply Shapley Values as attribution weighting
order_distribution = shap_vals * touch_combo_conv_rate$total_sequences
shapley_value_orders = t(t(round(colSums(order_distribution))))
shapley_value_orders = data.frame(channel = row.names(shapley_value_orders), 
                                  conversions = as.numeric(shapley_value_orders))


return(shapley_value_orders)
}
