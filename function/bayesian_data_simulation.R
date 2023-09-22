# simulation function
bayesian_data_simulation = function(alpha, beta, n_draw, obs, n, informative_prior_yn = TRUE, seed = 1234){
        
        # IMPORTANT: with large enough sample collection, prior does not really matter
        # whether it is uniform or beta distribution, the result should converge when large enough sample size is collected, i.e., large number of trials (alpha, beta) such as 1000 instead of just 10 trials
        # on the other hand, small or zero observation would turn the posterior odds very close or equal to the prior
        # informative prior is always preferred as it gives us more information about the data observed instead of simply guessing (assuming an uniform distribution)
        # Introduction to Bayesian data analysis - Part 2: Why use Bayes? -> https://www.youtube.com/watch?v=mAUwjSo5TJE
        
        set.seed(seed)
        
        # prior_rate
        if(informative_prior_yn) {
                # informative prior, i.e., beta distribution - alpha = num of success, beta = num of all (success + failure)
                prior_rate = rbeta(n_draw, shape1 = alpha, shape2 = beta)
        } else {
                # non-informative prior, i.e., uniform distribution - simply guessing
                prior_rate = runif(n_draw, min = 0, max = 1)
        }
        
        # generative model (binomial dist)
        gen_model = function(size, prob){
                rbinom(1, size, prob)
        }
        
        # simulation - return a series of likelihood, i.e., given the outcome (alpha), return a set of possible probabilities
        gen_series = prior_rate[sapply(1:length(prior_rate), \(x) gen_model(size = n, prob = prior_rate[x])) == obs]
        
        return(gen_series)
        
}

#exp_group = bayesian_data_simulation(alpha = 1, beta = 10000, n_draw = 10000, obs = 218, n = 479860)
#control_group = bayesian_data_simulation(alpha = 1, beta = 10000, n_draw = 10000, obs = 345, n = 949647)