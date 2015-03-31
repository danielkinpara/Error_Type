babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]          # weight of babies from non-smoker mothers
bwt.smoke = babies$bwt[babies$smoke==1]             # weight of babies from smoker mothers

N <- 15                                             # sample size
B <- 1000                                           # number of iterations in the simulation
alphas <- c(0.1, 0.05, 0.01)                        # different alphas: 10%, 5%, 1%

power <- sapply(alphas, function(a){                # run different alphas each time
        rejections <- sapply(1:B, function(i){      # sample 1000 times and perform t-test
                bns <- sample(bwt.nonsmoke, N)
                bs <- sample(bwt.smoke, N)
                t.test(bns, bs)$p.value < a
                })
        return(mean(rejections))                    # calculate the average of TRUE (calculated p-value < alpha)
        })
plot(alphas, power, xaxt = "n")
axis(1, at = alphas, labels = alphas)

# The "power" variable shows the power to reject null hypothesis when in fact there're differences in the
# groups' mean, given an alpha. Alpha reflects the desire of not happen Type II Error (false negative, that is,
# to not reject NULL hypothesis when actually there's a difference in the groups' mean).
# In this simulation, it shows that smaller the alpha chosen (that is, less desire to happen type II Error),
# imples in less power, given the same size of the sample.
