# Implementing a Chi Square test in R
# A study of the frequency of black and red two-spotted ladybugs 
# (Adalia bipunctata) in industrial and rural habitats produced the followingdata:

# Ho = no association between habitat & color
# Ha = an association exists between habitat & color

chi.data = matrix(c(115,30,85,70), 2, 2, byrow=TRUE,
                  dimnames=list(c("Black","Red"), 
                                c("Industrial","Rural")))
chi.data

# To visually inspect the data, a bar chart can be generated as follows:
barplot(chi.data, beside=T, col=c("Black","Red"), 
            ylim=c(0,125), legend=T)

# are the observed differences large enough to conclude that there is 
# an association between habitat and color morph?

# To answer this question, we’ll need to run a chi square test as follows:
chisq.test(chi.data)

# The results indicate a p-value of 1.239e-05, which is much less than the 
# typical significance level of 0.05.
# We would reject the null hypothesis and conclude that for two-spotted 
# ladybugs, there is an association between habitat and color morph.