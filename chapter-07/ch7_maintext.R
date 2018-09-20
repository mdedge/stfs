#############################
#Chapter 7, main text code
#Statistical Thinking from Scratch

#p values for the hypothetical hypothesis test
pnorm(63, mean = 64, sd = 4/5)
pnorm(63, mean = 64, sd = 4/5) + (1 - pnorm(65, mean = 64, sd = 4/5))
pnorm(65.5, mean = 64, sd = 4/5)
1 - pnorm(65.5, mean = 64, sd = 4/5)

