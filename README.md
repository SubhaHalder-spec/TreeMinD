# TreeMinD
Testing against tree-ordered alternatives in one-way ANOVA Code is for testing equality means against tree-ordered alternatives in one-way ANOVA. We have considered testing of H_0:mu_0 = mu_1 = ... = mu_k vs H_1:mu_0 <= mu_i (at least one strict inequality), for all i >= 1, where mu_0 and mu_i represents the population means of the control and i-th treatment. The input consists of two variables: list of sample_data and significance_level. The output consists of the critical value, the Max-D test statistic value, and the result, which indicates whether to reject or not reject the null hypothesis. If the test statistic value is greater than the critical value, we reject the null hypothesis; otherwise, we do not reject it.
