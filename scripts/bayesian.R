# Optional: Rscript scripts/bayesian.R (requires brms and a working Stan compiler).
if (!requireNamespace("brms", quietly = TRUE)) stop("Install brms and its Stan toolchain first.")
d <- read.csv("Data/processed/seasonal-fires.csv")
dir.create("outputs/bayesian", recursive = TRUE, showWarnings = FALSE)
set.seed(20261014)
for (cause in c("All", "Human", "Natural")) {
  data <- subset(d, Cause == cause & eligible)
  for (predictor in c(FALSE, TRUE)) {
    formula <- if (predictor) nFires ~ DrynessCm else nFires ~ 1
    priors <- c(brms::set_prior("normal(2, 2)", class = "Intercept"),
                brms::set_prior("exponential(1)", class = "shape"))
    if (predictor) priors <- c(priors, brms::set_prior("normal(0, 0.5)", class = "b"))
    fit <- brms::brm(formula, data = data, family = brms::negbinomial(), prior = priors,
                     chains = 4, cores = 4, iter = 4000, warmup = 2000,
                     seed = 20261014, control = list(adapt_delta = .95))
    key <- paste(cause, if (predictor) "drought" else "baseline", sep = "-")
    saveRDS(fit, paste0("outputs/bayesian/", key, ".rds"))
    capture.output(summary(fit), file = paste0("outputs/bayesian/", key, "-summary.txt"))
    saveRDS(brms::posterior_predict(fit), paste0("outputs/bayesian/", key, "-predictive.rds"))
  }
}
