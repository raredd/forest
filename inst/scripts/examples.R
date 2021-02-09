library('forest')
library('cmprsk2')

# simulated data to test 
set.seed(1)
n <- 500L
dd <- data.frame(
  ftime   = rexp(n),
  fstatus = sample(0:2, n, replace = TRUE),
  
  x1 = runif(n),
  x2 = runif(n),
  x3 = runif(n),
  
  factor1 = factor(sample(1:3, n, TRUE)),
  factor2 = factor(sample(1:2, n, TRUE))
)

dd[] <- lapply(dd, function(x) {
  if (!is.factor(x))
    x[sample(length(x), sample(1:10))] <- NA
  x
})

x <- with(dd, crr(ftime, fstatus,
                  cbind(x1, x2, x3, model.matrix(~factor1 + factor2)[, -1])
))
y <- crr2(Surv(ftime, fstatus(0) == 1) ~ ., dd)

clean <- cleanfp(x, formula = ftime + fstatus ~ ., dd)
clean_ref <- add_reference(clean)
prep_list <- prepare_forest(clean_ref)
plot(prep_list, show_conf = TRUE)


x <- crr2(Surv(futime, event(censored) == death) ~ age + sex + abo, transplant)

fp <- cleanfp(x$`CRR: death`, futime ~ age + sex + abo, transplant)
fp <- cleanfp(x)
fp <- add_reference(fp)
fp <- prepare_forest(fp)
plot(fp, show_conf = TRUE)

forest(x)
