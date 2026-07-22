# Pre-registered decision test for the sub-ceiling follow-up (OSF r8m2q).
# Criterion is applied EXACTLY as registered; nothing is chosen post hoc.
d <- read.csv("results/subceiling.csv")
emp_logit <- function(power, R) { k <- round(power * R); log((k + 0.5) / (R - k + 0.5)) }
P <- "power_all"   # primary power measure, as registered

cat(sprintf("cells=%d  R=%d  cv={%s}\n\n", nrow(d), unique(d$R_total),
            paste(sort(unique(d$cv)), collapse=",")))

## -- Criterion 1: reliability (empirical-logit regression, CV linear) --------
d$lp <- emp_logit(d[[P]], d$R_total)
m  <- lm(lp ~ cv + N + lambda_bar + D + beta1, data = d)
ci <- confint(m)["cv", ]
b  <- coef(m)["cv"]
c1 <- (b < 0) && (ci[2] < 0)
cat("CRITERION 1 (regression):\n")
cat(sprintf("  CV coefficient = %.3f   95%% CI [%.3f, %.3f]\n", b, ci[1], ci[2]))
cat(sprintf("  negative AND CI upper < 0 ?  %s\n\n", if (c1) "YES" else "NO"))

## -- Criterion 2: magnitude (matched pairs, informative band) ----------------
key <- with(d, paste(N, lambda_bar, D, beta1, sep="_"))
lo  <- d[d$cv==0.3, ]; hi <- d[d$cv==0.9, ]
lo <- lo[order(with(lo,paste(N,lambda_bar,D,beta1))), ]
hi <- hi[order(with(hi,paste(N,lambda_bar,D,beta1))), ]
stopifnot(nrow(lo)==nrow(hi),
          all(with(lo,paste(N,lambda_bar,D,beta1))==with(hi,paste(N,lambda_bar,D,beta1))))
gap  <- hi[[P]] - lo[[P]]                 # power(cv=.9) - power(cv=.3)
band <- lo[[P]] >= 0.2 & lo[[P]] <= 0.8   # reference (cv=.3) power in [0.2,0.8]
med_band <- median(gap[band])
c2 <- med_band <= -0.03
cat("CRITERION 2 (matched-pair magnitude, informative band):\n")
cat(sprintf("  matched pairs total = %d ;  in band [0.2,0.8] = %d\n", length(gap), sum(band)))
cat(sprintf("  median gap (all pairs)   = %+.3f\n", median(gap)))
cat(sprintf("  median gap (in band)     = %+.3f   (<= -0.03 ?  %s)\n\n",
            med_band, if (c2) "YES" else "NO"))

## -- VERDICT -----------------------------------------------------------------
cat("=======================================================\n")
cat(sprintf("  HYPOTHESIS %s\n", if (c1 && c2) "CONFIRMED (both criteria met)"
                                 else "NOT CONFIRMED"))
cat("=======================================================\n\n")

## -- Secondary: dose-response across cv {0.3,0.6,0.9} ------------------------
mid <- d[d$cv==0.6, ]; mid <- mid[order(with(mid,paste(N,lambda_bar,D,beta1))), ]
mono <- (lo[[P]] >= mid[[P]]) & (mid[[P]] >= hi[[P]])     # non-increasing in CV
cat(sprintf("SECONDARY dose-response: power non-increasing across cv in %d/%d design cells (%.0f%%)\n",
            sum(mono), length(mono), 100*mean(mono)))
cat(sprintf("  (parent exploratory estimate at beta1=0.1 was ~-4.2 pts median in the sub-ceiling regime)\n"))

## -- context: how many cells at ceiling/floor -------------------------------
cat(sprintf("\nregime spread: power_all in [.2,.8] for %d/%d cells; <.2 for %d; >.8 for %d\n",
            sum(d[[P]]>=.2 & d[[P]]<=.8), nrow(d), sum(d[[P]]<.2), sum(d[[P]]>.8)))
