library(R2ucare)

# # read in text file as described at pages 50-51 in http://www.phidot.org/software/mark/docs/book/pdf/app_3.pdf
#dipper <- system.file("extdata", "dipper.txt", package = "RMark")


CH = read.table("data/multimark_input.txt", header = T)
#CH = read.table("data/multimark_input_PlettenbergBay.txt", header = T)
CH = data.matrix(CH)
head(CH)
colnames(CH) <- NULL
head(CH)

freq <- rep(1, nrow(CH))

test3sr_ <- test3sr(CH, freq)
test3sm_ <- test3sm(CH, freq)
test2ct_ <- test2ct(CH, freq)
test2cl_ <- test2cl(CH, freq)

# display results:
test3sr_
test2ct_

overall_CJS(CH, freq)


# Count number of transients
row_totals <- rowSums(CH)
# number of individuals seen x times
as.data.frame(table(row_totals))

sum(row_totals == 1)  # number seen once

sum(row_totals > 1)  # number seen more than once

sum(row_totals == 1) / sum(row_totals >=1)  # proportion seen once

hist(row_totals,
    breaks = seq(min(row_totals) - 0.5,
                 max(row_totals) + 0.5,
                 by = 1),
    xlab = "Row total",
    main = "Distribution of row totals")

# plot this as a histogram

library(ggplot2)

df <- data.frame(row_totals = row_totals)

ggplot(df, aes(x = row_totals)) +
  geom_histogram(binwidth = 1,
                 boundary = -0.5) +
  scale_x_continuous(breaks = seq(min(row_totals),
                                  max(row_totals),
                                  by = 1)) +
  labs(x = "Number of detections",
       y = "Number of individuals") +
  theme_minimal()

