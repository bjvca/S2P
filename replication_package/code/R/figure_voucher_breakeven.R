## figure_voucher_breakeven.R
## ---------------------------------------------------------------------------
## Voucher-cost vs maize-price-contingent extra return.
## x: maize price (MWK/kg), 500-1500
## y: T2 production-value gain = 150.83 kg per HH (Table 5 Panel A) * price
## Horizontal: voucher face value 170,000 MWK
## ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(ggplot2) })

yield_effect_kg <- 150.83
yield_se        <- 72.35
voucher_cost    <- 170000
median_price    <- 750

p <- seq(500, 1500, length.out = 201)
df <- data.frame(
  price = p,
  ret   = yield_effect_kg * p,
  lo    = (yield_effect_kg - 1.96 * yield_se) * p,
  hi    = (yield_effect_kg + 1.96 * yield_se) * p
)
break_even <- voucher_cost / yield_effect_kg

g <- ggplot(df, aes(x = price, y = ret)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = voucher_cost, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = break_even, linetype = "dotted", colour = "darkred") +
  annotate("text", x = 530, y = voucher_cost + 8000,
           label = "Voucher cost = 170,000 MWK", hjust = 0, colour = "red", size = 3.4) +
  annotate("text", x = break_even + 25, y = 20000,
           label = sprintf("break-even\n%.0f MWK/kg", break_even),
           hjust = 0, colour = "darkred", size = 3.4) +
  annotate("point", x = median_price,
           y = yield_effect_kg * median_price, colour = "black", size = 2) +
  annotate("text", x = median_price + 25,
           y = yield_effect_kg * median_price - 8000,
           label = "sample median (750)", hjust = 0, size = 3.2) +
  scale_x_continuous(breaks = seq(500, 1500, by = 100)) +
  scale_y_continuous(breaks = seq(0, 250000, by = 50000),
                     labels = function(x) format(x, big.mark = ",")) +
  labs(x = "Maize price (MWK / kg)",
       y = "Voucher-induced production value (MWK / household)") +
  theme_classic(base_size = 11)

out_pdf <- "/home/claude/workspace/S2P/paper/figures/voucher_breakeven.pdf"
out_png <- "/home/claude/workspace/S2P/paper/figures/voucher_breakeven.png"
ggsave(out_pdf, g, width = 6.5, height = 4.2)
ggsave(out_png, g, width = 6.5, height = 4.2, dpi = 200)
cat("Saved:", out_pdf, "and", out_png, "\n")
cat("Break-even price:", round(break_even, 1), "MWK/kg\n")
