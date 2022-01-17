## Header ------------------------------------------------------------------
source("header_sec_4.2.R")


## Data processing ---------------------------------------------------------
data_thal <- read.table(file = "./data/data_Thaliana.txt",
                        header = T, sep = " ")
data_thal_num <- data_thal %>% 
  select(starts_with("c")) %>% 
  mutate_all(.funs = log) %>% 
  as.data.frame() %>% 
  t() %>% 
  scale() %>% 
  global_para$transform_funs[[1]]()

## (1) Scatter matrix estimation -----------------------------------------------
est <- list()
est$Kendall <- getKendall(X = data_thal_num)$S
est$Tyler <- cov2cor(getGenTyler(X = data_thal_num)$S)
est$Pearson <- cor(data_thal_num)



### Original scatter matrix estimators
gg_fig <- list()
gg_fig$est_Kendall <-
  ggplot(data = reshape2::melt(est$Kendall)) +
  geom_tile(aes(x = Var2, y = Var1, fill = value),
            color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1)) +
  labs(x = "", y = "") +
  user_theme +
  theme(legend.key.size = unit(3, "cm"), legend.key.height = unit(0.5,"cm")) +
  scale_y_reverse() + 
  ggtitle("Kendall's tau (orig)") + 
  geom_rect(aes(xmin = 22 - 0.5, xmax = 40 - 0.5, 
                ymin = 22 - 0.5, ymax = 40 - 0.5),
            fill = "transparent", color = "black", size = 1.5)

gg_fig$est_Tyler <- 
  ggplot(data = reshape2::melt(est$Tyler)) +
  geom_tile(aes(x = Var2, y = Var1, fill = value),
            color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1)) +
  labs(x = "", y = "") +
  user_theme +
  theme(legend.key.size = unit(3, "cm"), legend.key.height = unit(0.5,"cm")) +
  scale_y_reverse() + 
  ggtitle("Tyler's M (orig)") + 
  geom_rect(aes(xmin = 22 - 0.5, xmax = 40 - 0.5, 
                ymin = 22 - 0.5, ymax = 40 - 0.5),
            fill = "transparent", color = "black", size = 1.5)

gg_fig$est_Pearson <- 
  ggplot(data = reshape2::melt(est$Pearson)) +
  geom_tile(aes(x = Var2, y = Var1, fill = value),
            color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1)) +
  labs(x = "", y = "") +
  user_theme +
  theme(legend.key.size = unit(3, "cm"), legend.key.height = unit(0.5,"cm")) +
  scale_y_reverse() + 
  ggtitle("Pearson's R (orig)") + 
  geom_rect(aes(xmin = 22 - 0.5, xmax = 40 - 0.5, 
                ymin = 22 - 0.5, ymax = 40 - 0.5),
            fill = "transparent", color = "black", size = 1.5)



## (2) Approximation based PCA -------------------------------------------------
ed <- lapply(est, eigen)
est_approx <- list()
n_pc <- 3
est_approx$Kendall <- ed$Kendall$vectors[,1:n_pc] %*% diag(ed$Kendall$values[1:n_pc]) %*% t(ed$Kendall$vectors[,1:n_pc])
est_approx$Tyler   <- ed$Tyler$vectors[,1:n_pc]   %*% diag(ed$Tyler$values[1:n_pc])   %*% t(ed$Tyler$vectors[,1:n_pc])
est_approx$Pearson <- ed$Pearson$vectors[,1:n_pc] %*% diag(ed$Pearson$values[1:n_pc]) %*% t(ed$Pearson$vectors[,1:n_pc])


### Approximate scatter matrix estimators
gg_fig$est_approx_Kendall <- 
  ggplot(data = reshape2::melt(est_approx$Kendall)) +
  geom_tile(aes(x = Var2, y = Var1, fill = value),
            color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1)) +
  labs(x = "", y = "") +
  user_theme +
  theme(legend.key.size = unit(3, "cm"), legend.key.height = unit(0.5,"cm")) +
  scale_y_reverse() + 
  ggtitle("Kendall's tau (approx)")

gg_fig$est_approx_Tyler <- 
  ggplot(data = reshape2::melt(est_approx$Tyler)) +
  geom_tile(aes(x = Var2, y = Var1, fill = value),
            color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1)) +
  labs(x = "", y = "") +
  user_theme +
  theme(legend.key.size = unit(3, "cm"), legend.key.height = unit(0.5,"cm")) +
  scale_y_reverse() + 
  ggtitle("Tyler's M (approx)")

gg_fig$est_approx_Pearson <- 
  ggplot(data = reshape2::melt(est_approx$Pearson)) +
  geom_tile(aes(x = Var2, y = Var1, fill = value),
            color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1)) +
  labs(x = "", y = "") +
  user_theme +
  theme(legend.key.size = unit(3, "cm"), legend.key.height = unit(0.5,"cm")) +
  scale_y_reverse() + 
  ggtitle("Pearson's R (approx)")



## (3) Compute mutual angles between eigenvectors ------------------------------
angle <- list()
angle$K_vs_P <- acos(colSums(ed$Kendall$vectors * ed$Pearson$vectors))  / pi * 180
angle$K_vs_T <- acos(colSums(ed$Kendall$vectors * ed$Tyler$vectors))  / pi * 180
angle$T_vs_P <- acos(colSums(ed$Tyler$vectors * ed$Pearson$vectors))  / pi * 180
angle <- lapply(angle, function(x) pmin(x, 180 - x))


### Eigenvalues
gg_fig$compare_eval <- 
  ggplot(data = data.frame(Index = 1:length(ed$Kendall$values),
                           Kendall = ed$Kendall$values,
                           Tyler = ed$Tyler$values,
                           Pearson = ed$Pearson$values) %>% 
           pivot_longer(cols = -c("Index"), names_to = "Estimator", 
                        values_to = "Eigenvalue"),
         mapping = aes(x = Index, y = Eigenvalue,
                       color = Estimator)) +
  geom_point() + 
  geom_line() +
  user_theme

### Mutual angles
gg_fig$compare_angle <- 
  ggplot(data =  data.frame(Index = 1:length(angle$K_vs_P),
                            K_vs_P = angle$K_vs_P,
                            K_vs_T = angle$K_vs_T,
                            T_vs_P = angle$T_vs_P) %>% 
           pivot_longer(cols = -c("Index"), names_to = "Comparison", 
                        values_to = "Angle"),
         mapping = aes(x = Index, y = Angle, color = Comparison)) + 
  geom_point() + 
  geom_line() + 
  user_theme



### Save the plot in pdf (Figure 4)
pdf(CheckFile(filename = "compare_eigen",
              fileext = "pdf",
              filepath = "./result/"),
    onefile = F,
    width = list_pdf_option$width * 1.5,
    height = list_pdf_option$height * 2)
print(
  grid.arrange(
    lemon::grid_arrange_shared_legend(
      gg_fig$est_Kendall, 
      gg_fig$est_Tyler,
      gg_fig$est_Pearson,
      nrow = 1, 
      position = "top", plot = F
    ),
    lemon::grid_arrange_shared_legend(
      gg_fig$est_approx_Kendall, 
      gg_fig$est_approx_Tyler,
      gg_fig$est_approx_Pearson,
      nrow = 1, 
      position = "top", plot = F
    ),
    arrangeGrob(gg_fig$compare_eval,
                gg_fig$compare_angle, nrow = 1, ncol = 2))
)
dev.off()



## Outlier detection  ------------------------------------------------------

### Compute PC scores (up to 4-dimensional)
n_pc <- 4
PC_score <- list(
  Kendall = data_thal_num %*% ed$Kendall$vectors[,1:n_pc],
  Tyler = data_thal_num %*% ed$Tyler$vectors[,1:n_pc],
  Pearson = data_thal_num %*% ed$Pearson$vectors[,1:n_pc]
)

gg_score <- rbind(
  data.frame(Estimator = "Kendall", 
             PC = PC_score$Kendall) %>% 
    mutate(id = row_number()) %>% pivot_longer(cols = -c("id", "Estimator", "PC.1")),
  data.frame(Estimator = "Tyler", 
             PC = PC_score$Tyler) %>% 
    mutate(id = row_number()) %>% pivot_longer(cols = -c("id", "Estimator", "PC.1")),
  data.frame(Estimator = "Pearson", 
             PC = PC_score$Pearson) %>% 
    mutate(id = row_number()) %>% pivot_longer(cols = -c("id", "Estimator", "PC.1"))
) %>% 
  mutate(Estimator = factor(Estimator,
                            levels = c("Kendall", "Tyler", "Pearson")))


### Save the plot in pdf (Figure 5)
pdf(CheckFile(filename = "PC_score",
              fileext = "pdf",
              filepath = "./result/"),
    onefile = T,
    width = list_pdf_option$width * 0.8,
    height = list_pdf_option$height * 0.8)
ggplot(data = gg_score,
       mapping = aes(x = PC.1, y = value)) +
  geom_point(size = 0.1) + 
  geom_text(aes(label = ifelse(id %in% c(3, 13, 113), id, "")),
            hjust = 1.2,
            vjust = -0.3,
            color = 2, 
            size = 3) +
  facet_grid(name ~ Estimator) + 
  user_theme
dev.off()

