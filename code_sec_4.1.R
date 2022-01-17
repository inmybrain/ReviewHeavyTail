## Header ------------------------------------------------------------------
source("header_sec_4.1.R")

## Data generation ---------------------------------------------------------
set.seed(6)
res_comp <- CompHeavyTailData(n = global_para$n,
                  p = global_para$p,
                  Sigma = global_para$Sigma)



data <- list()

### MVN
data$MVN <- sweep(res_comp$U %*% res_comp$Sigma_sqrt, 1, res_comp$R, "*")

### SGM
lambda <- rexp(global_para$n, rate = 1)
data$SGM <- sweep(res_comp$U %*% res_comp$Sigma_sqrt, 1, res_comp$R * lambda, "*")

### EL
data$EL <- sweep(res_comp$U %*% res_comp$Sigma_sqrt, 1, res_comp$R^2, "*")

### GEL
data$GEL <- sweep(res_comp$U %*% res_comp$Sigma_sqrt, 1, res_comp$R^2 * rowSums(res_comp$U), "*")

### NPN
data$NPN <- data$MVN # temporary
data$NPN[,1] <- global_para$transform_funs[[1]](data$MVN[,1])
data$NPN[,2] <- global_para$transform_funs[[2]](data$MVN[,2])

### TEL
data$TEL <- data$MVN # temporary
data$TEL[,1] <- global_para$transform_funs[[1]](data$EL[,1])
data$TEL[,2] <- global_para$transform_funs[[2]](data$EL[,2])




## Scatterplot -------------------------------------------------------------
gg_fig <- list()
### MVN
gg_fig[[1]] <- 
  ggplot(data = data$MVN %>% as.data.frame,
         mapping = aes(x =  V1, y = V2)) +  
  geom_point(size = 0.1, alpha = 0.3) + 
  stat_density_2d(linetype = 1,
                  n = global_para$density_n) +
  user_theme +
  labs(title = "MVN", x = "x", y = "y") 


### SGM
gg_fig[[2]] <- 
  ggplot(data = data$SGM %>% as.data.frame,
         mapping = aes(x =  V1, y = V2)) +  
  geom_point(size = 0.1, alpha = 0.3) + 
  stat_density_2d(linetype = 1,
                  n = global_para$density_n) +
  user_theme +
  labs(title = "SGM", x = "x", y = "y")
### EL
gg_fig[[3]] <- 
  ggplot(data = data$EL %>% as.data.frame,
         mapping = aes(x =  V1, y = V2)) +  
  geom_point(size = 0.1, alpha = 0.3) + 
  stat_density_2d(linetype = 1,
                  n = global_para$density_n) +
  user_theme +
  labs(title = "EL", x = "x", y = "y")

### GEL
gg_fig[[4]] <- 
  ggplot(data = data$GEL %>% as.data.frame,
         mapping = aes(x =  V1, y = V2)) +  
  geom_point(size = 0.1, alpha = 0.3) + 
  stat_density_2d(linetype = 1,
                  n = global_para$density_n) +
  # geom_density_2d(alpha = 0.7, color = "blue", linetype = 2) +
  user_theme +
  labs(title = "GEL", x = "x", y = "y")

### NPN
gg_fig[[5]] <- 
  ggplot(data = data$NPN %>% as.data.frame,
         mapping = aes(x =  V1, y = V2)) +  
  geom_point(size = 0.1, alpha = 0.3) + 
  stat_density_2d(linetype = 1,
                  n = global_para$density_n) +
  # geom_density_2d(alpha = 0.7, color = "blue", linetype = 2) +
  user_theme +
  labs(title = "NPN", x = "x", y = "y")

### TEL
gg_fig[[6]] <- 
  ggplot(data = data$TEL %>% as.data.frame,
         mapping = aes(x =  V1, y = V2)) +  
  geom_point(size = 0.1, alpha = 0.3) + 
  stat_density_2d(linetype = 1,
                  n = global_para$density_n) +
  # geom_density_2d(alpha = 0.7, color = "blue", linetype = 2) +
  user_theme +
  labs(title = "TEL", x = "x", y = "y")


### Save the plot in pdf (Figure 3)
pdf(CheckFile(filename = "density",
              fileext = "pdf",
              filepath = "./result/"),
    onefile = F,
    width = list_pdf_option$width * 1,
    height = list_pdf_option$height * 1)

grid.arrange(gg_fig[[1]], 
             gg_fig[[2]],
             gg_fig[[5]],
             gg_fig[[3]],
             gg_fig[[4]],
             gg_fig[[6]],
             nrow = 2)
dev.off()
