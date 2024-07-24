library(readxl)
library(ggplot2)
library(reshape2)

Dataset <- read_excel("Questionario_tratado.xlsx")

y <- Dataset$V24
y <- ifelse(y < 18,1,0)
y <- na.omit(y)
y <- factor(y)
  
x <- data.frame(
  idade = Dataset$`V01`,
  raca = Dataset$`V02`,
  sexo = Dataset$`V03`,
  religiao = Dataset$`V04`,
  zona_residencia = Dataset$`V05`,
  ocupacao = Dataset$`V06`,
  conhecimento_infracoes = Dataset$`V08`,
  concordancia_reabilitacao = Dataset$`V10`,
  vitima_infracao = Dataset$`V12`,
  conhece_infrator = Dataset$`V13`,
  conhecimento_ECA = Dataset$`V25`,
  satizfacao_ECA = Dataset$`V26`,
  stringsAsFactors = TRUE
)
x <- na.omit(x)

table(y,x$raca)
table(y,x$religiao)
table(y, x$zona_residencia)
table(y, x$conhecimento_ECA)

x$religiao_2 <- ifelse(x$religiao == 'N?o religioso',1,0)
x$raca_pardo <- ifelse(x$raca == "Pardo",1,0)
x$raca_branco <- ifelse(x$raca == "Branco",1,0)
x$raca2<- factor(ifelse(x$raca == "Pardo",1,ifelse(x$raca == "Branco",2,0)))
x$zona_sul <- ifelse(x$zona_residencia == "Zona Sul (Capital - RJ)",1,0)
x$aprofundado <- ifelse(x$conhecimento_ECA == "Possuo conhecimento aprofundado", 1,0)

table(y,x$raca)

plot_bar <- function(df) {
  for (col in names(df)) {
    if (is.factor(df[[col]]) || is.character(df[[col]])) {
      print(
        ggplot(df, aes_string(x = col)) +
          geom_bar() +
          labs(title = paste("Bar Plot of", col)) +
          theme_minimal()
      )
    }
  }
}

###
plot_heatmap <- function(df) {
  cat_vars <- names(df)[sapply(df, is.factor)]
  for (i in 1:(length(cat_vars) - 1)) {
    for (j in (i + 1):length(cat_vars)) {
      cat_var1 <- cat_vars[i]
      cat_var2 <- cat_vars[j]
      contingency_table <- table(df[[cat_var1]], df[[cat_var2]])
      if (min(dim(contingency_table)) > 1) {  # Certifique-se de que a tabela tenha mais de uma categoria
        melted_data <- melt(contingency_table)
        colnames(melted_data) <- c(cat_var1, cat_var2, "Freq")
        print(
          ggplot(melted_data, aes_string(x = cat_var1, y = cat_var2, fill = "Freq")) +
            geom_tile() +
            scale_fill_gradient(low = "white", high = "blue") +
            labs(title = paste("Heatmap of", cat_var1, "and", cat_var2)) +
            theme_minimal()
        )
      }
    }
  }
}
####

plot_heatmap(x)
plot_bar(x)
summary(x)

modelo <- glm(y ~ idade + raca_pardo + sexo + religiao_2 + zona_residencia + ocupacao +
                conhecimento_infracoes + concordancia_reabilitacao + vitima_infracao + conhece_infrator
              + conhecimento_ECA + satizfacao_ECA, data = x, family = binomial)
summary(modelo)

modelo <- glm(y ~ idade + raca_pardo + religiao_2 + zona_residencia + ocupacao +
                conhecimento_infracoes + concordancia_reabilitacao + vitima_infracao + conhece_infrator
              + conhecimento_ECA + satizfacao_ECA, data = x, family = binomial) #Removi Sexo
summary(modelo)

modelo <- glm(y ~ idade + religiao_2 + ocupacao + raca_pardo+
                conhecimento_infracoes + concordancia_reabilitacao + vitima_infracao + conhece_infrator
              + conhecimento_ECA + satizfacao_ECA, data = x, family = binomial) #Removi zona_resid?ncia
summary(modelo)

modelo <- glm(y ~ idade + religiao_2 + ocupacao + raca_pardo+
                conhecimento_infracoes + concordancia_reabilitacao + vitima_infracao + conhece_infrator
              + satizfacao_ECA, data = x, family = binomial) #Removi conhecimento ECA
summary(modelo)

modelo <- glm(y ~ idade + religiao_2 + raca_pardo +
                conhecimento_infracoes + concordancia_reabilitacao + vitima_infracao + conhece_infrator
              + satizfacao_ECA, data = x, family = binomial) #Removi Ocupacao
summary(modelo)

modelo <- glm(y ~ idade + religiao_2 + raca_pardo +
                conhecimento_infracoes + concordancia_reabilitacao + conhece_infrator
              + satizfacao_ECA, data = x, family = binomial) #Removi Vitima de infra??o
summary(modelo)

modelo <- glm(y ~ idade + religiao_2 + raca_pardo +
                conhecimento_infracoes + concordancia_reabilitacao + conhece_infrator
              , data = x, family = binomial) #Removi Satizafa??o ECA
summary(modelo)

modelo <- glm(y ~ idade + religiao_2 + raca_pardo +
                conhecimento_infracoes + concordancia_reabilitacao
              , data = x, family = binomial) #Removi Conhece infrator
summary(modelo)
anova(modelo)
plot(modelo)




set.seed(123)  # Para reprodutibilidade
train_indices <- sample(seq_len(nrow(x)), size = 0.7 * nrow(x))
train_data <- x[train_indices, ]
test_data <- x[-train_indices, ]
train_y <- y[train_indices]
test_y <- y[-train_indices]

model <- glm(y ~ idade + religiao_2 + raca_pardo + conhecimento_infracoes + 
               concordancia_reabilitacao, family = binomial, data = x)
summary(model)

# Obter predi??es no conjunto de teste
predictions <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Matriz de confus?o
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_y)
print(confusion_matrix)

# M?tricas de avalia??o
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
