summaryR <- function(model, type = c("hc3", "hc0", "hc1", "hc2", "hc4"),
                     ...){
                    if(!require(car)) stop("Require car package is missing")
                    type <- match.arg(type)
                    V <- hccm(model, type = type) # kovarianèí matice
                    shrnuti <- summary(model) # shrnutí modelu
                    table <- coef(shrnuti) # vypsání koeficientù do tabulky
                    table[,2] <- sqrt(diag(V)) # Standart errors 2. sloupec
                    table[,3] <- table[,1]/table[,2] # t-testy = coef/S.E.
                    table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail = FALSE) # p-hodnoty
                    shrnuti$coefficients <- table
                    p <-nrow(table)
                    hyp <- cbind(0, diag(p-1))
                    shrnuti$fstatistic[1] <- linearHypothesis(model, hyp,
                                            white.adjust=type)[2,"F"]
                    print(shrnuti)
                    robustS <- vcovHC(model, type="HC0") # White S.E.
                   # print(robustS)
                    
                  cat("Note: Heteroskedasticity-consistent s.e. using adjustment", type, "\n")
                  print(waldtest(model, lm(AVGEXP ~ AGE + OWNRENT , data=new.table), vcov=robustS, test = c( "Chisq")))
                    
}
install.packages("car")
library(car)
