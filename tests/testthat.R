# Ejecuta todas las pruebas.   Desde la carpeta del proyecto:  Rscript tests/testthat.R
library(testthat)
resultado <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(resultado)
cat(sprintf("\nPruebas: %d correctas, %d con fallos, %d con error, %d omitidas\n",
            sum(df$passed > 0 & df$failed == 0 & !df$error), sum(df$failed > 0), sum(df$error), sum(df$skipped)))
if (!interactive() && (any(df$failed > 0) || any(df$error))) quit(status = 1)
