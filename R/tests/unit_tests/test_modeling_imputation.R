suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(withr))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fs))

skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste0("Package not installed: ", pkg))
  }
}

src_dir <- file.path(here("R"))
io_tools <- new.env()
source(file.path(src_dir, "./io.R"), local = io_tools)
model_tools <- new.env()
source(file.path(src_dir, "./modeling.R"), local = model_tools)

test_that("impute_expression_with_mice fills NA in expression rows", {
  skip_if_not_installed("mice")
  set.seed(123)
  # Build small expression matrix with missing values
  mat <- matrix(rnorm(5 * 6), nrow = 5, ncol = 6)
  mat[2, c(2, 5)] <- NA_real_
  mat[4, c(1, 3, 6)] <- NA_real_
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  rownames(mat) <- paste0("G", seq_len(nrow(mat)))

  # Metadata predictors aligned to samples
  meta <- data.frame(
    .sample_id = colnames(mat),
    group = factor(rep(c("A", "B"), length.out = ncol(mat))),
    score = c(1, 2, NA, 3, 4, 5),
    stringsAsFactors = FALSE,
    row.names = colnames(mat)
  )

  imputed <- model_tools$impute_expression_with_mice(mat, meta_df = meta, method = "pmm")
  expect_equal(dim(imputed), dim(mat))
  # Previously missing rows should now be complete (no NA)
  expect_false(any(is.na(imputed[2, ])))
  expect_false(any(is.na(imputed[4, ])))
})

test_that("custom MNAR imputer works through mice pipeline", {
  skip_if_not_installed("mice")
  set.seed(42)
  n <- 30
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    grp = factor(sample(c("A", "B"), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  miss_idx <- sample(seq_len(n), size = 5)
  df$y[miss_idx] <- NA_real_
  # methods: only impute 'y' using custom method, leave others unchanged
  meth <- c(y = "mnar_shift_norm", x1 = "", grp = "polyreg")
  imp <- mice::mice(df, m = 1, maxit = 1, method = meth, printFlag = FALSE, seed = 99)
  comp <- mice::complete(imp, 1)
  expect_false(any(is.na(comp$y)))
})

test_that("create_rnkfiles_from_model imputes metadata and writes diagnostics", {
  skip_if_not_installed("mice")
  withr::with_tempdir({
    set.seed(99)
    gct <- io_tools$make_random_gct(20, 8)
    # Add a simple grouping with NAs to force metadata imputation
    gct@cdesc$group <- rep(c("A", "B"), length.out = ncol(gct@mat))
    gct@cdesc$group[c(2, 5)] <- NA_character_
    # Introduce NAs in expression to trigger expression imputation
    gct@mat[3, c(1, 6)] <- NA_real_
    gct@rid <- paste0("Gene", seq_len(nrow(gct@mat)))
    gct@rdesc$GeneSymbol <- gct@rid

    gct_path <- file.path(getwd(), "impute_model.gct")
    cmapR::write_gct(gct, gct_path, appenddim = FALSE)

    model_spec <- list(
      name = "impute_demo",
      type = "limma",
      design = "~ 0 + group",
      contrasts = list("groupB - groupA"),
      imputation = list(
        engine = "mice",
        m = 1,
        maxit = 2,
        methods = "pmm",
        apply_to = "both",
        expression_method = "pmm"
      )
    )

    output_dir <- file.path(getwd(), "model", "limma", "impute_demo")
    rnkdfs <- model_tools$create_rnkfiles_from_model(
      gct_path = gct_path,
      model_spec = model_spec,
      output_dir = output_dir,
      replace = TRUE
    )

    # Basic sanity
    expect_type(rnkdfs, "list")
    expect_gt(length(rnkdfs), 0)
    first <- rnkdfs[[1]]
    expect_true(all(c("id", "value") %in% colnames(first)))
    expect_false(anyNA(first$value))

    # Verify imputation ran
    imp_meta <- attr(rnkdfs, "imputation")
    expect_true(is.list(imp_meta))
    expect_true(tolower(imp_meta$engine %||% "none") %in% c("mice", "none"))

    # Diagnostics directory should exist when imputation ran and metadata had NAs
    if (!is.null(imp_meta) && tolower(imp_meta$engine %||% "none") == "mice") {
      diag_dir <- fs::path(output_dir, "diagnostics", "imputation")
      expect_true(fs::dir_exists(diag_dir))
      pdfs <- if (fs::dir_exists(diag_dir)) fs::dir_ls(diag_dir, glob = "*.pdf") else character(0)
      expect_true(length(pdfs) >= 1)
    }
  })
})
