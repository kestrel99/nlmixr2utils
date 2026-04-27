# Canonical raw-results helpers shared across nlmixr2 extension packages.

.rawResultsSchemaVersion <- 1L

.rawResultsBaseCols <- c(
  "source",
  "hypothesis",
  "sample",
  "model_label",
  "role",
  "minimization_successful",
  "covariance_step_successful",
  "estimate_near_boundary",
  "significant_digits",
  "condition_number",
  "objf",
  "error_message"
)

.abortRawResults <- function(...) {
  cli::cli_abort(c("!" = ...))
}

.isScalarCharacter <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

.normalizeSchemaList <- function(schema) {
  needed <- c("columns", "thetaCols", "omegaCols", "sigmaCols", "seCols")
  missing <- setdiff(needed, names(schema))
  if (length(missing) > 0L) {
    .abortRawResults(
      "Schema is missing required field{?s} {.val {missing}}."
    )
  }
  schema$baseCols <- if (!is.null(schema$baseCols)) {
    schema$baseCols
  } else {
    .rawResultsBaseCols
  }
  schema$schemaVersion <- if (!is.null(schema$schemaVersion)) {
    as.integer(schema$schemaVersion[[1L]])
  } else {
    .rawResultsSchemaVersion
  }
  schema
}

.thetaNamesFromFit <- function(fit) {
  theta <- fit$theta
  if (!is.null(theta) && length(theta) > 0L && !is.null(names(theta))) {
    return(names(theta))
  }

  ini_df <- fit$iniDf
  if (is.data.frame(ini_df) && "ntheta" %in% names(ini_df)) {
    theta_rows <- !is.na(ini_df$ntheta)
    if ("fix" %in% names(ini_df)) {
      theta_fixed <- !is.na(ini_df$fix) & ini_df$fix
      theta_rows <- theta_rows & !theta_fixed
    }
    theta_names <- ini_df$name[theta_rows]
    theta_names <- theta_names[!is.na(theta_names) & nzchar(theta_names)]
    if (length(theta_names) > 0L) {
      return(theta_names)
    }
  }

  par_df <- fit$parFixedDf
  if (
    is.data.frame(par_df) && nrow(par_df) > 0L && !is.null(rownames(par_df))
  ) {
    theta_names <- rownames(par_df)
    theta_names <- theta_names[!is.na(theta_names) & nzchar(theta_names)]
    if (length(theta_names) > 0L) {
      return(theta_names)
    }
  }

  character(0)
}

.thetaValuesFromFit <- function(fit, theta_names = .thetaNamesFromFit(fit)) {
  vals <- rep(NA_real_, length(theta_names))
  names(vals) <- theta_names

  theta <- fit$theta
  if (!is.null(theta) && length(theta) > 0L) {
    theta <- as.numeric(theta)
    names(theta) <- names(fit$theta)
    common <- intersect(theta_names, names(theta))
    vals[common] <- unname(theta[common])
  }

  vals
}

.thetaSeFromFit <- function(fit, theta_names = .thetaNamesFromFit(fit)) {
  vals <- rep(NA_real_, length(theta_names))
  names(vals) <- theta_names

  par_df <- fit$parFixedDf
  if (
    is.data.frame(par_df) &&
      nrow(par_df) > 0L &&
      "SE" %in% names(par_df) &&
      !is.null(rownames(par_df))
  ) {
    common <- intersect(theta_names, rownames(par_df))
    vals[common] <- as.numeric(par_df[common, "SE", drop = TRUE])
    return(vals)
  }

  cov_mat <- fit$cov
  if (
    is.matrix(cov_mat) && nrow(cov_mat) > 0L && nrow(cov_mat) == ncol(cov_mat)
  ) {
    se <- sqrt(diag(cov_mat))
    common <- intersect(theta_names, names(se))
    vals[common] <- unname(se[common])
  }

  vals
}

.matrixCoordLabel <- function(
  prefix,
  row,
  col,
  row_name = NULL,
  col_name = NULL
) {
  row_lab <- if (!is.null(row_name) && !is.na(row_name) && nzchar(row_name)) {
    row_name
  } else {
    as.character(row)
  }
  col_lab <- if (!is.null(col_name) && !is.na(col_name) && nzchar(col_name)) {
    col_name
  } else {
    as.character(col)
  }
  paste0(prefix, "(", row_lab, ",", col_lab, ")")
}

.matrixInfo <- function(mat, prefix) {
  if (
    !is.matrix(mat) || length(mat) == 0L || nrow(mat) == 0L || ncol(mat) == 0L
  ) {
    return(data.frame(
      colName = character(0),
      row = integer(0),
      col = integer(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  row_names <- rownames(mat)
  col_names <- colnames(mat)
  idx <- which(lower.tri(mat, diag = TRUE), arr.ind = TRUE)
  idx <- idx[order(idx[, "col"], idx[, "row"]), , drop = FALSE]

  data.frame(
    colName = vapply(
      seq_len(nrow(idx)),
      function(i) {
        .matrixCoordLabel(
          prefix = prefix,
          row = idx[i, "row"],
          col = idx[i, "col"],
          row_name = if (!is.null(row_names)) {
            row_names[[idx[i, "row"]]]
          } else {
            NULL
          },
          col_name = if (!is.null(col_names)) {
            col_names[[idx[i, "col"]]]
          } else {
            NULL
          }
        )
      },
      character(1)
    ),
    row = idx[, "row"],
    col = idx[, "col"],
    value = mat[idx],
    stringsAsFactors = FALSE
  )
}

.omegaInfoFromFit <- function(fit) {
  omega_mat <- fit$omega
  if (!is.matrix(omega_mat) || nrow(omega_mat) == 0L || ncol(omega_mat) == 0L) {
    return(.matrixInfo(matrix(numeric(0), 0, 0), "omega"))
  }

  ini_df <- fit$iniDf
  if (
    !is.data.frame(ini_df) ||
      !all(c("neta1", "neta2", "name") %in% names(ini_df))
  ) {
    return(.matrixInfo(omega_mat, "omega"))
  }

  omega_rows <- ini_df[!is.na(ini_df$neta1), , drop = FALSE]
  if (nrow(omega_rows) == 0L) {
    return(.matrixInfo(matrix(numeric(0), 0, 0), "omega"))
  }

  omega_fixed <- if ("fix" %in% names(omega_rows)) {
    !is.na(omega_rows$fix) & omega_rows$fix
  } else {
    rep(FALSE, nrow(omega_rows))
  }
  lower_rows <- omega_rows[
    !omega_fixed & omega_rows$neta1 >= omega_rows$neta2,
    ,
    drop = FALSE
  ]
  if (nrow(lower_rows) == 0L) {
    return(.matrixInfo(matrix(numeric(0), 0, 0), "omega"))
  }
  lower_rows <- lower_rows[
    order(lower_rows$neta2, lower_rows$neta1),
    ,
    drop = FALSE
  ]

  diag_rows <- omega_rows[omega_rows$neta1 == omega_rows$neta2, , drop = FALSE]
  idx_to_name <- stats::setNames(
    diag_rows$name,
    as.character(diag_rows$neta1)
  )
  row_names <- rownames(omega_mat)
  col_names <- colnames(omega_mat)

  data.frame(
    colName = vapply(
      seq_len(nrow(lower_rows)),
      function(i) {
        r <- lower_rows$neta1[[i]]
        c_idx <- lower_rows$neta2[[i]]
        .matrixCoordLabel(
          prefix = "omega",
          row = r,
          col = c_idx,
          row_name = if (!is.null(idx_to_name[[as.character(r)]])) {
            idx_to_name[[as.character(r)]]
          } else if (!is.null(row_names) && length(row_names) >= r) {
            row_names[[r]]
          } else {
            NULL
          },
          col_name = if (!is.null(idx_to_name[[as.character(c_idx)]])) {
            idx_to_name[[as.character(c_idx)]]
          } else if (!is.null(col_names) && length(col_names) >= c_idx) {
            col_names[[c_idx]]
          } else {
            NULL
          }
        )
      },
      character(1)
    ),
    row = as.integer(lower_rows$neta1),
    col = as.integer(lower_rows$neta2),
    value = vapply(
      seq_len(nrow(lower_rows)),
      function(i) {
        omega_mat[lower_rows$neta1[[i]], lower_rows$neta2[[i]]]
      },
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}

.sigmaInfoFromFit <- function(fit) {
  .matrixInfo(fit$sigma, "sigma")
}

.schemaHeader <- function(schema) {
  total_param <- c(schema$thetaCols, schema$omegaCols, schema$sigmaCols)
  block_range <- function(cols, start_index) {
    if (length(cols) == 0L) {
      return(NULL)
    }
    c(as.integer(start_index), as.integer(start_index + length(cols) - 1L))
  }

  theta_start <- length(schema$baseCols) + 1L
  omega_start <- theta_start + length(schema$thetaCols)
  sigma_start <- omega_start + length(schema$omegaCols)
  se_start <- sigma_start + length(schema$sigmaCols)

  list(
    schema_version = as.integer(schema$schemaVersion),
    columns = unname(schema$columns),
    base_cols = unname(schema$baseCols),
    theta_cols = unname(schema$thetaCols),
    omega_cols = unname(schema$omegaCols),
    sigma_cols = unname(schema$sigmaCols),
    se_cols = unname(schema$seCols),
    block_ranges = list(
      base = block_range(schema$baseCols, 1L),
      theta = block_range(schema$thetaCols, theta_start),
      omega = block_range(schema$omegaCols, omega_start),
      sigma = block_range(schema$sigmaCols, sigma_start),
      se = block_range(schema$seCols, se_start)
    ),
    parameter_cols = unname(total_param)
  )
}

.schemaFromHeader <- function(header) {
  .normalizeSchemaList(list(
    columns = header$columns,
    baseCols = if (!is.null(header$base_cols)) {
      header$base_cols
    } else {
      .rawResultsBaseCols
    },
    thetaCols = if (!is.null(header$theta_cols)) {
      header$theta_cols
    } else {
      character(0)
    },
    omegaCols = if (!is.null(header$omega_cols)) {
      header$omega_cols
    } else {
      character(0)
    },
    sigmaCols = if (!is.null(header$sigma_cols)) {
      header$sigma_cols
    } else {
      character(0)
    },
    seCols = if (!is.null(header$se_cols)) header$se_cols else character(0),
    schemaVersion = if (!is.null(header$schema_version)) {
      header$schema_version
    } else {
      .rawResultsSchemaVersion
    }
  ))
}

.inferHeaderFromRows <- function(rows) {
  if (!is.data.frame(rows)) {
    .abortRawResults("{.arg rows} must be a data frame.")
  }

  cols <- names(rows)
  missing_base <- setdiff(.rawResultsBaseCols, cols)
  if (length(missing_base) > 0L) {
    .abortRawResults(
      "Raw-results rows are missing required base column{?s} {.val {missing_base}}."
    )
  }

  non_base <- setdiff(cols, .rawResultsBaseCols)
  se_cols <- non_base[grepl("\\.se$", non_base)]
  param_cols <- setdiff(non_base, se_cols)
  theta_cols <- param_cols[
    !grepl("^omega\\(", param_cols) &
      !grepl("^sigma\\(", param_cols)
  ]
  omega_cols <- param_cols[grepl("^omega\\(", param_cols)]
  sigma_cols <- param_cols[grepl("^sigma\\(", param_cols)]

  ordered_param <- c(theta_cols, omega_cols, sigma_cols)
  ordered_se <- paste0(ordered_param, ".se")
  ordered_cols <- c(.rawResultsBaseCols, ordered_param, ordered_se)

  .schemaHeader(.normalizeSchemaList(list(
    columns = ordered_cols,
    thetaCols = theta_cols,
    omegaCols = omega_cols,
    sigmaCols = sigma_cols,
    seCols = ordered_se
  )))
}

.canonicalizeRows <- function(rows, header = NULL) {
  if (!is.data.frame(rows)) {
    .abortRawResults("{.arg rows} must be a data frame.")
  }

  header <- if (is.null(header)) {
    .inferHeaderFromRows(rows)
  } else {
    header
  }
  schema <- .schemaFromHeader(header)
  missing_cols <- setdiff(schema$columns, names(rows))
  if (length(missing_cols) > 0L) {
    for (col in missing_cols) {
      rows[[col]] <- NA
    }
  }
  rows <- rows[, schema$columns, drop = FALSE]
  attr(rows, "rawResultsHeader") <- .schemaHeader(schema)
  class(rows) <- unique(c("nlmixr2RawResults", class(rows)))
  rows
}

.maybeReadRawResults <- function(rawres) {
  if (is.character(rawres) && length(rawres) == 1L) {
    return(readRawResults(rawres))
  }
  rawres
}

.rawResultsFilterColumns <- function(cols, available) {
  missing_cols <- setdiff(unique(cols), available)
  if (length(missing_cols) > 0L) {
    .abortRawResults(
      "Filter references unknown raw-results column{?s} {.val {missing_cols}}."
    )
  }
}

.coerceFilterResult <- function(x, n_expected) {
  if (!is.logical(x) || length(x) != n_expected) {
    .abortRawResults(
      "Raw-results filter must return a logical vector of length {n_expected}."
    )
  }
  x[is.na(x)] <- FALSE
  x
}

.parsePsnValue <- function(x) {
  if (grepl("^['\"].*['\"]$", x)) {
    return(sub("^['\"](.*)['\"]$", "\\1", x))
  }
  utils::type.convert(x, as.is = TRUE)
}

.vectorWithNames <- function(x, target_names, arg) {
  if (is.null(x)) {
    return(stats::setNames(numeric(0), character(0)))
  }
  nms <- names(x)
  x <- as.numeric(x)
  names(x) <- nms
  if (!is.null(names(x))) {
    return(x)
  }
  if (length(x) != length(target_names)) {
    .abortRawResults(
      "{.arg {arg}} must be named or have length {length(target_names)}."
    )
  }
  stats::setNames(x, target_names)
}

.extractConditionNumber <- function(fit) {
  for (field in c(
    "conditionNumberCov",
    "conditionNumberCor",
    "conditionNumber"
  )) {
    value <- fit[[field]]
    if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
      return(as.numeric(value))
    }
  }
  NA_real_
}

.extractObjf <- function(fit) {
  value <- fit$objf
  if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
    return(as.numeric(value))
  }
  NA_real_
}

.guessMinSuccess <- function(fit, objf) {
  flag <- fit$minimization_successful
  if (length(flag) == 1L && !is.na(flag)) {
    return(as.integer(flag))
  }
  msg <- fit$message
  if (is.character(msg) && length(msg) > 0L && nzchar(msg[[1L]])) {
    return(0L)
  }
  if (is.finite(objf)) {
    return(1L)
  }
  NA_integer_
}

.guessCovSuccess <- function(fit) {
  flag <- fit$covariance_step_successful
  if (length(flag) == 1L && !is.na(flag)) {
    return(as.integer(flag))
  }
  cov_mat <- fit$cov
  if (
    is.matrix(cov_mat) && nrow(cov_mat) > 0L && nrow(cov_mat) == ncol(cov_mat)
  ) {
    return(1L)
  }
  if (is.null(cov_mat) || length(cov_mat) == 0L) {
    return(0L)
  }
  NA_integer_
}

.guessBoundary <- function(fit) {
  flag <- fit$estimate_near_boundary
  if (length(flag) == 1L && !is.na(flag)) {
    return(as.integer(flag))
  }
  NA_integer_
}

.guessSigDigits <- function(fit) {
  digits <- fit$significant_digits
  if (is.numeric(digits) && length(digits) == 1L && !is.na(digits)) {
    return(as.numeric(digits))
  }
  NA_real_
}

#' Canonical raw-results helpers
#'
#' These helpers define the shared per-fit raw-results schema used by the
#' `nlmixr2` extension packages. Writers emit the canonical column order,
#' optional standard-error columns, and a JSON sidecar describing the block
#' boundaries so downstream readers do not need to re-parse parameter labels.
#'
#' @name raw-results
#' @section Lifecycle:
#' Stable.
NULL

#' @rdname raw-results
#' @param fit A fitted `nlmixr2` object, or a fit-like list containing `theta`,
#'   `omega`, `sigma`, and related metadata.
#' @return `rawResultsSchema()` returns a list with `columns`, `baseCols`,
#'   `thetaCols`, `omegaCols`, `sigmaCols`, `seCols`, and `schemaVersion`.
#' @export
rawResultsSchema <- function(fit) {
  theta_cols <- .thetaNamesFromFit(fit)
  omega_info <- .omegaInfoFromFit(fit)
  sigma_info <- .sigmaInfoFromFit(fit)
  param_cols <- c(theta_cols, omega_info$colName, sigma_info$colName)
  se_cols <- paste0(param_cols, ".se")

  .normalizeSchemaList(list(
    columns = c(.rawResultsBaseCols, param_cols, se_cols),
    thetaCols = theta_cols,
    omegaCols = omega_info$colName,
    sigmaCols = sigma_info$colName,
    seCols = se_cols
  ))
}

#' @rdname raw-results
#' @param source Canonical producer name such as `"bootstrap"`, `"sir"`, or
#'   `"sse"`.
#' @param hypothesis Hypothesis label recorded in the raw-results row.
#' @param sample Integer replicate index. Use `0` for a reference row.
#' @param modelLabel Short model label stored in `model_label`.
#' @param role Role label stored in `role`.
#' @param errorMessage Optional error message recorded for failed fits.
#' @param objf Optional objective-function value override.
#' @param minimizationSuccessful,covarianceStepSuccessful,estimateNearBoundary
#'   Optional diagnostic flag overrides. When `NULL`, `rawResultsRow()` uses
#'   simple heuristics based on the supplied fit.
#' @param significantDigits Optional significant-digits override.
#' @param conditionNumber Optional condition-number override.
#' @param theta,omega,sigma Optional parameter overrides. `theta` may be a
#'   named vector or an unnamed vector in schema order. `omega` and `sigma`
#'   may be named vectors, unnamed vectors in schema order, or full matrices.
#' @param se Optional standard-error overrides in parameter-column order.
#' @param schema Optional schema list from [rawResultsSchema()]. Supply this to
#'   target a wider union schema than the one implied by `fit`.
#' @return `rawResultsRow()` returns a one-row data frame in canonical schema
#'   order.
#' @export
rawResultsRow <- function(
  fit = NULL,
  source,
  hypothesis,
  sample,
  modelLabel,
  role,
  errorMessage = NA_character_,
  objf = NULL,
  minimizationSuccessful = NULL,
  covarianceStepSuccessful = NULL,
  estimateNearBoundary = NULL,
  significantDigits = NULL,
  conditionNumber = NULL,
  theta = NULL,
  omega = NULL,
  sigma = NULL,
  se = NULL,
  schema = NULL
) {
  if (!.isScalarCharacter(source)) {
    .abortRawResults("{.arg source} must be a single string.")
  }
  if (!.isScalarCharacter(hypothesis)) {
    .abortRawResults("{.arg hypothesis} must be a single string.")
  }
  if (!is.numeric(sample) || length(sample) != 1L || is.na(sample)) {
    .abortRawResults("{.arg sample} must be a single numeric value.")
  }
  if (!.isScalarCharacter(modelLabel)) {
    .abortRawResults("{.arg modelLabel} must be a single string.")
  }
  if (!.isScalarCharacter(role)) {
    .abortRawResults("{.arg role} must be a single string.")
  }

  if (is.null(schema)) {
    if (is.null(fit)) {
      .abortRawResults(
        "Supply either {.arg fit} or {.arg schema} to build a raw-results row."
      )
    }
    schema <- rawResultsSchema(fit)
  } else {
    schema <- .normalizeSchemaList(schema)
  }

  row <- as.list(stats::setNames(
    rep(NA, length(schema$columns)),
    schema$columns
  ))
  row$source <- source
  row$hypothesis <- hypothesis
  row$sample <- as.integer(sample)
  row$model_label <- modelLabel
  row$role <- role

  fit_theta <- if (!is.null(fit)) {
    .thetaValuesFromFit(fit, schema$thetaCols)
  } else {
    stats::setNames(rep(NA_real_, length(schema$thetaCols)), schema$thetaCols)
  }
  fit_omega <- if (!is.null(fit)) {
    stats::setNames(
      .omegaInfoFromFit(fit)$value,
      .omegaInfoFromFit(fit)$colName
    )
  } else {
    stats::setNames(rep(NA_real_, length(schema$omegaCols)), schema$omegaCols)
  }
  fit_sigma <- if (!is.null(fit)) {
    stats::setNames(
      .sigmaInfoFromFit(fit)$value,
      .sigmaInfoFromFit(fit)$colName
    )
  } else {
    stats::setNames(rep(NA_real_, length(schema$sigmaCols)), schema$sigmaCols)
  }
  fit_se <- if (!is.null(fit)) {
    theta_se <- .thetaSeFromFit(fit, schema$thetaCols)
    stats::setNames(
      c(
        theta_se,
        rep(NA_real_, length(schema$omegaCols) + length(schema$sigmaCols))
      ),
      c(schema$thetaCols, schema$omegaCols, schema$sigmaCols)
    )
  } else {
    stats::setNames(
      rep(
        NA_real_,
        length(schema$thetaCols) +
          length(schema$omegaCols) +
          length(schema$sigmaCols)
      ),
      c(schema$thetaCols, schema$omegaCols, schema$sigmaCols)
    )
  }

  theta_vals <- .vectorWithNames(theta, schema$thetaCols, "theta")
  if (length(theta_vals) > 0L) {
    fit_theta[intersect(names(theta_vals), schema$thetaCols)] <- theta_vals[
      intersect(names(theta_vals), schema$thetaCols)
    ]
  }

  omega_vals <- if (is.matrix(omega)) {
    info <- .matrixInfo(omega, "omega")
    stats::setNames(info$value, info$colName)
  } else {
    .vectorWithNames(omega, schema$omegaCols, "omega")
  }
  if (length(omega_vals) > 0L) {
    fit_omega[intersect(names(omega_vals), schema$omegaCols)] <- omega_vals[
      intersect(names(omega_vals), schema$omegaCols)
    ]
  }

  sigma_vals <- if (is.matrix(sigma)) {
    info <- .matrixInfo(sigma, "sigma")
    stats::setNames(info$value, info$colName)
  } else {
    .vectorWithNames(sigma, schema$sigmaCols, "sigma")
  }
  if (length(sigma_vals) > 0L) {
    fit_sigma[intersect(names(sigma_vals), schema$sigmaCols)] <- sigma_vals[
      intersect(names(sigma_vals), schema$sigmaCols)
    ]
  }

  se_vals <- .vectorWithNames(
    se,
    c(schema$thetaCols, schema$omegaCols, schema$sigmaCols),
    "se"
  )
  if (length(se_vals) > 0L) {
    fit_se[intersect(names(se_vals), names(fit_se))] <- se_vals[
      intersect(names(se_vals), names(fit_se))
    ]
  }

  for (nm in names(fit_theta)) {
    row[[nm]] <- fit_theta[[nm]]
  }
  for (nm in names(fit_omega)) {
    row[[nm]] <- fit_omega[[nm]]
  }
  for (nm in names(fit_sigma)) {
    row[[nm]] <- fit_sigma[[nm]]
  }
  for (nm in names(fit_se)) {
    row[[paste0(nm, ".se")]] <- fit_se[[nm]]
  }

  row$minimization_successful <- if (is.null(minimizationSuccessful)) {
    if (is.null(fit)) {
      NA_integer_
    } else {
      .guessMinSuccess(fit, if (is.null(objf)) .extractObjf(fit) else objf)
    }
  } else {
    as.integer(minimizationSuccessful)
  }
  row$covariance_step_successful <- if (is.null(covarianceStepSuccessful)) {
    if (is.null(fit)) NA_integer_ else .guessCovSuccess(fit)
  } else {
    as.integer(covarianceStepSuccessful)
  }
  row$estimate_near_boundary <- if (is.null(estimateNearBoundary)) {
    if (is.null(fit)) NA_integer_ else .guessBoundary(fit)
  } else {
    as.integer(estimateNearBoundary)
  }
  row$significant_digits <- if (is.null(significantDigits)) {
    if (is.null(fit)) NA_real_ else .guessSigDigits(fit)
  } else {
    as.numeric(significantDigits)
  }
  row$condition_number <- if (is.null(conditionNumber)) {
    if (is.null(fit)) NA_real_ else .extractConditionNumber(fit)
  } else {
    as.numeric(conditionNumber)
  }
  row$objf <- if (is.null(objf)) {
    if (is.null(fit)) NA_real_ else .extractObjf(fit)
  } else {
    as.numeric(objf)
  }
  row$error_message <- if (is.null(errorMessage)) {
    NA_character_
  } else {
    as.character(errorMessage)
  }

  out <- as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  .canonicalizeRows(out, .schemaHeader(schema))
}

#' @rdname raw-results
#' @param rows Data frame of canonical raw-results rows.
#' @param dir Directory where the CSV, RDS, and JSON sidecar should be written.
#' @param basename Basename used for the output files.
#' @return `writeRawResults()` invisibly returns a list containing the canonical
#'   data frame and the three output paths.
#' @export
writeRawResults <- function(rows, dir, basename = "raw_results") {
  if (!.isScalarCharacter(dir)) {
    .abortRawResults("{.arg dir} must be a single directory path.")
  }
  if (!.isScalarCharacter(basename)) {
    .abortRawResults("{.arg basename} must be a single string.")
  }

  header <- attr(rows, "rawResultsHeader", exact = TRUE)
  canonical <- .canonicalizeRows(rows, header)
  header <- attr(canonical, "rawResultsHeader", exact = TRUE)

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  csv_path <- file.path(dir, paste0(basename, ".csv"))
  rds_path <- file.path(dir, paste0(basename, ".rds"))
  header_path <- file.path(dir, paste0(basename, "_header.json"))

  utils::write.csv(
    canonical,
    csv_path,
    row.names = FALSE,
    na = "NA"
  )
  saveRDS(canonical, rds_path)
  writeLines(
    jsonlite::toJSON(header, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    con = header_path,
    useBytes = TRUE
  )

  invisible(list(
    data = canonical,
    csvPath = csv_path,
    rdsPath = rds_path,
    headerPath = header_path
  ))
}

.resolveRawResultsPaths <- function(path) {
  if (!.isScalarCharacter(path)) {
    .abortRawResults("{.arg path} must be a single file or directory path.")
  }

  if (dir.exists(path)) {
    base <- file.path(path, "raw_results")
    if (file.exists(paste0(base, ".rds"))) {
      return(list(dataPath = paste0(base, ".rds"), basePath = base))
    }
    if (file.exists(paste0(base, ".csv"))) {
      return(list(dataPath = paste0(base, ".csv"), basePath = base))
    }
    .abortRawResults(
      "Could not find {.file raw_results.csv} or {.file raw_results.rds} in {.path {path}}."
    )
  }

  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "rds")) {
    base <- sub(paste0("\\.", ext, "$"), "", path)
    return(list(dataPath = path, basePath = base))
  }
  if (file.exists(paste0(path, ".rds"))) {
    return(list(dataPath = paste0(path, ".rds"), basePath = path))
  }
  if (file.exists(paste0(path, ".csv"))) {
    return(list(dataPath = paste0(path, ".csv"), basePath = path))
  }

  .abortRawResults(
    "Could not resolve a raw-results file from {.path {path}}."
  )
}

#' @rdname raw-results
#' @param path Path to a canonical raw-results CSV, RDS, or containing
#'   directory.
#' @return `readRawResults()` returns the raw-results data frame with the parsed
#'   header attached as the `rawResultsHeader` attribute.
#' @export
readRawResults <- function(path) {
  paths <- .resolveRawResultsPaths(path)
  header_path <- paste0(paths$basePath, "_header.json")
  ext <- tolower(tools::file_ext(paths$dataPath))

  data <- if (ext == "rds") {
    readRDS(paths$dataPath)
  } else {
    utils::read.csv(
      paths$dataPath,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = "NA"
    )
  }

  header <- if (file.exists(header_path)) {
    jsonlite::fromJSON(header_path, simplifyVector = TRUE)
  } else {
    attr(data, "rawResultsHeader", exact = TRUE)
  }
  if (is.null(header)) {
    cli::cli_warn(c(
      "!" = "Raw-results header sidecar was not found for {.path {paths$dataPath}}.",
      "i" = "Inferring block boundaries from column names."
    ))
    header <- .inferHeaderFromRows(data)
  }

  version <- header$schema_version
  if (is.null(version)) {
    .abortRawResults("Raw-results header is missing {.field schema_version}.")
  }
  version_num <- as.integer(version[[1L]])
  current_version <- .rawResultsSchemaVersion
  if (version_num > current_version) {
    .abortRawResults(
      paste0(
        "Raw-results schema version ",
        version_num,
        " is newer than this package understands (",
        current_version,
        ")."
      )
    )
  }

  data <- .canonicalizeRows(data, header)
  attr(data, "rawResultsPath") <- normalizePath(
    paths$dataPath,
    mustWork = FALSE
  )
  data
}

#' @rdname raw-results
#' @param filter Either a PsN-style character filter, a one-sided formula, or
#'   an unevaluated expression.
#' @return `setupRawResultsFilter()` returns a predicate function that accepts a
#'   raw-results data frame and returns a logical inclusion vector.
#' @export
setupRawResultsFilter <- function(filter) {
  if (is.null(filter)) {
    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      rep(TRUE, nrow(rawres))
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  if (is.function(filter)) {
    class(filter) <- unique(c("nlmixr2RawResultsFilter", class(filter)))
    return(filter)
  }

  if (inherits(filter, "formula")) {
    expr <- filter[[2L]]
    env <- environment(filter)
    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      .rawResultsFilterColumns(all.vars(expr), names(rawres))
      out <- eval(expr, envir = rawres, enclos = env)
      .coerceFilterResult(out, nrow(rawres))
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  if (is.language(filter)) {
    expr <- filter
    env <- parent.frame()
    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      .rawResultsFilterColumns(all.vars(expr), names(rawres))
      out <- eval(expr, envir = rawres, enclos = env)
      .coerceFilterResult(out, nrow(rawres))
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  if (is.character(filter)) {
    parts <- trimws(unlist(
      strsplit(filter, ",", fixed = TRUE),
      use.names = FALSE
    ))
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0L) {
      .abortRawResults("{.arg filter} did not contain any filter clauses.")
    }

    clauses <- lapply(parts, function(part) {
      match <- regexec("^(.*)\\.(eq|ne|gt|ge|lt|le)\\.(.*)$", part)
      parsed <- regmatches(part, match)[[1L]]
      if (length(parsed) != 4L) {
        .abortRawResults(
          "Could not parse PsN-style filter clause {.val {part}}."
        )
      }
      list(
        column = parsed[[2L]],
        op = parsed[[3L]],
        value = .parsePsnValue(parsed[[4L]])
      )
    })

    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      .rawResultsFilterColumns(
        vapply(clauses, `[[`, character(1), "column"),
        names(rawres)
      )

      out <- rep(TRUE, nrow(rawres))
      for (clause in clauses) {
        lhs <- rawres[[clause$column]]
        rhs <- clause$value
        this <- switch(
          clause$op,
          eq = lhs == rhs,
          ne = lhs != rhs,
          gt = lhs > rhs,
          ge = lhs >= rhs,
          lt = lhs < rhs,
          le = lhs <= rhs
        )
        this[is.na(this)] <- FALSE
        out <- out & this
      }
      out
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  .abortRawResults(
    "{.arg filter} must be NULL, a function, a formula, an expression, or a character vector."
  )
}

.rebuildLowerTriMatrix <- function(info, values, template, sample_id, kind) {
  if (nrow(info) == 0L) {
    return(template)
  }
  mat <- template
  for (i in seq_len(nrow(info))) {
    nm <- info$colName[[i]]
    value <- values[[nm]]
    if (is.na(value)) {
      .abortRawResults(
        "Sample {.val {sample_id}} is missing required {.val {kind}} parameter {.val {nm}}."
      )
    }
    mat[info$row[[i]], info$col[[i]]] <- value
    mat[info$col[[i]], info$row[[i]]] <- value
  }
  mat
}

#' @rdname raw-results
#' @param rawres A raw-results data frame, a path understood by
#'   [readRawResults()], or the result of [readRawResults()].
#' @param offset Integer sample offset. The default `1L` skips the canonical
#'   reference row with `sample = 0`.
#' @return `parseRawResultsParams()` returns a named list of per-sample
#'   parameter sets, each containing `sample`, `source`, `hypothesis`,
#'   `modelLabel`, `role`, `theta`, `omega`, and `sigma`.
#' @export
parseRawResultsParams <- function(rawres, fit, offset = 1L, filter = NULL) {
  rawres <- .maybeReadRawResults(rawres)
  if (!is.data.frame(rawres)) {
    .abortRawResults("{.arg rawres} must resolve to a data frame.")
  }
  if (
    !is.numeric(offset) || length(offset) != 1L || is.na(offset) || offset < 0
  ) {
    .abortRawResults("{.arg offset} must be a single non-negative number.")
  }

  raw_header <- attr(rawres, "rawResultsHeader", exact = TRUE)
  if (is.null(raw_header)) {
    raw_header <- .inferHeaderFromRows(rawres)
  }
  raw_schema <- .schemaFromHeader(raw_header)
  fit_schema <- rawResultsSchema(fit)

  missing_theta <- setdiff(fit_schema$thetaCols, raw_schema$thetaCols)
  missing_omega <- setdiff(fit_schema$omegaCols, raw_schema$omegaCols)
  missing_sigma <- setdiff(fit_schema$sigmaCols, raw_schema$sigmaCols)
  missing_cols <- c(missing_theta, missing_omega, missing_sigma)
  if (length(missing_cols) > 0L) {
    .abortRawResults(
      "Raw-results input is missing parameter column{?s} {.val {missing_cols}} required by the supplied fit."
    )
  }

  rows <- rawres[rawres$sample >= as.integer(offset), , drop = FALSE]
  if (!is.null(filter)) {
    predicate <- if (is.function(filter)) {
      filter
    } else {
      setupRawResultsFilter(filter)
    }
    rows <- rows[predicate(rows), , drop = FALSE]
  }
  if (nrow(rows) == 0L) {
    return(list())
  }

  rows <- rows[order(rows$sample), , drop = FALSE]
  if (anyDuplicated(rows$sample) > 0L) {
    .abortRawResults(
      "More than one raw-results row remains for at least one sample after applying {.arg offset} and {.arg filter}."
    )
  }

  theta_template <- .thetaValuesFromFit(fit, fit_schema$thetaCols)
  omega_template <- fit$omega
  if (!is.matrix(omega_template)) {
    omega_template <- matrix(numeric(0), 0, 0)
  }
  sigma_template <- fit$sigma
  if (!is.matrix(sigma_template)) {
    sigma_template <- matrix(numeric(0), 0, 0)
  }
  omega_info <- .omegaInfoFromFit(fit)
  sigma_info <- .sigmaInfoFromFit(fit)

  out <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    sample_id <- as.integer(row$sample[[1L]])

    theta_vals <- theta_template
    if (length(fit_schema$thetaCols) > 0L) {
      theta_vals[fit_schema$thetaCols] <- as.numeric(row[
        1,
        fit_schema$thetaCols,
        drop = TRUE
      ])
      if (anyNA(theta_vals[fit_schema$thetaCols])) {
        missing_theta <- fit_schema$thetaCols[is.na(theta_vals[
          fit_schema$thetaCols
        ])]
        .abortRawResults(
          "Sample {.val {sample_id}} is missing required theta parameter{?s} {.val {missing_theta}}."
        )
      }
    }

    omega_vals <- if (length(fit_schema$omegaCols) > 0L) {
      stats::setNames(
        as.numeric(row[1, fit_schema$omegaCols, drop = TRUE]),
        fit_schema$omegaCols
      )
    } else {
      numeric(0)
    }
    sigma_vals <- if (length(fit_schema$sigmaCols) > 0L) {
      stats::setNames(
        as.numeric(row[1, fit_schema$sigmaCols, drop = TRUE]),
        fit_schema$sigmaCols
      )
    } else {
      numeric(0)
    }

    list(
      sample = sample_id,
      source = row$source[[1L]],
      hypothesis = row$hypothesis[[1L]],
      modelLabel = row$model_label[[1L]],
      role = row$role[[1L]],
      theta = theta_vals,
      omega = .rebuildLowerTriMatrix(
        omega_info,
        omega_vals,
        omega_template,
        sample_id = sample_id,
        kind = "omega"
      ),
      sigma = .rebuildLowerTriMatrix(
        sigma_info,
        sigma_vals,
        sigma_template,
        sample_id = sample_id,
        kind = "sigma"
      )
    )
  })
  names(out) <- paste0("sample_", rows$sample)
  out
}
