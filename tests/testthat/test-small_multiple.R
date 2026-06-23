test_that("small_multiple returns a ggplot object", {
    m1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)
    m2 <- update(m1, . ~ . + hp)
    p <- small_multiple(list(m1, m2))
    expect_s3_class(p, "ggplot")
})

test_that("model_order sets the order of models on the x-axis (Issue #108)", {
    m1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)
    m2 <- update(m1, . ~ . + hp)
    m3 <- update(m2, . ~ . + am)

    # Unnamed list: models are labelled "Model 1", "Model 2", ...
    p <- small_multiple(list(m1, m2, m3),
                        model_order = c("Model 3", "Model 1", "Model 2"))
    x_labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
    expect_equal(x_labels, c("Model 3", "Model 1", "Model 2"))

    # Named list: models keep their names
    p_named <- small_multiple(list(A = m1, B = m2, C = m3),
                              model_order = c("C", "A", "B"))
    x_labels_named <- ggplot2::ggplot_build(p_named)$layout$panel_params[[1]]$x$get_labels()
    expect_equal(x_labels_named, c("C", "A", "B"))
})

test_that("submodel_order sets the order of submodels (Issue #108)", {
    fit <- function(f, d) broom::tidy(lm(f, data = d))
    mk <- function(f, label) dplyr::bind_rows(
        cbind(fit(f, mtcars[mtcars$am == 0, ]), submodel = "0", model = label),
        cbind(fit(f, mtcars[mtcars$am == 1, ]), submodel = "1", model = label)
    )
    by_trans <- dplyr::bind_rows(
        mk(mpg ~ wt, "Model 1"),
        mk(mpg ~ wt + cyl, "Model 2")
    )

    p <- small_multiple(by_trans, submodel_order = c("1", "0"))
    expect_equal(levels(p$data$submodel), c("1", "0"))
})
