# =============================================================================
# CPT Visualization — Interactive Shiny App
#
# Based on the CPT model in cct_hbayes_optimized_v2.stan:
#   Value function:        v(x) = x^alpha  (gains);  v(x) = -lambda * |x|^alpha (losses)
#   Probability weighting: w(p) = exp(-(-ln p)^gamma)   [Prelec-1]
#   Choice rule:           P(continue) = logistic(theta * V_continue)
#   V_continue = w(p_gain) * v(gain) + w(p_loss) * v(loss)
#
# Layout: sidebar (controls) + two tabs (CPT Functions | CCT Simulation)
# =============================================================================

library(shiny)
library(ggplot2)

# -- CPT helper functions (mirror Stan model) ---------------------------------

value_fn <- function(x, alpha, lambda) {
  ifelse(x >= 0, x^alpha, -lambda * abs(x)^alpha)
}

prelec_weight <- function(p, gamma) {
  p <- pmax(pmin(p, 1 - 1e-9), 1e-9)
  exp(-(-log(p))^gamma)
}

compute_V_continue <- function(gain, loss, p_loss,
                               alpha, lambda, gamma) {
  p_gain <- 1 - p_loss
  v_gain <- gain^alpha
  v_loss <- -lambda * loss^alpha
  w_gain <- prelec_weight(p_gain, gamma)
  w_loss <- prelec_weight(p_loss, gamma)
  w_gain * v_gain + w_loss * v_loss
}

# Simulate a single CCT trial; returns list(cards_turned, outcome, payoff)
simulate_one_trial <- function(alpha, lambda, gamma_val, theta,
                               gain, loss, n_loss) {
  n_safe <- TOTAL_CARDS - n_loss
  deck <- sample(c(rep("loss", n_loss), rep("safe", n_safe)))
  cumulative_gain <- 0

  for (i in seq_len(TOTAL_CARDS)) {
    cards_turned <- i - 1
    remaining <- TOTAL_CARDS - cards_turned
    loss_remaining <- sum(deck[(cards_turned + 1):TOTAL_CARDS] == "loss")
    pl <- loss_remaining / remaining

    V <- compute_V_continue(gain, loss, pl, alpha, lambda, gamma_val)
    p_cont <- plogis(theta * V)

    if (rbinom(1, 1, p_cont) == 0) {
      return(list(cards_turned = cards_turned,
                  outcome = "Stop", payoff = cumulative_gain))
    }

    if (deck[i] == "loss") {
      return(list(cards_turned = cards_turned + 1,
                  outcome = "Loss", payoff = -loss))
    }
    cumulative_gain <- cumulative_gain + gain
  }

  list(cards_turned = n_safe,
       outcome = "Stop", payoff = cumulative_gain)
}

# -- Constants -----------------------------------------------------------------

TOTAL_CARDS <- 32
HEATMAP_LIMIT <- 300

# -- UI ------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .plot-square {
      width: 37.5vw; height: 37.5vw; margin: 0 auto;
    }
    .sim-plot {
      width: 75vw; height: 37.5vw; margin: 0 auto;
    }
  "))),
  titlePanel("CPT Model Explorer \u2014 Columbia Card Task"),

  fluidRow(
    # ---- Sidebar controls ----
    column(2,
      wellPanel(
        h4("CPT Parameters"),
        sliderInput("alpha", "\u03b1 (outcome sensitivity)",
                    min = 0.01, max = 5, value = 0.88,
                    step = 0.01),
        numericInput("alpha_num", NULL,
                     value = 0.88, min = 0.01, max = 5,
                     step = 0.01),
        sliderInput("lambda", "\u03bb (loss aversion)",
                    min = 0.01, max = 5, value = 2.25,
                    step = 0.01),
        numericInput("lambda_num", NULL,
                     value = 2.25, min = 0.01, max = 5,
                     step = 0.01),
        sliderInput("gamma", "\u03b3 (probability distortion)",
                    min = 0.01, max = 5, value = 0.61,
                    step = 0.01),
        numericInput("gamma_num", NULL,
                     value = 0.61, min = 0.01, max = 5,
                     step = 0.01),
        actionButton("reset_cpt", "Reset CPT Defaults",
                     width = "100%"),

        hr(),
        h4("CCT Task Parameters"),
        sliderInput("theta", "\u03b8 (inverse temperature)",
                    min = 0.1, max = 20, value = 5,
                    step = 0.1),
        numericInput("theta_num", NULL,
                     value = 5, min = 0.1, max = 20,
                     step = 0.1),
        numericInput("gain_amt", "Gain amount",
                     value = 10, min = 1, max = 500,
                     step = 1),
        numericInput("loss_amt", "Loss amount",
                     value = 30, min = 1, max = 500,
                     step = 1),
        sliderInput("loss_cards", "Number of loss cards",
                    min = 1, max = 31, value = 3,
                    step = 1),
        sliderInput("cards_turned",
                    "Cards already turned (PV plot)",
                    min = 0, max = 31, value = 0,
                    step = 1)
      )
    ),

    # ---- Main content: two tabs ----
    column(10,
      tabsetPanel(
        type = "tabs",

        # ======== Tab 1: CPT Functions ========
        tabPanel("CPT Functions",
          fluidRow(
            column(6, div(class = "plot-square",
              plotOutput("plot_value_fn",
                         height = "100%", width = "100%")
            )),
            column(6, div(class = "plot-square",
              plotOutput("plot_prob_weight",
                         height = "100%", width = "100%")
            ))
          ),
          fluidRow(
            column(6,
              radioButtons("pv_view", NULL,
                choices = c(
                  "Line: V vs cards turned" = "line",
                  "Heatmap: gain \u00d7 loss" = "heatmap"
                ), inline = TRUE),
              div(class = "plot-square",
                plotOutput("plot_prospect",
                           height = "100%", width = "100%")
              )
            ),
            column(6, div(class = "plot-square",
              plotOutput("plot_choice_prob",
                         height = "100%", width = "100%")
            ))
          )
        ),

        # ======== Tab 2: CCT Simulation ========
        tabPanel("CCT Simulation",
          # -- Top: single trial step-through --
          h4("Single Trial Step-Through"),
          fluidRow(
            column(2, actionButton("simulate",
              "Simulate 1 Trial",
              width = "100%", class = "btn-primary")),
            column(2, actionButton("reset_sim",
              "Reset", width = "100%"))
          ),
          br(),
          div(style = paste0(
            "max-height: 40vh; overflow-y: auto;",
            " border: 1px solid #ddd;",
            " border-radius: 4px;"),
            tableOutput("sim_table")
          ),
          uiOutput("sim_summary"),

          hr(),

          # -- Bottom: batch simulation --
          h4("Batch Simulation"),
          fluidRow(
            column(2, numericInput("n_sims",
              "Number of simulations",
              value = 200, min = 10, max = 5000,
              step = 10)),
            column(2, br(),
              actionButton("run_batch",
                "Run Batch", width = "100%",
                class = "btn-success"))
          ),
          div(class = "sim-plot",
            plotOutput("plot_batch_hist",
                       height = "100%", width = "100%")
          ),
          uiOutput("batch_summary")
        )
      )
    )
  )
)

# -- Server --------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- Sync sliders <-> numeric inputs (guard against loops) ----
  sync_pair <- function(slider_id, num_id) {
    observeEvent(input[[slider_id]], {
      val <- input[[slider_id]]
      if (!isTRUE(all.equal(val, input[[num_id]])))
        updateNumericInput(session, num_id, value = val)
    }, ignoreInit = TRUE)
    observeEvent(input[[num_id]], {
      val <- input[[num_id]]
      if (!is.na(val) && !isTRUE(all.equal(val, input[[slider_id]])))
        updateSliderInput(session, slider_id, value = val)
    }, ignoreInit = TRUE)
  }
  sync_pair("alpha", "alpha_num")
  sync_pair("lambda", "lambda_num")
  sync_pair("gamma", "gamma_num")
  sync_pair("theta", "theta_num")

  # ---- Reset CPT defaults ----
  observeEvent(input$reset_cpt, {
    updateSliderInput(session, "alpha", value = 0.88)
    updateSliderInput(session, "lambda", value = 2.25)
    updateSliderInput(session, "gamma", value = 0.61)
  })

  # ==================================================================
  # Tab 1: CPT Functions
  # ==================================================================

  # ---- Plot 1: Value Function v(x) ----
  output$plot_value_fn <- renderPlot({
    x <- seq(-300, 300, length.out = 601)
    y <- value_fn(x, input$alpha, input$lambda)
    df <- data.frame(x = x, y = y)
    ggplot(df, aes(x, y)) +
      geom_line(color = "#2166AC", linewidth = 1.2) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "grey50") +
      geom_hline(yintercept = 0, color = "grey30") +
      geom_vline(xintercept = 0, color = "grey30") +
      labs(title = "Value Function v(x)",
           x = "Outcome (x)",
           y = "Subjective Value v(x)") +
      theme_minimal(base_size = 14) +
      coord_cartesian(xlim = c(-300, 300)) +
      theme(aspect.ratio = 1)
  })

  # ---- Plot 2: Probability Weighting w(p) ----
  output$plot_prob_weight <- renderPlot({
    p <- seq(0.001, 0.999, length.out = 500)
    w <- prelec_weight(p, input$gamma)
    df <- data.frame(p = p, w = w)
    ggplot(df, aes(p, w)) +
      geom_line(color = "#B2182B", linewidth = 1.2) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "grey50") +
      labs(title = "Probability Weighting w(p)  [Prelec-1]",
           x = "Objective Probability p",
           y = "Decision Weight w(p)") +
      theme_minimal(base_size = 14) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      theme(aspect.ratio = 1)
  })

  # ---- Plot 3: Prospect Value (two views) ----
  output$plot_prospect <- renderPlot({
    if (input$pv_view == "line") {
      cards_seq <- 0:(TOTAL_CARDS - input$loss_cards - 1)
      remaining <- TOTAL_CARDS - cards_seq
      p_loss <- input$loss_cards / remaining
      V <- sapply(p_loss, function(pl) {
        compute_V_continue(
          input$gain_amt, input$loss_amt, pl,
          input$alpha, input$lambda, input$gamma)
      })
      df <- data.frame(cards_turned = cards_seq, V = V)
      ggplot(df, aes(cards_turned, V)) +
        geom_line(color = "#2166AC", linewidth = 1.2) +
        geom_point(color = "#2166AC", size = 2) +
        geom_hline(yintercept = 0,
                   linetype = "dashed", color = "red") +
        labs(title = sprintf(
          "V_continue vs Cards Turned  (G=%g, L=%g, #loss=%d)",
          input$gain_amt, input$loss_amt, input$loss_cards),
          x = "Cards Already Turned",
          y = expression(V[continue])) +
        theme_minimal(base_size = 14) +
        theme(aspect.ratio = 1)
    } else {
      gains <- seq(1, 300, length.out = 60)
      losses <- seq(1, 300, length.out = 60)
      grid <- expand.grid(gain = gains, loss = losses)
      remaining <- TOTAL_CARDS - input$cards_turned
      p_loss <- input$loss_cards / remaining
      grid$V <- mapply(function(g, l) {
        compute_V_continue(
          g, l, p_loss,
          input$alpha, input$lambda, input$gamma)
      }, grid$gain, grid$loss)
      p <- ggplot(grid, aes(gain, loss, fill = V)) +
        geom_tile() +
        scale_fill_gradient2(
          low = "#B2182B", mid = "white", high = "#2166AC",
          midpoint = 0,
          limits = c(-HEATMAP_LIMIT, HEATMAP_LIMIT),
          oob = scales::squish,
          name = expression(V[cont])) +
        labs(title = sprintf(
          "Prospect Value  (turned=%d, p_loss=%.3f)",
          input$cards_turned, p_loss),
          x = "Gain Amount", y = "Loss Amount") +
        theme_minimal(base_size = 14) +
        theme(aspect.ratio = 1)
      # Only draw V=0 contour if it exists
      if (any(grid$V > 0) && any(grid$V < 0)) {
        p <- p + geom_contour(
          aes(x = gain, y = loss, z = V),
          inherit.aes = FALSE,
          breaks = 0, color = "black", linewidth = 0.8)
      }
      p
    }
  })

  # ---- Plot 4: P(continue) vs cards turned ----
  output$plot_choice_prob <- renderPlot({
    cards_seq <- 0:(TOTAL_CARDS - input$loss_cards - 1)
    remaining <- TOTAL_CARDS - cards_seq
    p_loss <- input$loss_cards / remaining
    V <- sapply(p_loss, function(pl) {
      compute_V_continue(
        input$gain_amt, input$loss_amt, pl,
        input$alpha, input$lambda, input$gamma)
    })
    p_cont <- plogis(input$theta * V)
    df <- data.frame(cards_turned = cards_seq,
                     p_continue = p_cont)
    ggplot(df, aes(cards_turned, p_continue)) +
      geom_line(color = "#4DAF4A", linewidth = 1.2) +
      geom_point(color = "#4DAF4A", size = 2) +
      geom_hline(yintercept = 0.5,
                 linetype = "dashed", color = "grey50") +
      labs(title = sprintf(
        "P(continue) vs Cards Turned  (\u03b8=%g)",
        input$theta),
        x = "Cards Already Turned",
        y = "P(continue)") +
      theme_minimal(base_size = 14) +
      coord_cartesian(ylim = c(0, 1)) +
      theme(aspect.ratio = 1)
  })

  # ==================================================================
  # Tab 2: CCT Simulation
  # ==================================================================

  # ---- Single trial step-through ----
  sim_data <- reactiveVal(NULL)

  observeEvent(input$simulate, {
    set.seed(NULL)
    alpha <- input$alpha
    lambda <- input$lambda
    gamma_val <- input$gamma
    theta <- input$theta
    gain <- input$gain_amt
    loss <- input$loss_amt
    n_loss <- input$loss_cards

    deck <- sample(c(rep("loss", n_loss),
                     rep("safe", TOTAL_CARDS - n_loss)))

    results <- data.frame(
      Step = integer(), Cards_Turned = integer(),
      Remaining = integer(), p_loss = numeric(),
      V_continue = numeric(), P_continue = numeric(),
      Decision = character(), Card_Drawn = character(),
      Cumulative_Gain = numeric(),
      stringsAsFactors = FALSE)

    cumulative_gain <- 0

    for (i in seq_len(TOTAL_CARDS)) {
      cards_turned <- i - 1
      remaining <- TOTAL_CARDS - cards_turned
      loss_rem <- sum(
        deck[(cards_turned + 1):TOTAL_CARDS] == "loss")
      pl <- loss_rem / remaining

      V <- compute_V_continue(
        gain, loss, pl, alpha, lambda, gamma_val)
      p_cont <- plogis(theta * V)

      if (rbinom(1, 1, p_cont) == 0) {
        results <- rbind(results, data.frame(
          Step = i, Cards_Turned = cards_turned,
          Remaining = remaining,
          p_loss = round(pl, 4),
          V_continue = round(V, 4),
          P_continue = round(p_cont, 4),
          Decision = "STOP", Card_Drawn = "\u2014",
          Cumulative_Gain = cumulative_gain,
          stringsAsFactors = FALSE))
        break
      }

      drawn <- deck[i]
      if (drawn == "loss") {
        results <- rbind(results, data.frame(
          Step = i, Cards_Turned = cards_turned,
          Remaining = remaining,
          p_loss = round(pl, 4),
          V_continue = round(V, 4),
          P_continue = round(p_cont, 4),
          Decision = "Continue", Card_Drawn = "LOSS",
          Cumulative_Gain = -loss,
          stringsAsFactors = FALSE))
        break
      }

      cumulative_gain <- cumulative_gain + gain
      results <- rbind(results, data.frame(
        Step = i, Cards_Turned = cards_turned,
        Remaining = remaining,
        p_loss = round(pl, 4),
        V_continue = round(V, 4),
        P_continue = round(p_cont, 4),
        Decision = "Continue", Card_Drawn = "safe",
        Cumulative_Gain = cumulative_gain,
        stringsAsFactors = FALSE))
    }

    sim_data(results)
  })

  observeEvent(input$reset_sim, { sim_data(NULL) })

  output$sim_table <- renderTable({
    req(sim_data())
    sim_data()
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     digits = 4)

  output$sim_summary <- renderUI({
    req(sim_data())
    d <- sim_data()
    final <- tail(d, 1)
    payoff <- final$Cumulative_Gain
    if (final$Card_Drawn == "LOSS") {
      bg <- "#f8d7da"
      msg <- sprintf("Hit a LOSS card at step %d! Payoff = %g",
                     final$Step, payoff)
    } else if (final$Decision == "STOP") {
      bg <- "#d4edda"
      msg <- sprintf(
        "Stopped voluntarily after %d cards. Payoff = %g",
        final$Cards_Turned, payoff)
    } else {
      bg <- "#d4edda"
      msg <- sprintf("Turned all safe cards! Payoff = %g",
                     payoff)
    }
    tags$div(
      style = paste0(
        "margin-top:10px; padding:10px; background:", bg,
        "; border-radius:5px;"),
      tags$strong("Result: "), msg)
  })

  # ---- Batch simulation ----
  batch_data <- reactiveVal(NULL)

  observeEvent(input$run_batch, {
    set.seed(NULL)
    n <- input$n_sims
    alpha <- input$alpha
    lambda <- input$lambda
    gamma_val <- input$gamma
    theta <- input$theta
    gain <- input$gain_amt
    loss <- input$loss_amt
    n_loss <- input$loss_cards

    res <- replicate(n, simulate_one_trial(
      alpha, lambda, gamma_val, theta,
      gain, loss, n_loss), simplify = FALSE)

    df <- data.frame(
      cards_turned = sapply(res, `[[`, "cards_turned"),
      outcome = sapply(res, `[[`, "outcome"),
      payoff = sapply(res, `[[`, "payoff"),
      stringsAsFactors = FALSE)

    batch_data(df)
  })

  output$plot_batch_hist <- renderPlot({
    req(batch_data())
    df <- batch_data()
    n_safe <- TOTAL_CARDS - input$loss_cards

    ggplot(df, aes(x = cards_turned, fill = outcome)) +
      geom_histogram(
        binwidth = 1, color = "white",
        boundary = -0.5, position = "stack") +
      scale_fill_manual(
        values = c("Stop" = "#4DAF4A", "Loss" = "#E41A1C"),
        name = "Outcome") +
      scale_x_continuous(
        breaks = seq(0, n_safe, by = 2)) +
      labs(
        title = sprintf(
          "Batch Simulation: %d trials  (G=%g, L=%g, #loss=%d, \u03b8=%g)",
          nrow(df), input$gain_amt, input$loss_amt,
          input$loss_cards, input$theta),
        x = "Total Cards Turned",
        y = "Count") +
      theme_minimal(base_size = 14)
  })

  output$batch_summary <- renderUI({
    req(batch_data())
    df <- batch_data()
    n <- nrow(df)
    n_stop <- sum(df$outcome == "Stop")
    n_loss <- sum(df$outcome == "Loss")
    avg_cards <- mean(df$cards_turned)
    avg_payoff <- mean(df$payoff)

    tags$div(
      style = paste0(
        "margin-top:10px; padding:10px;",
        " background:#f0f0f0; border-radius:5px;"),
      tags$table(
        style = "width:100%;",
        tags$tr(
          tags$td(tags$strong("Trials: "), n),
          tags$td(tags$strong("Stop: "),
            sprintf("%d (%.1f%%)", n_stop,
                    100 * n_stop / n)),
          tags$td(tags$strong("Loss: "),
            sprintf("%d (%.1f%%)", n_loss,
                    100 * n_loss / n))
        ),
        tags$tr(
          tags$td(tags$strong("Avg cards: "),
                  sprintf("%.1f", avg_cards)),
          tags$td(tags$strong("Avg payoff: "),
                  sprintf("%.1f", avg_payoff)),
          tags$td()
        )
      )
    )
  })
}

# -- Launch --------------------------------------------------------------------
shinyApp(ui = ui, server = server)
