create_group_selection_state <- function() {
  reactiveValues(
    selections = list(),
    combined_groups = list(),
    include_total = FALSE
  )
}

group_selection_controller <- function(
  input,
  session,
  col_groups,
  group_state = NULL,
  active_tab = NULL,
  tab_value = NULL,
  include_total_input_id = "include_total"
) {
  use_shared_group_state <- inherits(group_state, "reactivevalues")
  local_combined_groups <- reactiveVal(list())

  is_module_active <- reactive({
    if (is.null(active_tab) || is.null(tab_value)) {
      return(TRUE)
    }

    identical(active_tab(), tab_value)
  })

  get_combined_groups <- reactive({
    if (!use_shared_group_state) {
      return(local_combined_groups())
    }

    value <- group_state$combined_groups
    if (is.list(value)) value else list()
  })

  set_combined_groups <- function(value) {
    if (use_shared_group_state) {
      group_state$combined_groups <- value
    } else {
      local_combined_groups(value)
    }
  }

  current_include_total <- reactive({
    if (!use_shared_group_state) {
      return(isTRUE(input[[include_total_input_id]]))
    }

    isTRUE(group_state$include_total)
  })

  observeEvent(input[[include_total_input_id]], {
    if (!use_shared_group_state) {
      return()
    }

    if (!isTRUE(is_module_active())) {
      return()
    }

    value <- isTRUE(input[[include_total_input_id]])
    if (!identical(isTRUE(group_state$include_total), value)) {
      group_state$include_total <- value
    }
  }, ignoreInit = TRUE)

  selected_group_selections <- reactive({
    groups <- col_groups()
    if (!length(groups)) {
      return(list())
    }

    if (use_shared_group_state) {
      return(plot_sanitize_group_selections(group_state$selections, groups))
    }

    plot_read_group_selections_from_input(input, groups)
  })

  input_group_selections <- reactive({
    groups <- col_groups()
    plot_read_group_selections_from_input(input, groups)
  })

  observeEvent(input_group_selections(), {
    if (!use_shared_group_state) {
      return()
    }

    if (!isTRUE(is_module_active())) {
      return()
    }

    groups <- col_groups()
    if (!length(groups)) {
      if (!identical(group_state$selections, list())) {
        group_state$selections <- list()
      }
      return()
    }

    input_selections <- input_group_selections()
    current <- plot_sanitize_group_selections(group_state$selections, groups)
    if (!identical(input_selections, current)) {
      group_state$selections <- input_selections
    }
  }, ignoreInit = TRUE)

  observe({
    if (!use_shared_group_state) {
      return()
    }

    updateCheckboxInput(session, include_total_input_id, value = current_include_total())
  })

  list(
    use_shared_group_state = use_shared_group_state,
    selected_group_selections = selected_group_selections,
    get_combined_groups = get_combined_groups,
    set_combined_groups = set_combined_groups,
    current_include_total = current_include_total
  )
}
