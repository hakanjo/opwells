create_group_selection_state <- function() {
  reactiveValues(
    selections = list(),
    combined_groups = list(),
    include_total = FALSE,
    reference_groups = character(0)
  )
}

group_selection_controller <- function(
  input,
  session,
  col_groups,
  ref_group_choices = NULL,
  group_state = NULL,
  active_tab = NULL,
  tab_value = NULL,
  include_total_input_id = "include_total",
  reference_groups_input_id = "reference_groups"
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

  current_include_total <- reactive({
    if (!use_shared_group_state) {
      return(isTRUE(input[[include_total_input_id]]))
    }

    isTRUE(group_state$include_total)
  })

  current_reference_groups <- reactive({
    choices <- if (is.null(ref_group_choices)) {
      character(0)
    } else {
      unique(plot_trim_non_empty(ref_group_choices()))
    }

    if (!length(choices)) {
      return(character(0))
    }

    selected <- if (use_shared_group_state) {
      group_state$reference_groups
    } else {
      input[[reference_groups_input_id]]
    }

    selected <- unique(plot_trim_non_empty(selected))
    selected[selected %in% choices]
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

  observeEvent(input[[reference_groups_input_id]], {
    if (!use_shared_group_state) {
      return()
    }

    if (!isTRUE(is_module_active())) {
      return()
    }

    choices <- if (is.null(ref_group_choices)) {
      character(0)
    } else {
      unique(plot_trim_non_empty(ref_group_choices()))
    }

    selected <- unique(plot_trim_non_empty(input[[reference_groups_input_id]]))
    selected <- selected[selected %in% choices]

    current <- unique(plot_trim_non_empty(group_state$reference_groups))
    current <- current[current %in% choices]

    if (!identical(current, selected)) {
      group_state$reference_groups <- selected
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

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

    choices <- if (is.null(ref_group_choices)) {
      character(0)
    } else {
      unique(plot_trim_non_empty(ref_group_choices()))
    }

    selected <- current_reference_groups()
    current_input <- isolate(input[[reference_groups_input_id]])

    current_norm <- sort(unique(plot_trim_non_empty(current_input)))
    selected_norm <- sort(unique(plot_trim_non_empty(selected)))

    if (!identical(current_norm, selected_norm)) {
      freezeReactiveValue(input, reference_groups_input_id)
      updateSelectizeInput(
        session,
        reference_groups_input_id,
        choices = choices,
        selected = selected,
        server = TRUE
      )
    }
  })

  list(
    use_shared_group_state = use_shared_group_state,
    selected_group_selections = selected_group_selections,
    get_combined_groups = get_combined_groups,
    current_include_total = current_include_total,
    current_reference_groups = current_reference_groups
  )
}
