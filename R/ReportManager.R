#' @title ReportManager Object
#' @description
#' This can be used to manage state of report, load and save them into file
#' It will create `shiny::reactiveVal` variables and load report list from `report_path`
#' @keywords internal
ReportManager <- R6::R6Class("ReportManager", # nolint: object_name_linter
  public = list(
    #' @field reports_path (`character(1)`)\cr
    #' An absolute path to a folder where reports are stored
    reports_path = "reports",

    #' @field current_report_title (`character(1)`)\cr
    #' `reactiveVal` field that stores current report title
    current_report_title = NULL,

    #' @field available_reports (`list()`)\cr
    #' `reactiveVal` field that stores a list of available reports
    available_reports = list(),

    #' @field my_locked_report (`character(1)`)\cr
    #' `reactiveVal` field that stores currently locked report title
    my_locked_report = NULL,

    #' @field read_only_mode (`logical(1)`)\cr
    #' `reactiveVal` field that indicates if current report is in read-only mode
    read_only_mode = NULL,

    #' @field session
    #' Placeholder for Shiny session object
    session = NULL,

    #' Initialize ReportManager
    #' @description
    #' This can be used to manage state of report, load and save them into file
    #' It will create `shiny::reactiveVal` variables and load report list from `reports_path`
    #' @param reports_path (character) An absolute path to where reports are stored
    #' @param session Session object passed from `moduleServer`
    #'
    #' @return ReportManager object
    initialize = function(reports_path, session) {
      self$reports_path <- reports_path
      if (!dir.exists(reports_path)) {
        dir.create(reports_path)
      }
      self$session <- session
      self$available_reports <- shiny::reactiveVal(list())
      self$current_report_title <- shiny::reactiveVal(NULL)
      self$read_only_mode <- shiny::reactiveVal(FALSE)
      self$list_reports()

      # Register cleanup if session is provided
      if (!is.null(session)) {
        private$register_cleanup()
      }
    },

    #' List available reports from objects' `reports_path`
    #'
    #' @return `data.frame` with fields `reports`, `created_by`, `locked_by`, and `last_rebuild`
    list_reports = function() {
      reports <- list.files(self$reports_path, full.names = FALSE)

      if (length(reports) == 0) {
        reports_df <- data.frame(
          reports = character(0),
          created_by = character(0),
          locked_by = character(0),
          last_rebuild = character(0),
          stringsAsFactors = FALSE
        )
      } else {
        lockfiles <- vapply(reports, function(x) {
          report_lockfile <- file.path(self$get_abs_report_path(x), ".lockfile.rds")
          if (file.exists(report_lockfile)) {
            val <- readRDS(report_lockfile)
            if (is.null(val)) NA_character_ else as.character(val)
          } else {
            NA_character_
          }
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)

        created_by <- vapply(reports, function(x) {
          creator_file <- file.path(self$get_abs_report_path(x), ".creator.rds")
          if (file.exists(creator_file)) {
            val <- readRDS(creator_file)
            if (is.null(val)) NA_character_ else as.character(val)
          } else {
            NA_character_
          }
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)

        last_rebuild <- vapply(reports, function(x) {
          rebuild_file <- file.path(self$get_abs_report_path(x), ".rebuild_time.rds")
          if (file.exists(rebuild_file)) {
            timestamp <- readRDS(rebuild_file)
            format(timestamp, "%Y-%m-%d %H:%M:%S")
          } else {
            "Never"
          }
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)

        reports_df <- data.frame(
          reports = reports,
          created_by = created_by,
          locked_by = lockfiles,
          last_rebuild = last_rebuild,
          stringsAsFactors = FALSE
        )
      }

      self$available_reports(reports_df)

      reports_df
    },

    #' Method for storing loaded report title inside object.
    #' This assigns a value in `current_report_title` field
    #'
    #' @param title_or_index (character/numeric) Title of report or an index from objects' `available_reports`
    set_current_report_title = function(title_or_index) {
      if (is.character(title_or_index)) {
        self$current_report_title(title_or_index)
      } else {
        self$current_report_title(self$available_reports()[title_or_index])
      }
    },

    #' Get absolute report path
    #'
    #' @param title (character) A title of report to get path of
    #'
    #' @return (character) A path of desired report
    get_abs_report_path = function(title) {
      paste0(self$reports_path, "/", title)
    },

    #' Save report with provided title
    #' This utilizes methods from `teal.reporter::Reporter` to store files required to load and save reports
    #' If a report with specific title exists, it will overwrite it.
    #' @param report_title (`character`) A title for the saved report.
    #' @param reporter (`Reporter`) A reporter object passed from `teal::init`
    #' @param first (`logical`) Whether this is the first time saving the report.
    #'
    save_report_files = function(report_title, reporter, first = FALSE) {
      if (!dir.exists(self$get_abs_report_path(report_title))) {
        dir.create(self$get_abs_report_path(report_title))
      }

      # Store creator information if not already saved
      private$save_creator(report_title)

      if (!self$is_locked_by_other(report_title)) {
        # Only save to JSON if there are cards
        if (length(reporter$get_cards()) > 0) {
          path <- self$get_abs_report_path(report_title)
          tryCatch(
            {
              reporter$set_id(report_title)
              reporter$to_jsondir(path)
              private$save_card_codes(reporter, path)
            },
            error = function(e) {
              showNotification(e$message, type = "error")
            }
          )
        }

        # Always update lock and current title
        self$current_report_title(report_title)
        if (!is.null(self$my_locked_report)) private$unlock_report(self$my_locked_report)
        private$lock_report(report_title)
        self$available_reports(self$list_reports())
      }
      NULL
    },

    #' Load report to report previewer
    #' This utilizes methods from `teal.reporter::Reporter` to load reports
    #'
    #' @param selected_title (`integer` or `character`) An index or a title of a report to load
    #' @param reporter (`Reporter`) A reporter object passed from `teal::init`
    #' @param skip_lock (`boolean`) Whether to skip locking a report or not
    load_report = function(selected_title, reporter, skip_lock = FALSE) {
      assertthat::assert_that(
        is.character(selected_title) | is.numeric(selected_title)
      )
      report_title <- ifelse(
        is.numeric(selected_title), self$available_reports()[selected_title, ][[1]], selected_title
      )
      report_dir <- self$get_abs_report_path(report_title)

      tryCatch(
        {
          # Change reporter ID and load saved report
          report_id <- jsonlite::read_json(file.path(report_dir, "Report.json"))$id

          reporter$set_id(as.character(report_id))
          reporter$from_jsondir(report_dir)

          # Only update state if loading succeeded
          if (!skip_lock) {
            if (!is.null(self$my_locked_report)) private$unlock_report(self$my_locked_report)

            if (!self$is_locked_by_other(report_title, message_type = "warning")) {
              private$lock_report(report_title)
              self$read_only_mode(FALSE) # Full access
            } else {
              self$read_only_mode(TRUE) # Read-only access
            }
            self$current_report_title(report_title)
          } else {
            # Skip lock mode is read-only
            self$read_only_mode(TRUE)
            self$current_report_title(report_title)
          }
        },
        error = function(e) {
          showNotification(e$message, type = "error")
        }
      )
    },

    #' Merge reports together
    #'
    #' @param x_title Base report title
    #' @param y_title Title of report to merge into x
    #' @param reporter A `TealReporter` object
    merge_reports = function(x_title, y_title, reporter) {
      y_rep <- teal.reporter::Reporter$new()

      x_dir <- self$get_abs_report_path(x_title)
      y_dir <- self$get_abs_report_path(y_title)

      # Check if Report.json exists for both reports
      if (!file.exists(file.path(x_dir, "Report.json"))) {
        stop(paste("Report", x_title, "has no saved content to merge"))
      }
      if (!file.exists(file.path(y_dir, "Report.json"))) {
        stop(paste("Report", y_title, "has no saved content to merge"))
      }

      self$load_report(x_title, x_rep, skip_lock = TRUE)
      self$load_report(y_title, y_rep, skip_lock = TRUE)

      reporter$append_cards(y_rep$get_cards())

      self$save_report_files(x_title, reporter)
    },

    #' Reset report manager
    #'
    #' @return the return of `self$list_reports()`
    reset = function() {
      if (!is.null(self$current_report_title()) & !is.null(self$my_locked_report)) {
        private$unlock_report(self$my_locked_report)
      }
      self$current_report_title(NULL)
      self$read_only_mode(FALSE)
      self$list_reports()
    },

    #' Delete report
    #' @param selected_title (integer or character) An index or a title of a report to load
    delete_report = function(selected_title) {
      assertthat::assert_that(
        is.character(selected_title) | is.numeric(selected_title)
      )

      if (is.numeric(selected_title)) {
        avail_reports <- self$available_reports()
        if (nrow(avail_reports) >= selected_title) {
          report_title <- as.character(avail_reports$reports[selected_title])
        } else {
          showNotification("Selected report index is out of range.", type = "error")
          return()
        }
      } else {
        report_title <- selected_title
      }

      # Validate report title
      if (is.null(report_title) || length(report_title) == 0 || !nzchar(report_title)) {
        showNotification("Invalid report title.", type = "error")
        return()
      }

      report_dir <- self$get_abs_report_path(report_title)

      if (!self$is_locked_by_other(report_title, verbose = FALSE)) {
        success <- tryCatch(
          {
            unlink(report_dir, recursive = TRUE)
            TRUE
          },
          error = function(e) {
            showNotification(paste("Error deleting report:", e$message), type = "error")
            FALSE
          }
        )

        if (success) {
          if (identical(self$current_report_title(), report_title)) {
            self$reset()
          }
          showNotification(paste("Successfully deleted report:", report_title), type = "message")
        }
      } else {
        showNotification(paste("Cannot delete locked report:", report_title), type = "error")
      }

      self$list_reports()
    },

    #' Checks if report is locked by another user
    #' @param report_title (character) A title for the saved report.
    #' @param verbose (boolean) Whether to show notification on locked report or not
    #' @param message_type (character) showNotification's type argument
    #'
    #' @return logical(1) TRUE if locked by another user, FALSE if unlocked or locked by me
    is_locked_by_other = function(report_title, verbose = TRUE, message_type = "error") {
      if (!is.null(self$my_locked_report) && self$my_locked_report == report_title) {
        return(FALSE)
      }
      if (file.exists(private$get_metadata_path(report_title, ".lockfile.rds"))) {
        lockfile <- readRDS(private$get_metadata_path(report_title, ".lockfile.rds"))
        if (verbose) {
          showNotification(paste(
            "This report is locked by:", lockfile,
            "Cannot save or delete report until unlocked."
          ), type = message_type)
        }
        return(TRUE)
      }
      FALSE
    },

    #' Rename report
    #' @param old_title (character) Current report title
    #' @param new_title (character) New report title
    #' @param reporter (Reporter) A reporter object
    rename_report = function(old_title, new_title, reporter) {
      old_path <- self$get_abs_report_path(old_title)
      new_path <- self$get_abs_report_path(new_title)

      if (!dir.exists(old_path)) {
        stop("Report directory does not exist")
      }

      if (dir.exists(new_path)) {
        stop("A report with that name already exists")
      }

      # Rename directory
      file.rename(old_path, new_path)

      # Update current title if this is the active report
      if (identical(self$current_report_title(), old_title)) {
        self$current_report_title(new_title)
        # Update reporter ID
        reporter$set_id(new_title)
        # Save updated ID to JSON
        reporter$to_jsondir(new_path)
      }

      self$list_reports()
    },

    #' Check if this would be a new report
    #' @param reporter (Reporter) A reporter object
    #' @return logical indicating if this is a new report
    is_new_report = function(reporter) {
      current_title <- self$current_report_title()
      has_cards <- length(reporter$get_cards()) > 0
      is.null(current_title) && has_cards
    },

    #' Create new report with title
    #' @param report_title (character) Title for the new report
    #' @param reporter (Reporter) A reporter object
    create_new_report = function(report_title, reporter) {
      if (is.null(report_title) || !nzchar(report_title)) {
        stop("Report title cannot be empty")
      }

      # Set the current report title and full access mode
      self$current_report_title(report_title)
      self$read_only_mode(FALSE)

      # Create report directory if it doesn't exist
      if (!dir.exists(self$get_abs_report_path(report_title))) {
        dir.create(self$get_abs_report_path(report_title), recursive = TRUE)
      }

      # Store creator information
      private$save_creator(report_title)

      # Save the report if it has cards
      if (length(reporter$get_cards()) > 0) {
        self$save_report_files(report_title, reporter)
      } else {
        # Even without cards, lock the report for the user
        if (!is.null(self$my_locked_report)) private$unlock_report(self$my_locked_report)
        private$lock_report(report_title)
        self$available_reports(self$list_reports())
      }
    },

    #' Add observeEvent to invoke auto save on reporter cards change
    #'
    #' @param reporter (Reporter) A reporter object passed from `teal::init`
    auto_save_observer = function(reporter) {
      shiny::observeEvent(reporter$get_cards(),
        {
          cards_count <- length(reporter$get_cards())
          current_title <- self$current_report_title()
          is_read_only <- self$read_only_mode()

          # If no current title but cards exist, trigger new report modal
          if (is.null(current_title) && cards_count > 0) {
            # Set a flag that a new report modal should be shown
            if (!is.null(self$session)) {
              self$session$sendCustomMessage("show_new_report_modal", list())
            }
            return()
          }

          # Reset if no cards
          if (!is.null(current_title) && cards_count == 0) {
            self$reset()
          }

          # Auto-save only if report already exists, not in read-only mode, and not locked by others
          if (!is.null(current_title) && cards_count > 0 && !is_read_only &&
            !self$is_locked_by_other(current_title, verbose = FALSE)) {
            # Guard against external uploads overwriting the active report:
            # If the reporter's ID no longer matches the current report title,
            # it means the reporter was loaded externally (e.g. via teal.reporter upload).
            # Reset the ReportManager to prevent saving uploaded content over the active report.
            reporter_id <- reporter$get_id()
            if (!identical(reporter_id, current_title)) {
              self$reset()
              return()
            }
            print("Report updated. Saving changes.")
            withProgress(message = "Saving report...", value = 0.3, {
              self$save_report_files(current_title, reporter)
              incProgress(0.7)
            })
          } else if (!is.null(current_title) && cards_count > 0 && is_read_only) {
            # Show warning if trying to modify read-only report
            showNotification("This report is in read-only mode. Changes cannot be saved.", type = "warning")
          }
        },
        ignoreInit = TRUE
      )
    },

    #' Unlock a locked report
    #' @param report_title (character) Title of the report to unlock
    unlock_report_public = function(report_title) {
      if (self$is_locked_by_other(report_title, verbose = FALSE)) {
        private$unlock_report(report_title)
        self$list_reports()
        showNotification(paste("Unlocked report:", report_title), type = "message")
      } else {
        showNotification(paste("Report is not locked:", report_title), type = "warning")
      }
    },

    #' Release lock on current report (keep report loaded but make it read-only)
    #' @param report_title (character) Title of the report to release lock from
    release_lock = function(report_title = NULL) {
      if (is.null(report_title)) {
        report_title <- self$current_report_title()
      }

      if (!is.null(report_title) && !is.null(self$my_locked_report) &&
        self$my_locked_report == report_title) {
        private$unlock_report(report_title)
        self$list_reports()
        showNotification(paste("Released lock on report:", report_title, "(now read-only)"), type = "message")
      } else {
        showNotification("No locked report to release", type = "warning")
      }
    },

    #' Re-build reports
    #' @description
    #' Rebuild reports to include data that has changed.
    #' This will replace loaded report cards with new cards, that were rebuilt from
    #' a code used to generate these cards.
    #' @param report_title (character) A title for the saved report.
    #' @param rp (Reporter) A reporter object passed from `teal::init`
    rebuild_report = function(report_title, rp) {
      reporter <- teal.reporter::Reporter$new()
      report_dir <- self$get_abs_report_path(report_title)
      report_id <- jsonlite::read_json(file.path(report_dir, "Report.json"))$id
      reporter$set_id(as.character(report_id))
      reporter$from_jsondir(report_dir)

      cards <- reporter$get_cards()
      code_file <- file.path(report_dir, "code.rds")
      card_codes <- if (file.exists(code_file)) readRDS(code_file) else list()

      for (i in seq_along(cards)) {
        card_name <- names(cards)[[i]]
        card <- cards[[i]]
        card_code <- if (i <= length(card_codes)) card_codes[[i]] else card[[length(card)]]

        # Remove teal's internal data-setup lines that can't be re-evaluated
        code_lines <- strsplit(card_code, "\n")[[1]]
        cleaned_lines <- code_lines[
          !grepl("^\\s*(stopifnot|lockEnvironment)\\(", code_lines) &
            !grepl("#\\s*@linksto\\b", code_lines) &
            !grepl("\\.raw_data", code_lines) &
            !grepl("\\[\\]", code_lines)
        ]
        cleaned <- paste(cleaned_lines, collapse = "\n")

        output <- tryCatch(
          eval(parse(text = cleaned)),
          error = function(e) {
            warning(sprintf("Failed to rebuild card '%s': %s", card_name, e$message))
            NULL
          }
        )

        if (!is.null(output)) {
          idx <- which(
            vapply(card, function(x) inherits(x, c("TableTree", "recordedplot", "chunk_output")), logical(1))
          )
          if (length(idx) > 0) {
            card[[idx[1]]] <- output
            reporter$replace_card(card = teal_card(card), card_id = card_name)
            reporter$to_jsondir(report_dir)
          }
        }
      }

      # Save rebuild timestamp
      saveRDS(Sys.time(), private$get_metadata_path(report_title, ".rebuild_time.rds"))

      # Reload rebuilt report if it's the same as the one loaded.
      if (!is.null(self$current_report_title()) && report_title == self$current_report_title()) {
        self$load_report(report_title, rp)
        print("Current report loaded")
      }
      print(paste0(report_title, ": Rebuild done"))
      self$list_reports()
    }
  ),
  private = list(
    #' Get current user name
    get_user_name = function() {
      user_name <- ifelse(interactive(), Sys.getenv("USER"), self$session$user)
      if (is.null(user_name) || user_name == "") user_name <- "Current User"
      user_name
    },

    #' Get metadata file path
    get_metadata_path = function(report_title, filename) {
      file.path(self$get_abs_report_path(report_title), filename)
    },

    #' Lock report so that it can't be overwritten by another user
    lock_report = function(report_title) {
      if (!self$is_locked_by_other(report_title)) {
        self$my_locked_report <- report_title
        saveRDS(
          object = private$get_user_name(),
          file = private$get_metadata_path(report_title, ".lockfile.rds")
        )
      }
    },

    #' Unlock report
    unlock_report = function(report_title) {
      if (!is.null(report_title) & dir.exists(self$get_abs_report_path(report_title))) {
        file.remove(private$get_metadata_path(report_title, ".lockfile.rds"))
        self$my_locked_report <- NULL
      }
    },

    #' Save creator information
    save_creator = function(report_title) {
      creator_file <- private$get_metadata_path(report_title, ".creator.rds")
      if (!file.exists(creator_file)) {
        saveRDS(object = private$get_user_name(), file = creator_file)
      }
    },

    #' Save code from each card as code.rds in the report directory
    save_card_codes = function(reporter, path) {
      cards <- reporter$get_cards()
      code_list <- lapply(cards, function(card) {
        idx <- which(vapply(card, function(x) inherits(x, "code_chunk"), logical(1)))
        if (length(idx) > 0) paste(vapply(card[idx], as.character, character(1)), collapse = "\n") else NA_character_
      })
      saveRDS(unname(code_list), file.path(path, "code.rds"))
    },

    #' Register `onSessionEnded` to unlock report when session is closed
    register_cleanup = function() {
      if (!is.null(self$session)) {
        self$session$onSessionEnded(function() {
          private$unlock_report(self$my_locked_report)
        })
      }
    }
  )
)
