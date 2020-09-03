# file_path_sans_ext <- function(x){
#   sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
# }
#
#
# #' @export
# #' @param name The name of the module.
# #' @param pkg Path to the root of the package. Default is `getwd()`.
# #' @param open Should the file be opened?
# #' @param dir_create Creates the directory if it doesn't exist, default is `TRUE`.
# #' @param dir Path to the dir where the file while be created.
# #' @param initialize Whether to add the initialize method. Default to FALSE. Some JavaScript API
# #' require to initialize components before using them.
# #' @param dev Whether to insert console.log calls in the most important methods of the binding.
# #' This is only to help building the input binding. Default to FALSE
# #' @param events List of events to generate event listeners in the subscribe method. For instance,
# #' \code{list(name = c("click", "keyup"), rate_policy = c(FALSE, TRUE))}.
# #' The list contain names and rate policies to apply to each event. If a rate policy is found,
# #' the debounce method with a default delay of 250 ms is applied. You may edit manually according to
# #' \url{https://shiny.rstudio.com/articles/building-inputs.html}.
# #' @importFrom fs path_abs path file_create file_exists
# create_input_binding <- function(
#   name,
#   pkg = getwd(),
#   dir = "inst/custom/js",
#   open = TRUE,
#   dir_create = TRUE,
#   initialize = FALSE,
#   dev = FALSE,
#   events = list(
#     name = "click",
#     rate_policy = FALSE
#   )
# ){
#   attempt::stop_if(
#     rlang::is_missing(name),
#     msg = "Name is required"
#   )
#
#   attempt::stop_if(
#     length(events$name) == 0,
#     msg = "At least one event is required"
#   )
#
#   attempt::stop_if(
#     length(events$name) != length(events$rate_policy),
#     msg = "Incomplete events list"
#   )
#
#   name <- file_path_sans_ext(name)
#
#   old <- setwd(path_abs(pkg))
#   on.exit(setwd(old))
#
#   dir_created <- create_if_needed(
#     dir, type = "directory"
#   )
#
#   if (!dir_created){
#     cat_red_bullet(
#       "File not added (needs a valid directory)"
#     )
#     return(invisible(FALSE))
#   }
#
#   dir <- path_abs(dir)
#
#   where <- file.path(
#     dir, sprintf("%s.js", name)
#   )
#
#   if (!file.exists(where)){
#
#     file_create(where)
#
#     write_there <- function(...){
#       write(..., file = where, append = TRUE)
#     }
#
#     # If we find at least 1 event with a rate policy, we allow
#     # the getRatePolicy method
#     global_rate_policy <- sum(sapply(events$rate_policy, `[[`, 1)) > 0
#
#     write_there(sprintf("var %s = new Shiny.InputBinding();", name))
#     write_there(sprintf("$.extend(%s, {", name))
#     # find
#     write_there("  find: function(scope) {")
#     write_there("    // JS logic $(scope).find('whatever')")
#     write_there("  },")
#     # initialize
#     if (initialize) {
#       write_there("  initialize: function(el) {")
#       write_there("    // optional part. Only if the input relies on a JS API with specific initialization.")
#       write_there("  },")
#     }
#     # get value
#     write_there("  getValue: function(el) {")
#     if (dev) write_there("    console.log($(el));")
#     write_there("    // JS code to get value")
#     write_there("  },")
#     # set value
#     write_there("  setValue: function(el, value) {")
#     if (dev) write_there("    console.log('New value is: ' + value);")
#     write_there("    // JS code to set value")
#     write_there("  },")
#     # receive
#     write_there("  receiveMessage: function(el, data) {")
#     write_there("    // this.setValue(el, data);")
#     if (dev) write_there("    console.log('Updated ...');")
#     write_there("  },")
#     # subscribe
#     write_there("  subscribe: function(el, callback) {")
#     # list of event listeners
#     lapply(seq_along(events$name), function(i) {
#       write_there(sprintf("    $(el).on('%s.%s', function(e) {", events$name[i], name))
#       if (events$rate_policy[i]) {
#         write_there("      callback(true);")
#       } else {
#         write_there("      callback();")
#       }
#       if (dev) write_there("      console.log('Subscribe ...');")
#       write_there("    });")
#       write_there("")
#     })
#     write_there("  },")
#
#     # rate policy if any
#     if (global_rate_policy) {
#       write_there("  getRatePolicy: function() {")
#       write_there("    return {")
#       write_there("      policy: 'debounce',")
#       write_there("      delay: 250")
#       write_there("    };")
#       write_there("  },")
#     }
#
#     # unsubscribe
#     write_there("  unsubscribe: function(el) {")
#     write_there(sprintf("    $(el).off('.%s');", name))
#     write_there("  }")
#
#     # end
#     write_there("});")
#     write_there(sprintf("Shiny.inputBindings.register(%s, 'shiny.whatever');", name))
#
#
#     file_created_dance(
#       where,
#       after_creation_message_js,
#       pkg,
#       dir,
#       name,
#       open_file = open
#     )
#   } else {
#     file_already_there_dance(
#       where,
#       open_file = open
#     )
#   }
#
#
#
# }
