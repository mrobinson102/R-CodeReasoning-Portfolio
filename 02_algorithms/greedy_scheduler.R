#' Greedy Job Scheduler by Earliest Deadline
#' @param jobs data.frame with columns 'id' and 'deadline' (integer time slots)
#' @return integer vector of selected job ids in scheduled order
greedy_schedule <- function(jobs) {
  stopifnot(all(c("id", "deadline") %in% names(jobs)))
  jobs <- jobs[order(jobs$deadline), ]
  used <- integer(0)
  current <- 0
  for (i in seq_len(nrow(jobs))) {
    if (current < jobs$deadline[i]) {
      used <- c(used, jobs$id[i])
      current <- current + 1
    }
  }
  used
}
