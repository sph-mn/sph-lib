
/* code for creating a guile extension as a shared library.
   for features that can apparently not adequately be written in guile scheme,
   for example child process creation */

// set gnu source to include functions that arent part of the c standard (dirfd)
#define _GNU_SOURCE 1
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <libguile.h>
#include <linux/limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef OPEN_MAX
#define OPEN_MAX 1024
#endif

/* include sph-sc-lib sph/set.c to use for tracking file descriptors to keep
 * after fork */
#include <foreign/set.h>
sph_set_declare_type_nonull(fd_set, int, sph_set_hash_integer,
                            sph_set_equal_integer, 0, 2);
#define debug_log(format, ...)                                                 \
  fprintf(stderr, "%s:%d " format "\n", __func__, __LINE__, __VA_ARGS__)
#define move_fd(a)                                                             \
  do {                                                                         \
    a = dup(a);                                                                \
  } while ((errno == EINTR))
#define dup2_fd(old, new)                                                      \
  do {                                                                         \
    dup2(old, new);                                                            \
  } while ((errno == EINTR));                                                  \
  close(old)
#define close_fd(scm, a)                                                       \
  if (scm_is_false(scm)) {                                                     \
    close(a);                                                                  \
  }

/** variable integer null/path ->
     if "a" is -1, set it to a newly opened filed descriptor for path or
   /dev/null */
#define ensure_fd(a, open_flags, path)                                         \
  if (-1 == a) {                                                               \
    a = open((path ? path : "/dev/null"), open_flags);                         \
  }

/** SCM int-variable char*-variable ->
     set fd to a file descriptor from an SCM argument or -1.
     if "a" is a path string, set "path" */
#define port_argument_set_fd(a, fd, path)                                      \
  if (scm_is_true((scm_port_p(a)))) {                                          \
    fd = scm_to_int((scm_fileno(a)));                                          \
  } else {                                                                     \
    if (scm_is_integer(a)) {                                                   \
      fd = scm_to_int(a);                                                      \
    } else {                                                                   \
      fd = -1;                                                                 \
      if (scm_is_string(a)) {                                                  \
        path = scm_to_locale_string(a);                                        \
      };                                                                       \
    };                                                                         \
  }

/** integer integer integer ->
     to be called in a new process */
#define set_standard_streams(input, output, error)                             \
  if (input > 0) {                                                             \
    if (0 == output) {                                                         \
      move_fd(output);                                                         \
    };                                                                         \
    if (0 == error) {                                                          \
      move_fd(error);                                                          \
    };                                                                         \
    dup2_fd(input, 0);                                                         \
  };                                                                           \
  if (output > 1) {                                                            \
    if (1 == error) {                                                          \
      move_fd(error);                                                          \
    };                                                                         \
    dup2_fd(output, 1);                                                        \
  };                                                                           \
  if (error > 2) {                                                             \
    dup2_fd(error, 2);                                                         \
  }

/** integer fd-set ->
   try to close all used file descriptors greater than or equal to start-fd.
   tries to use one of /proc/{process-id}/fd, sysconf and getdtablesize.
   if none of those is available, closes file descriptors from start-fd
   to 1024 or OPEN_MAX if that is defined at compile time */
void close_file_descriptors_from(int start_fd, fd_set_t keep) {
  long fd;
  long maxfd;
  char path_proc_fd[PATH_MAX];
  DIR *directory;
  int path_length;
  struct dirent *entry;
  char *first_invalid;
  path_length = snprintf(path_proc_fd, (sizeof(path_proc_fd)), "/proc/%ld/fd",
                         ((long)(getpid())));
  if ((path_length > 0) && (((size_t)(path_length)) <= sizeof(path_proc_fd)) &&
      (directory = opendir(path_proc_fd))) {
    while (!(0 == (entry = readdir(directory)))) {
      fd = strtol((entry->d_name), (&first_invalid), 10);
      if (!(entry->d_name == first_invalid) && (*first_invalid == 0) &&
          (fd >= 0) && (fd < INT_MAX) && (fd >= start_fd) &&
          !(fd == dirfd(directory)) && !fd_set_get(keep, fd)) {
        ((void)(close(((int)(fd)))));
      };
    };
    ((void)(closedir(directory)));
  } else {
/* fallback */
#if HAVE_SYSCONF
    maxfd = sysconf(_SC_OPEN_MAX);
#else
    maxfd = getdtablesize();
#endif
    if (maxfd < 0) {
      maxfd = OPEN_MAX;
    };
    fd = start_fd;
    while ((fd <= maxfd)) {
      if (!fd_set_get(keep, fd)) {
        ((void)(close(((int)(fd)))));
      };
      fd = (1 + fd);
    };
  };
}

/** free a null pointer terminated char** */
void free_env(char **a) {
  char **a_temp = a;
  while (a_temp) {
    free(a_temp);
  };
  free(a);
}
unsigned char scm_is_null_scm(SCM a) {
  return ((scm_is_true(
      (scm_call_1((scm_variable_ref((scm_c_lookup("null?")))), a)))));
}
int scm_list_to_fd_set(SCM scm_a, fd_set_t *out) {
  fd_set_t result;
  int a_length = scm_to_uint32((scm_length(scm_a)));
  if (fd_set_new((3 + a_length), (&result))) {
    return (1);
  };
  while (!scm_is_null_scm(scm_a)) {
    if (!fd_set_add(result, (scm_to_int((SCM_CAR(scm_a)))))) {
      fd_set_free(result);
      return (2);
    };
    scm_a = SCM_CDR(scm_a);
  };
  *out = result;
  return (0);
}

/** returns a null pointer terminated char** */
char **scm_string_list_to_string_pointer_array(SCM scm_a) {
  int a_length = scm_to_int((scm_length(scm_a)));
  char **result = malloc((sizeof(char *) * (1 + a_length)));
  result[a_length] = 0;
  char **result_pointer = result;
  while (!scm_is_null_scm(scm_a)) {
    char *b;
    size_t b_length;
    b = scm_to_locale_stringn((SCM_CAR(scm_a)), (&b_length));
    *result_pointer = malloc(((1 + b_length) * sizeof(char)));
    memcpy((*result_pointer), b, b_length);
    (*result_pointer)[b_length] = 0;
    result_pointer = (1 + result_pointer);
    scm_a = SCM_CDR(scm_a);
  };
  return (result);
}
SCM scm_primitive_process_create(SCM scm_executable, SCM scm_arguments,
                                 SCM scm_input_port, SCM scm_output_port,
                                 SCM scm_error_port, SCM scm_env,
                                 SCM scm_keep_descriptors,
                                 SCM scm_path_open_flags) {
  int path_open_flags =
      (scm_is_true(scm_path_open_flags) ? scm_to_int(scm_path_open_flags) : 0);
  char **arguments = scm_string_list_to_string_pointer_array(scm_arguments);
  char **env = (scm_is_false(scm_env)
                    ? environ
                    : scm_string_list_to_string_pointer_array(scm_env));
  char *executable = scm_to_locale_string(scm_executable);
  fd_set_t keep;
  int input;
  int output;
  int error;
  char *input_path = 0;
  char *output_path = 0;
  char *error_path = 0;
  port_argument_set_fd(scm_input_port, input, input_path);
  port_argument_set_fd(scm_output_port, output, output_path);
  port_argument_set_fd(scm_error_port, error, error_path);
  if (scm_list_to_fd_set(scm_keep_descriptors, (&keep))) {
    return (SCM_BOOL_F);
  };
  int process_id = fork();
  if (!(0 == process_id)) {
    free(arguments);
    free(executable);
    fd_set_free(keep);
    if (scm_is_true(scm_env)) {
      free_env(env);
    };
    return ((scm_from_int(process_id)));
  };
  /* after fork */
  ensure_fd(input, O_RDONLY, input_path);
  ensure_fd(output, (O_WRONLY | O_CREAT | path_open_flags), output_path);
  ensure_fd(error, (O_WRONLY | O_CREAT | path_open_flags), error_path);
  fd_set_add(keep, input);
  fd_set_add(keep, output);
  fd_set_add(keep, error);
  close_file_descriptors_from(3, keep);
  set_standard_streams(input, output, error);
  execve(executable, arguments, env);
  /* terminates the program immediately with neither scheme-level nor c-level
   * cleanups */
  _exit(127);
  return (SCM_UNSPECIFIED);
}
void init_sph_lib() {
  SCM primitive_process_create = scm_c_define_gsubr(
      "primitive-process-create", 8, 0, 0, scm_primitive_process_create);
  scm_set_procedure_property_x(
      primitive_process_create, (scm_from_locale_symbol("documentation")),
      (scm_from_locale_string(
          ("string (string ...) false/port/string/integer "
           "false/port/string/integer false/port/string/integer false/(string "
           "...) (integer ...) false/integer -> false/integer\n      "
           "executable (argument ...) input output error environ-result "
           "keep-file-descriptors path-open-flags -> child-process-id\n      "
           "values for input, output or error:\n      * false: /dev/null\n     "
           " * string: filesystem path\n      * integer: file descriptor\n     "
           " * port: port\n      creates a child process via an async safe "
           "fork/exec.\n      uses execve and does not search in directories "
           "of the PATH environment variable"))));
}
