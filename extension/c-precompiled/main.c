/* code for creating a guile extension as a shared library.
  for features that can apparently not adequately be written in guile scheme,
  for example child process creation */
/* set gnu source to include functions that arent part of the c standard (dirfd)
 */
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
#define imht_set_key_t int
#define imht_set_can_contain_zero_p 0
/* include imht-set to use for tracking file descriptors to keep after fork */
#include <inttypes.h>
#include <stdlib.h>
#ifndef imht_set_key_t
#define imht_set_key_t uint64_t
#endif
#ifndef imht_set_can_contain_zero_p
#define imht_set_can_contain_zero_p 1
#endif
#ifndef imht_set_size_factor
#define imht_set_size_factor 2
#endif
uint16_t imht_set_primes[] = {
    0,   3,   7,   13,  19,  29,  37,  43,  53,  61,  71,  79,  89,  101, 107,
    113, 131, 139, 151, 163, 173, 181, 193, 199, 223, 229, 239, 251, 263, 271,
    281, 293, 311, 317, 337, 349, 359, 373, 383, 397, 409, 421, 433, 443, 457,
    463, 479, 491, 503, 521, 541, 557, 569, 577, 593, 601, 613, 619, 641, 647,
    659, 673, 683, 701, 719, 733, 743, 757, 769, 787, 809, 821, 827, 839, 857,
    863, 881, 887, 911, 929, 941, 953, 971, 983, 997};
uint16_t *imht_set_primes_end = (imht_set_primes + 83);
typedef struct {
  size_t size;
  imht_set_key_t *content;
} imht_set_t;
size_t imht_set_calculate_hash_table_size(size_t min_size) {
  min_size = (imht_set_size_factor * min_size);
  uint16_t *primes = imht_set_primes;
  while ((primes < imht_set_primes_end)) {
    if (min_size <= *primes) {
      return ((*primes));
    } else {
      primes = (1 + primes);
    };
  };
  if (min_size <= *primes) {
    return ((*primes));
  };
  return ((1 | min_size));
}
uint8_t imht_set_create(size_t min_size, imht_set_t **result) {
  *result = malloc((sizeof(imht_set_t)));
  if (!*result) {
    return (0);
  };
  min_size = imht_set_calculate_hash_table_size(min_size);
  (*(*result)).content = calloc(min_size, (sizeof(imht_set_key_t)));
  (*(*result)).size = min_size;
  return (((*result)->content ? 1 : 0));
}
void imht_set_destroy(imht_set_t *a) {
  if (a) {
    free((a->content));
    free(a);
  };
}
#if imht_set_can_contain_zero_p
#define imht_set_hash(value, hash_table)                                       \
  (value ? (1 + (value % (hash_table.size - 1))) : 0)
#else
#define imht_set_hash(value, hash_table) (value % hash_table.size)
#endif
/** returns the address of the element in the set, 0 if it was not found.
  caveat: if imht-set-can-contain-zero? is defined, which is the default,
  pointer-geterencing a returned address for the found value 0 will return 1
  instead */
imht_set_key_t *imht_set_find(imht_set_t *a, imht_set_key_t value) {
  imht_set_key_t *h = (a->content + imht_set_hash(value, (*a)));
  if (*h) {
#if imht_set_can_contain_zero_p
    if ((*h == value) || (0 == value)) {
      return (h);
    };
#else
    if (*h == value) {
      return (h);
    };
#endif
    imht_set_key_t *content_end = (a->content + (a->size - 1));
    imht_set_key_t *h2 = (1 + h);
    while ((h2 < content_end)) {
      if (!*h2) {
        return (0);
      } else {
        if (value == *h2) {
          return (h2);
        };
      };
      h2 = (1 + h2);
    };
    if (!*h2) {
      return (0);
    } else {
      if (value == *h2) {
        return (h2);
      };
    };
    h2 = a->content;
    while ((h2 < h)) {
      if (!*h2) {
        return (0);
      } else {
        if (value == *h2) {
          return (h2);
        };
      };
      h2 = (1 + h2);
    };
  };
  return (0);
}
#define imht_set_contains_p(a, value) ((0 == imht_set_find(a, value)) ? 0 : 1)
/** returns 1 if the element was removed, 0 if it was not found */
uint8_t imht_set_remove(imht_set_t *a, imht_set_key_t value) {
  imht_set_key_t *value_address = imht_set_find(a, value);
  if (value_address) {
    *value_address = 0;
    return (1);
  } else {
    return (0);
  };
}
/** returns the address of the added or already included element, 0 if there is
 * no space left in the set */
imht_set_key_t *imht_set_add(imht_set_t *a, imht_set_key_t value) {
  imht_set_key_t *h = (a->content + imht_set_hash(value, (*a)));
  if (*h) {
#if imht_set_can_contain_zero_p
    if ((value == *h) || (0 == value)) {
      return (h);
    };
#else
    if (value == *h) {
      return (h);
    };
#endif
    imht_set_key_t *content_end = (a->content + (a->size - 1));
    imht_set_key_t *h2 = (1 + h);
    while (((h2 <= content_end) && *h2)) {
      h2 = (1 + h2);
    };
    if (h2 > content_end) {
      h2 = a->content;
      while (((h2 < h) && *h2)) {
        h2 = (1 + h2);
      };
      if (h2 == h) {
        return (0);
      } else {
#if imht_set_can_contain_zero_p
        *h2 = ((0 == value) ? 1 : value);
#else
        *h2 = value;
#endif
      };
    } else {
#if imht_set_can_contain_zero_p
      *h2 = ((0 == value) ? 1 : value);
#else
      *h2 = value;
#endif
    };
  } else {
#if imht_set_can_contain_zero_p
    *h = ((0 == value) ? 1 : value);
#else
    *h = value;
#endif
    return (h);
  };
}
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
  if (scm == SCM_BOOL_F) {                                                     \
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
/** integer imht-set ->
  try to close all used file descriptors greater than or equal to start-fd.
  tries to use one of /proc/{process-id}/fd, sysconf and getdtablesize.
  if none of those is available, closes file descriptors from sart-fd
  to 1024 or OPEN_MAX if that as defined at compile time */
void close_file_descriptors_from(int start_fd, imht_set_t *keep) {
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
          !(fd == dirfd(directory)) && !imht_set_contains_p(keep, fd)) {
        ((void)(close(((int)(fd)))));
      };
    };
    ((void)(closedir(directory)));
  } else {
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
      if (!imht_set_contains_p(keep, fd)) {
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
int scm_list_to_imht_set(SCM scm_a, imht_set_t **result) {
  int a_length = scm_to_uint32((scm_length(scm_a)));
  if (!imht_set_create((3 + a_length), result)) {
    return (1);
  };
  while (!scm_is_null(scm_a)) {
    if (!imht_set_add((*result), (scm_to_int((SCM_CAR(scm_a)))))) {
      imht_set_destroy((*result));
      return (2);
    };
    scm_a = SCM_CDR(scm_a);
  };
  return (0);
}
/** returns a null pointer terminated char** */
char **scm_string_list_to_string_pointer_array(SCM scm_a) {
  int a_length = scm_to_int((scm_length(scm_a)));
  char **result = malloc((sizeof(char *) * (1 + a_length)));
  result[a_length] = 0;
  char **result_pointer = result;
  while (!scm_is_null(scm_a)) {
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
/** see init-sph-lib for documentation */
SCM scm_primitive_process_create(SCM scm_executable, SCM scm_arguments,
                                 SCM scm_input_port, SCM scm_output_port,
                                 SCM scm_error_port, SCM scm_env,
                                 SCM scm_keep_descriptors,
                                 SCM scm_path_open_flags) {
  int path_open_flags =
      (scm_is_true(scm_path_open_flags) ? scm_to_int(scm_path_open_flags) : 0);
  char **arguments = scm_string_list_to_string_pointer_array(scm_arguments);
  char **env = ((SCM_BOOL_F == scm_env)
                    ? environ
                    : scm_string_list_to_string_pointer_array(scm_env));
  char *executable = scm_to_locale_string(scm_executable);
  imht_set_t *keep;
  int input;
  int output;
  int error;
  char *input_path = 0;
  char *output_path = 0;
  char *error_path = 0;
  port_argument_set_fd(scm_input_port, input, input_path);
  port_argument_set_fd(scm_output_port, output, output_path);
  port_argument_set_fd(scm_error_port, error, error_path);
  if (scm_list_to_imht_set(scm_keep_descriptors, (&keep))) {
    return (SCM_BOOL_F);
  };
  int process_id = fork();
  if (!(0 == process_id)) {
    free(arguments);
    free(executable);
    imht_set_destroy(keep);
    if (!(SCM_BOOL_F == scm_env)) {
      free_env(env);
    };
    return ((scm_from_int(process_id)));
  };
  ensure_fd(input, O_RDONLY, input_path);
  ensure_fd(output, (O_WRONLY | O_CREAT | path_open_flags), output_path);
  ensure_fd(error, (O_WRONLY | O_CREAT | path_open_flags), error_path);
  imht_set_add(keep, input);
  imht_set_add(keep, output);
  imht_set_add(keep, error);
  close_file_descriptors_from(3, keep);
  set_standard_streams(input, output, error);
  execve(executable, arguments, env);
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
