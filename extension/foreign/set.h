
#ifndef sph_set_h
#define sph_set_h

/* a macro that defines set data types and related functions for arbitrary value types.
 * compared to hashtable.c, this uses less than half the space and operations are faster (about 20% in first tests)
 * linear probing for collision resolve
 * sph-set-declare-type allows the null value (used for unset elements) to be part of the set
 * except for the null value, values are in field .values starting from index 1
 * notnull is used at index 0 to check if the empty-value is included
 * sph-set-declare-type-nonull does not support the null value to be part of the set and should be a bit faster
 * values are in .values, starting from index 0
 * null and notnull arguments are user provided so that they have the same data type as other set elements
 * primes from https://planetmath.org/goodhashtableprimes
 * automatic resizing is not implemented. resizing can be done by re-inserting each value into a larger set */
#include <stdlib.h>
#include <inttypes.h>
uint32_t sph_set_primes[] = { 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741 };
uint32_t* sph_set_primes_end = (sph_set_primes + 25);
#define sph_set_hash_integer(value, hashtable_size) (value % hashtable_size)
#define sph_set_equal_integer(value_a, value_b) (value_a == value_b)
#define sph_set_declare_type_shared_1(name, value_type, set_hash, set_equal, null, size_factor) \
  typedef struct { \
    size_t size; \
    value_type* values; \
  } name##_t; \
  size_t name##_calculate_size(size_t min_size) { \
    min_size = (size_factor * min_size); \
    uint32_t* primes; \
    for (primes = sph_set_primes; (primes <= sph_set_primes_end); primes += 1) { \
      if (min_size <= *primes) { \
        return ((*primes)); \
      }; \
    }; \
    /* if no prime has been found, make size at least an odd number */ \
    return ((1 | min_size)); \
  } \
  void name##_clear(name##_t a) { \
    size_t i; \
    for (i = 0; (i < a.size); i = (1 + i)) { \
      (a.values)[i] = null; \
    }; \
  } \
  void name##_free(name##_t a) { free((a.values)); }
#define sph_set_declare_type_shared_2(name, value_type, set_hash, set_equal, null, size_factor) \
  /** returns 0 if the element was removed, 1 if it was not found */ \
  uint8_t name##_remove(name##_t a, value_type value) { \
    value_type* v = name##_get(a, value); \
    if (v) { \
      *v = null; \
      return (0); \
    } else { \
      return (1); \
    }; \
  }
#define sph_set_declare_type_with_null(name, value_type, set_hash, set_equal, null, notnull, size_factor) \
  /** returns 0 on success or 1 if the memory allocation failed */ \
  uint8_t name##_new(size_t min_size, name##_t* result) { \
    name##_t temp; \
    temp.size = (1 + name##_calculate_size(min_size)); \
    temp.values = calloc((temp.size), (sizeof(value_type))); \
    if (!temp.values) { \
      return (1); \
    }; \
    name##_clear(temp); \
    *result = temp; \
    return (0); \
  } \
\
  /** returns the address of the value or 0 if it was not found. \
         if value is the null value and exists, then address points to the notnull value */ \
  value_type* name##_get(name##_t a, value_type value) { \
    size_t i; \
    size_t hash_i; \
    if (set_equal(null, value)) { \
      return ((set_equal(notnull, (*(a.values))) ? a.values : 0)); \
    }; \
    hash_i = (1 + set_hash(value, (a.size - 1))); \
    i = hash_i; \
    while ((i < a.size)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        return (0); \
      } else { \
        if (set_equal(value, ((a.values)[i]))) { \
          return ((i + a.values)); \
        }; \
      }; \
      i += 1; \
    }; \
    /* wraps over */ \
    i = 1; \
    while ((i < hash_i)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        return (0); \
      } else { \
        if (set_equal(value, ((a.values)[i]))) { \
          return ((i + a.values)); \
        }; \
      }; \
      i += 1; \
    }; \
    return (0); \
  } \
\
  /** add if not already exists. returns the address of the existing or new value or 0 if no space is left */ \
  value_type* name##_add(name##_t a, value_type value) { \
    size_t i; \
    size_t hash_i; \
    if (set_equal(null, value)) { \
      *(a.values) = notnull; \
      return ((a.values)); \
    }; \
    hash_i = (1 + set_hash(value, (a.size - 1))); \
    i = hash_i; \
    while ((i < a.size)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        if (!set_equal(value, ((a.values)[i]))) { \
          (a.values)[i] = value; \
        }; \
        return ((i + a.values)); \
      }; \
      i += 1; \
    }; \
    /* wraps over */ \
    i = 1; \
    while ((i < hash_i)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        if (!set_equal(value, ((a.values)[i]))) { \
          (a.values)[i] = value; \
        }; \
        return ((i + a.values)); \
      }; \
      i += 1; \
    }; \
    return (0); \
  }
#define sph_set_declare_type_without_null(name, value_type, set_hash, set_equal, null, size_factor) \
  /** returns 0 on success or 1 if the memory allocation failed */ \
  uint8_t name##_new(size_t min_size, name##_t* result) { \
    value_type* values; \
    min_size = name##_calculate_size(min_size); \
    values = calloc(min_size, (sizeof(value_type))); \
    if (!values) { \
      return (1); \
    }; \
    (*result).values = values; \
    (*result).size = min_size; \
    return (0); \
  } \
\
  /** returns the address of the value or 0 if it was not found */ \
  value_type* name##_get(name##_t a, value_type value) { \
    size_t i; \
    size_t hash_i; \
    hash_i = set_hash(value, (a.size)); \
    i = hash_i; \
    while ((i < a.size)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        return (0); \
      } else { \
        if (set_equal(value, ((a.values)[i]))) { \
          return ((i + a.values)); \
        }; \
      }; \
      i += 1; \
    }; \
    /* wraps over */ \
    i = 0; \
    while ((i < hash_i)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        return (0); \
      } else { \
        if (set_equal(value, ((a.values)[i]))) { \
          return ((i + a.values)); \
        }; \
      }; \
      i += 1; \
    }; \
    return (0); \
  } \
\
  /** returns the address of the value or 0 if no space is left */ \
  value_type* name##_add(name##_t a, value_type value) { \
    size_t i; \
    size_t hash_i; \
    hash_i = set_hash(value, (a.size)); \
    i = hash_i; \
    while ((i < a.size)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        (a.values)[i] = value; \
        return ((i + a.values)); \
      }; \
      i += 1; \
    }; \
    /* wraps over */ \
    i = 0; \
    while ((i < hash_i)) { \
      if (set_equal(null, ((a.values)[i]))) { \
        (a.values)[i] = value; \
        return ((i + a.values)); \
      }; \
      i += 1; \
    }; \
    return (0); \
  }
#define sph_set_declare_type(name, value_type, hash, equal, null, notnull, size_factor) sph_set_declare_type_shared_1(name, value_type, hash, equal, null, size_factor) \
  sph_set_declare_type_with_null(name, value_type, hash, equal, null, notnull, size_factor) \
    sph_set_declare_type_shared_2(name, value_type, hash, equal, null, size_factor)
#define sph_set_declare_type_nonull(name, value_type, hash, equal, null, size_factor) sph_set_declare_type_shared_1(name, value_type, hash, equal, null, size_factor) \
  sph_set_declare_type_without_null(name, value_type, hash, equal, null, size_factor) \
    sph_set_declare_type_shared_2(name, value_type, hash, equal, null, size_factor)
#endif
