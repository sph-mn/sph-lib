(library (sph scrypt)
  (export
    scrypt
    scrypt-defaults
    scrypt-check
    scrypt->string)
  (import
    (rnrs base)
    (only (guile) dynamic-link dynamic-func inexact->exact)
    (system foreign)
    (sph)
    (rnrs bytevectors))

(define libscrypt (dynamic-link "libscrypt"))

(define foreign-scrypt->string
  (pointer->procedure uint32
    (dynamic-func "scrypt_to_string_base91" libscrypt)
    (list (q *) size_t (q *) size_t uint64 uint32 uint32 size_t (q *) (q *))))

(define foreign-scrypt
  (pointer->procedure uint32 (dynamic-func "scrypt" libscrypt)
    (list (q *) size_t (q *) size_t uint64 uint32 uint32 (q *) size_t)))

(define foreign-scrypt-set-defaults
  (pointer->procedure uint32 (dynamic-func "scrypt_set_defaults" libscrypt) (q (* * * * * *))))

(define pointer-size (sizeof (q *)))

(define* (scrypt->string password #:optional salt (size 0) (N 0) (r 0) (p 0))
  (let
    ( (res (bytevector->pointer (make-bytevector pointer-size 0)))
      (res-len (make-bytevector pointer-size 0)))
    (foreign-scrypt->string
      (bytevector->pointer password)
      (bytevector-length password)
      (if salt (bytevector->pointer salt) %null-pointer)
      (if salt (bytevector-length salt) 0)
      N r p size res (bytevector->pointer res-len))
    (pointer->string (dereference-pointer res)
      (bytevector-uint-ref res-len 0 (endianness little) pointer-size))))

(define (scrypt-defaults)
  (let
    ( (salt (bytevector->pointer (make-bytevector pointer-size)))
      (salt-length (make-bytevector pointer-size))
      (size (make-bytevector (sizeof size_t)))
      (N (make-bytevector (sizeof uint64)))
      (r (make-bytevector (sizeof uint32)))
      (p (make-bytevector (sizeof uint32))))
    (apply foreign-scrypt-set-defaults salt (map bytevector->pointer (list salt-length size N r p)))
    (list
      (pointer->bytevector (dereference-pointer salt) (bytevector-uint-ref salt-length 0 (endianness little) (sizeof size_t)))
      (bytevector-uint-ref size 0 (endianness little) (sizeof size_t))
      (inexact->exact (/ (log (bytevector-u64-ref N 0 (endianness little))) (log 2)))
      (bytevector-u32-ref r 0 (endianness little))
      (bytevector-u32-ref p 0 (endianness little)))))

(define (scrypt password salt size logN r p)
  (let (res (make-bytevector size 0))
    (foreign-scrypt
      (bytevector->pointer password)
      (bytevector-length password)
      (bytevector->pointer salt)
      (bytevector-length salt)
      (expt 2 logN) r p (bytevector->pointer res) size)
    res))

(define (scrypt-check key . args) (bytevector=? key (apply scrypt args))))
