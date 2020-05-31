(define-module (sph scrypt))

(use-modules (rnrs bytevectors) (sph)
  (system foreign) ((guile) #:select (dynamic-link dynamic-func inexact->exact)))

(export scrypt scrypt->string scrypt-check scrypt-defaults sph-scrypt-description)

(define sph-scrypt-description
  "use the scrypt key derivation function. depends on https://github.com/jkalbhenn/scrypt")

(define libscrypt (dynamic-link "libscrypt"))

(define foreign-scrypt->string
  (pointer->procedure int (dynamic-func "scrypt_to_string_base91" libscrypt)
    (list (q *) size_t (q *) size_t uint64 uint32 uint32 size_t (q *) (q *))))

(define foreign-scrypt
  (pointer->procedure int (dynamic-func "scrypt" libscrypt)
    (list (q *) size_t (q *) size_t uint64 uint32 uint32 (q *) size_t)))

(define foreign-scrypt-set-defaults
  (pointer->procedure int (dynamic-func "scrypt_set_defaults" libscrypt) (q (* * * * * *))))

(define pointer-size (sizeof (q *)))

(define* (scrypt->string password #:optional salt (size 0) (N 0) (r 0) (p 0))
  "bytevector bytevector integer integer integer integer -> string:base91-encoded-hash"
  (let
    ( (r (bytevector->pointer (make-bytevector pointer-size 0)))
      (r-len (make-bytevector pointer-size 0)))
    (foreign-scrypt->string (bytevector->pointer password) (bytevector-length password)
      (if salt (bytevector->pointer salt) %null-pointer) (if salt (bytevector-length salt) 0)
      N r p size r (bytevector->pointer r-len))
    (pointer->string (dereference-pointer r)
      (bytevector-uint-ref r-len 0 (endianness little) pointer-size))))

(define (scrypt-defaults)
  "-> (salt salt-length size N r p)
   get scrypt-library defaults for the values"
  (let
    ( (salt (bytevector->pointer (make-bytevector pointer-size)))
      (salt-length (make-bytevector pointer-size)) (size (make-bytevector (sizeof size_t)))
      (N (make-bytevector (sizeof uint64))) (r (make-bytevector (sizeof uint32)))
      (p (make-bytevector (sizeof uint32))))
    (apply foreign-scrypt-set-defaults salt (map bytevector->pointer (list salt-length size N r p)))
    (list
      (pointer->bytevector (dereference-pointer salt)
        (bytevector-uint-ref salt-length 0 (endianness little) (sizeof size_t)))
      (bytevector-uint-ref size 0 (endianness little) (sizeof size_t))
      (inexact->exact (/ (log (bytevector-u64-ref N 0 (endianness little))) (log 2)))
      (bytevector-u32-ref r 0 (endianness little)) (bytevector-u32-ref p 0 (endianness little)))))

(define (scrypt password salt size logN r p)
  "bytevector bytevector integer integer integer integer -> bytevector:hash"
  (let (result (make-bytevector size 0))
    (foreign-scrypt (bytevector->pointer password) (bytevector-length password)
      (bytevector->pointer salt) (bytevector-length salt)
      (expt 2 logN) r p (bytevector->pointer result) size)
    result))

(define (scrypt-check key . args) (bytevector=? key (apply scrypt args)))
