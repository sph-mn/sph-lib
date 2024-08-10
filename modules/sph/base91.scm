(define-module (sph base91))
(use-modules (rnrs arithmetic bitwise) (rnrs bytevectors))
(export base91-decode base91-encode sph-base91-description)
(define sph-base91-description "encoder/decoder")

(define chars-encode
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!#$%&()*+,./:;<=>?@[]^_`{|}~\"")

(define chars-decode
  (let loop ((i 0) (table (make-vector 256 #f)))
    (if (< i (string-length chars-encode))
      (let ((index (char->integer (string-ref chars-encode i))))
        (loop (+ i 1) (begin (vector-set! table index i) table)))
      table)))

(define (base91-decode data) "string -> bytevector"
  (u8-list->bytevector
    (let ((data-length (string-length data)))
      (let decode-loop ((i 0) (value #f) (bit-accumulator 0) (bit-count 0))
        (if (< i data-length)
          (let ((bits (vector-ref chars-decode (char->integer (string-ref data i)))))
            (if bits
              (if value
                (let*
                  ( (value (+ value (* bits 91)))
                    (bit-accumulator
                      (bitwise-ior bit-accumulator (bitwise-arithmetic-shift-left value bit-count)))
                    (bit-count (+ bit-count (if (> (bitwise-and value 8191) 88) 13 14))))
                  (cons (bitwise-and 255 bit-accumulator)
                    (let loop
                      ( (bit-accumulator (bitwise-arithmetic-shift-right bit-accumulator 8))
                        (bit-count (- bit-count 8)))
                      (if (> bit-count 7)
                        (cons (bitwise-and 255 bit-accumulator)
                          (loop (bitwise-arithmetic-shift-right bit-accumulator 8) (- bit-count 8)))
                        (decode-loop (+ 1 i) #f bit-accumulator bit-count)))))
                (decode-loop (+ 1 i) bits bit-accumulator bit-count))
              (decode-loop (+ 1 i) value bit-accumulator bit-count)))
          (if value
            (list
              (bitwise-and 255
                (bitwise-ior bit-accumulator (bitwise-arithmetic-shift-left value bit-count))))
            (list)))))))

(define (base91-encode data) "bytevector -> string"
  (list->string
    (let encode-loop ((i 0) (bit-accumulator 0) (bit-count 0))
      (if (< i (bytevector-length data))
        (let*
          ( (a (bytevector-u8-ref data i))
            (bit-accumulator
              (bitwise-ior bit-accumulator (bitwise-arithmetic-shift-left a bit-count)))
            (bit-count (+ bit-count 8)))
          (if (> bit-count 13)
            (let*
              ( (value (bitwise-and bit-accumulator 8191))
                (bit-accumulator
                  (if (> value 88) (bitwise-arithmetic-shift-right bit-accumulator 13)
                    (bitwise-arithmetic-shift-right bit-accumulator 14)))
                (bit-count (if (> value 88) (- bit-count 13) (- bit-count 14))))
              (cons* (string-ref chars-encode (modulo value 91))
                (string-ref chars-encode (quotient value 91))
                (encode-loop (+ i 1) bit-accumulator bit-count)))
            (encode-loop (+ i 1) bit-accumulator bit-count)))
        (if (= 0 bit-count) (list)
          (cons (string-ref chars-encode (modulo bit-accumulator 91))
            (if (or (< 7 bit-count) (< 90 bit-accumulator))
              (list (string-ref chars-encode (quotient bit-accumulator 91))) (list))))))))
