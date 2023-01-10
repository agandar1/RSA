(defun euclid (a b x y m &aux (r (mod a b)))
  "returns a cons cell containing gcd(a, m) and inv, s.t. a*inv = 1(mod m)"
  (if (zerop r) (cons (abs b) (mod y m))
      (euclid b r y (- x (* y (floor a b))) m)))

(defun my-gcd (a b)
  "find the GCD of integers A and B"
  (car (euclid a b 1 0 b)))

(defun mod-inv (a b)
  "find the inverse of A in (mod B)"
  (cdr (euclid a b 1 0 b)))

(defun mod-pow (base exp n)
  "find base^exp (mod n)"
  (cond ((= exp 0) 1)
        ((= exp 1) (mod base n))
        ((oddp exp) (let ((x (mod-pow base (/ (1- exp) 2) n)))
                       (mod (* (mod base n) x x) n)))
        ((evenp exp) (let ((x (mod-pow base (/ exp 2) n)))
                       (mod (* x x) n)))))

(defun primep (p &key (amplify 20))
  "test if a number is prime. repeated AMPLIFY times to get better chances"
  (labels ((fermat (a) (and (<= (my-gcd a p) 1)
                            (member (mod-pow a (/ (1- p) 2) p) `(,(1- p) 1)))))
    (cond ((= p 2) t)
          ((or (< p 2) (evenp p)) nil)
          (t (loop repeat amplify for test = (1+ (random (1- p)))
                   do (unless (fermat test) (return nil))
                   finally (return t))))))

(defun gen-odd ()
  "generate a huge odd number of SIZE bits"
  (labels ((to-int (bits) (reduce (lambda (x y) (+ y (* x 2))) bits)))
    (let ((lst (loop repeat 512 collect (random 2))))
      (to-int (make-array 512 :element-type 'bit :initial-contents lst)))))

(defun gen-prime (&key (other-than -1))
  "generate a big prime number, not equal to OTHER-THAN"
  (loop for try = (gen-odd) then (gen-odd)
        until (and (primep try) (/= try other-than))
        finally (return try)))

(defun rsa-keys ()
  "generate an RSA public/private key pair of the form (N e d)"
  (let* ((p (gen-prime))
         (q (gen-prime :other-than p))
         (n (* p q))
         (e 65537)
         (d (mod-inv e (* (1- p) (1- q)))))
    (list n e d)))

(defun rsa-encrypt (msg pub-key)
  "RSA encrypt int MSG using public key PUB-KEY"
  (mod-pow msg (cdr pub-key) (car pub-key)))

(defun rsa-decrypt (cipher pub-key priv-key)
  "decrypt RSA ciphertext (int) using key pair PUB-KEY and PRIV-KEY"
  (mod-pow cipher priv-key (car pub-key)))

;;; Example usage
(defun run-test (msg)
  (let* ((keys (rsa-keys))
         (pub  (cons (car keys) (cadr keys)))
         (priv (caddr keys))
         (cipher (rsa-encrypt msg pub)))
    (rsa-decrypt (print cipher) pub priv)))
