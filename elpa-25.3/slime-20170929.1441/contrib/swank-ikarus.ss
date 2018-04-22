;; swank-larceny.scm --- Swank server for Ikarus
;;
;; License: Public Domain
;; Author: Helmut Eller
;;
;; In a shell execute:
;;   ikarus swank-ikarus.ss
;; and then `M-x slime-connect' in Emacs.
;;

(library (swank os)
    (export getpid make-server-socket accept local-port close-socket)
    (import (rnrs)
	    (only (ikarus foreign) make-c-callout dlsym dlopen
		  pointer-set-c-long! pointer-ref-c-unsigned-short
		  malloc free pointer-size)
	    (rename (only (ikarus ipc) tcp-server-socket accept-connection
			  close-tcp-server-socket)
		    (tcp-server-socket make-server-socket)
		    (close-tcp-server-socket close-socket))
	    (only (ikarus)
		  struct-type-descriptor
		  struct-type-field-names
		  struct-field-accessor)
	    )

 (define libc (dlopen))
 (define (cfun name return-type arg-types)
   ((make-c-callout return-type arg-types) (dlsym libc name)))

 (define getpid (cfun "getpid" 'signed-int '()))

 (define (accept socket codec)
   (let-values (((in out) (accept-connection socket)))
     (values (transcoded-port in (make-transcoder codec))
	     (transcoded-port out (make-transcoder codec)))))

 (define (socket-fd socket)
   (let ((rtd (struct-type-descriptor socket)))
     (do ((i 0 (+ i 1))
	  (names (struct-type-field-names rtd) (cdr names)))
	 ((eq? (car names) 'fd) ((struct-field-accessor rtd i) socket)))))

 (define sockaddr_in/size 16)
 (define sockaddr_in/sin_family 0)
 (define sockaddr_in/sin_port 2)
 (define sockaddr_in/sin_addr 4)

 (define (local-port socket)
   (let* ((fd (socket-fd socket))
	  (addr (malloc sockaddr_in/size))
	  (size (malloc (pointer-size))))
     (pointer-set-c-long! size 0 sockaddr_in/size)
     (let ((code (getsockname fd addr size))
	   (port (ntohs (pointer-ref-c-unsigned-short 
			 addr sockaddr_in/sin_port))))
       (free addr)
       (free size)
       (cond ((= code -1) (error "getsockname failed"))
	     (#t port)))))

 (define getsockname 
   (cfun "getsockname" 'signed-int '(signed-int pointer pointer)))

 (define ntohs (cfun "ntohs" 'unsigned-short '(unsigned-short)))

 )


(library (swank sys)
    (export implementation-name eval-in-interaction-environment)
    (import (rnrs) 
	    (rnrs eval)
	    (only (ikarus) interaction-environment))

 (define (implementation-name) "ikarus")

 (define (eval-in-interaction-environment form)
   (eval form (interaction-environment)))

 )

(import (only (ikarus) load))
(load "swank-r6rs.scm")
(import (swank))
(start-server #f)
