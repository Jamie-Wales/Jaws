;; Load portaudio library
(load-library "portaudio" "/Users/jamie/dev/jaws/pa/build/libportaudio_wrapper.dylib")

;; Register C functions
(register-function "portaudio" "pa_initialize" "pa-init" "int")
(register-function "portaudio" "pa_start" "pa-start" "int")
(register-function "portaudio" "pa_stop" "pa-stop" "int")
(register-function "portaudio" "pa_set_frequency" "pa-set-frequency" "void")
(register-function "portaudio" "pa_set_amplitude" "pa-set-amplitude" "void")
(register-function "portaudio" "pa_cleanup" "pa-cleanup" "void")

(define (delay-loop n)
  (if (= n 0)
      #t
      (delay-loop (- n 1))))

(define (delay n)
  (delay-loop (* n 1000000)))

(define (init-audio)
  (if (= (pa-init) 0)
      (error "Failed to initialize PortAudio")
      #t))

(define (play-tone freq amp)
  (pa-set-frequency freq)
  (pa-set-amplitude amp)
  (if (= (pa-start) 0)
      (error "Failed to start audio")
      #t))

(define (stop-tone)
  (if (= (pa-stop) 0)
      (error "Failed to stop audio")
      #t))

;; Musical note frequencies using lists
(define notes
  (list
   (list 'A4 440.0)
   (list 'B4 493.88)
   (list 'C5 523.25)
   (list 'D5 587.33)
   (list 'E5 659.25)
   (list 'F5 698.46)
   (list 'G5 783.99)))

;; Helper to find note frequency
(define (find-note name notes)
  (if (null? notes)
      #f
      (if (eq? name (car (car notes)))
          (car (cdr (car notes)))
          (find-note name (cdr notes)))))

;; Helper to play a note by name
(define (play-note note-name amp)
  (let ((freq (find-note note-name notes)))
    (if freq
        (play-tone freq amp)
        (error "Unknown note"))))

;; Recursive function to play notes with delay
(define (play-notes notes)
  (if (null? notes)
      #t
      (begin
        (play-note (car (car notes)) 0.5)
        (delay 5)  ; ~0.5 second delay
        (stop-tone)
        (delay 1)  ; ~0.1 second delay
        (play-notes (cdr notes)))))

;; Play scale using recursion
(define (play-scale)
  (init-audio)
  (play-notes notes))
