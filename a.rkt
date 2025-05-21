#lang racket

(define jaws-path "/Users/jamie/Dev/jaws/build/jaws")
(define script-path "/Users/jamie/Dev/jaws/example.scm")

(system* jaws-path "--script" script-path)
