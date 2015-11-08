#lang racket

(module reader syntax/module-reader
  lambda/main
  #:read        lambda-read
  #:read-syntax lambda-read-syntax

  (require (prefix-in lambda- "reader.rkt")))
