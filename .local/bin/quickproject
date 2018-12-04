#!/usr/bin/sbcl --script

#|

Manoel Vilela © 2018

This script helps me building a simple ASDF project for Common Lisp
based in the library quickproject.

|#

#-quicklisp (load "~/.sbclrc")
#-quicklisp (error "You should have quicklisp installed!")

(ql:quickload (list :quickproject
                    :cl-fad)
              :silent t)

(defpackage #:quickproject-cli
  (:use #:cl
        #:quickproject)
  (:import-from #:uiop :delete-directory-tree)
  (:import-from #:sb-ext :exit :*posix-argv*)
  (:import-from #:cl-fad :pathname-as-directory :directory-exists-p))

(in-package #:quickproject-cli)

(setq *author* "Manoel Vilela")
(defparameter *program-name*
  (or "quickproject"
      (pathname-name (car *posix-argv*)))
  "Program script name")
(defparameter *pathname* (cadr *posix-argv*)
  "Required parameter: Project pathname")
(defparameter *project-name* (caddr *posix-argv*)
  "Optional parameter: Project name")
(defparameter *usage*
  (format nil
          "Usage: ~a PROJECT_PATH [PROJECT_NAME]

Default author: ~a

This script creates a quicklisp project based on the library
quickproject.
"
          *program-name*
          quickproject:*author*)
  "Usage docstring!")


(defun exit-message (message exit-code)
  "EXIT-MESSAGE print a message to *ERROR-OUTPUT* and exists with EXIT-CODE."
  (format *error-output* message)
  (exit :code exit-code))

(defun errorf (message &rest args)
  "ERRORF print MESSAGE using FORMAT to *ERROR-OUTPUT* based on ARGS."
  (apply #'format
         *error-output*
         (format nil "~a: error: ~a" *program-name* message)
         args))

(defun help-p (&optional (args *posix-argv*))
  "HELP-P checks if -h or --help was passed to *POSIX-ARGV*."
  (loop for x in args
        thereis (or (equal x "-h")
                    (equal x "--help"))))

(defun string->directory (string)
  "STRING->DIRECTORY converts a STRING proper into directory pathname."
  (pathname-as-directory (merge-pathnames (make-pathname :name string))))

(defun create-project (pathname-string project-name)
  "CREATE-PROJECT is quickproject:make-project wrapper with additional handling."
  (let* ((pathname (string->directory pathname-string))
         (pathname-wildcard (merge-pathnames pathname "*.*"))
         (directory-name (first (last (pathname-directory pathname)))))
    (when (directory-exists-p pathname)
      (format t "INFO: overwriting project directory ~a~%" directory-name)
      (delete-directory-tree pathname :validate t)
      (format t "INFO: ~a was totally deleted!~%" directory-name))
    (cond ((null project-name) ;; if there is a *project-name*
           (make-project pathname))
          (t (make-project pathname
                           :name project-name)))
    (format t "Created project '~a' under ~a:~%"
            (or project-name directory-name)
            pathname)
    (loop for f in (directory pathname-wildcard)
          do (format t "    + ~a~%" (file-namestring f)))))

(defun main ()
  "MAIN runs all the shits together."
  (cond ((help-p) ;; help!
         (exit-message *usage* 0))
        ((null *pathname*)  ;; a project_pathname is need
         (errorf "PROJECT_PATHNAME should be provided.~%")
         (exit-message *usage* 0))
        (t (create-project *pathname* *project-name*))))


(eval-when (:execute)
  (main))
