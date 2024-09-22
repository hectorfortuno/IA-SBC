;;;======================================================
;;;   Automotive Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a car.
;;;
;;;     CLIPS Version 6.4 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;* DEFCLASSES *
;;****************


;;;+++++++++++++++++
;;; 
;;; MÓDULO DE PREGUNTAS
;;;
;;;+++++++++++++++++

;;;(defmodule preguntas)

(deffunction ask-question (?question $?allowed-values)
    (print ?question)
    (bind ?answer (read))
    (if (lexemep ?answer) 
        then (bind ?answer (lowcase ?answer)))
    (while (not (member$ ?answer ?allowed-values)) do
        (print ?question)
        (bind ?answer (read))
        (if (lexemep ?answer) 
            then (bind ?answer (lowcase ?answer))))
    ?answer)

(deffunction yes-or-no-p (?question)
    (bind ?response (ask-question ?question yes no y n))
    (if (or (eq ?response yes) (eq ?response y))
        then yes 
        else no))

(defrule saber-come-carne ""
    (not (come-carne ?))
    (not (output ?))
    =>
    (assert (come-carne (yes-or-no-p "Comes carne (yes/no)? "))))

(defrule saber-gusta-ternera ""
    (come-carne yes)
    (not (gusta-ternera ?))
    (not (output ?))
    =>
    (assert (gusta-ternera (yes-or-no-p "Te gusta el bistec (yes/no)? "))))

(defrule saber-gusta-pollo ""
    (come-carne yes)
    (not (gusta-pollo ?))
    (not (output ?))
    =>
    (assert (gusta-pollo (yes-or-no-p "Te gusta el pollo (yes/no)? "))))

(defrule poner-ternera ""
    (come-carne yes)
    (gusta-ternera yes)
    (not (output ?))
    =>
    (assert (output "Come ternera todos los putos días.")))

(defrule poner-pollo ""
    (come-carne yes)
    (gusta-pollo yes)
    (not (output ?))
    =>
    (assert (output "Come pollo todos los putos días.")))    


;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
    (declare (salience 10))
    =>
    (println crlf "The Menu Creator" crlf))

(defrule print-menu ""
    (declare (salience 10))
    (output ?item)
    =>
    (println crlf "Suggested Menu:" crlf)
    (println " " ?item crlf))
