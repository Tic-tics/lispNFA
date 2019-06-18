;;;; statemachine.lisp

(in-package #:statemachine)

(defun next-state (current-state delta symbol)
  (fset:convert 'fset::set (gethash symbol (gethash current-state delta))))

(defun get-next-states-delta (current-states delta symbol)
  (fset:reduce (lambda (istate nstate)
                 (fset:union istate (next-state nstate delta symbol)))
               current-states :initial-value (fset:empty-set)))

(defun get-next-states (current-states machine symbol)
  (let ((delta (gethash "delta" machine)))
    (get-next-states-delta current-states delta symbol)))

(defun build-machine (filename)
  (file-load-json filename))

(defun build-nfa (filename)
  (let ((nfa (file-load-json filename)))
    (close-epsilon nfa)
    nfa))

(defun start-state (machine)
  (fset:convert 'fset:set (gethash "start_states" machine)))

(defun epsilon-closure-t (state machine)
  (let ((ec (fset:set state)) (delta (gethash "delta" machine)))
    (dotimes (x (hash-table-size delta))
      (setf ec (fset:union ec (get-next-states ec machine "epsilon"))))
    ec))

(defun close-epsilon-destinations (destinations ec-table)
  (let ((new-destinations (fset:empty-set)))
    (dolist (x destinations)
      (setf new-destinations (fset:union new-destinations (gethash x ec-table))))
    (fset:convert 'list new-destinations)))

(defun build-ec-table (machine)
  (let ((delta (gethash "delta" machine)) (ec-table (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (declare (ignore value))
               (setf (gethash key ec-table) (epsilon-closure-t key machine)))
             delta)
    ec-table))

(defun close-epsilon-transition (symbol transition-table ec-table)
  (setf (gethash symbol transition-table) (close-epsilon-destinations (gethash symbol transition-table) ec-table)))

(defun close-epsilon-state (transition-table ec-table)
  (maphash (lambda (symbol transition)
             (declare (ignore transition))
             (setf (gethash symbol transition-table) (close-epsilon-transition symbol transition-table ec-table)))
           transition-table)
  transition-table)

(defun close-epsilon (machine)
  (let ((ec-table (build-ec-table machine)) (delta (gethash "delta" machine)))
    (maphash (lambda (state transition-table)
               (declare (ignore state))
               (close-epsilon-state transition-table ec-table))
             delta)))

(defun in-final-state (machine states)
  (let ((final-states (fset:convert 'fset:set (gethash "final_states" machine))))
    (not (fset:disjoint? states final-states))))

(defun calc-next (machine string states)
  (if string
      (calc-next machine (cdr string) (get-next-states states machine (car string)))
      (in-final-state machine states)))

(defun test-list (machine string)
  (calc-next machine string (start-state machine)))

(defun string-to-list (string)
  (loop for character across string collect (string character)))

(defun test-string (machine string)
  (test-list machine (string-to-list string)))


