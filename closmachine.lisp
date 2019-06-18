(in-package #:statemachine)

(defparameter *current-state-color* :chartreuse)
(defparameter *default-state-color* :ivory)

(defparameter *epsilon-string* "epsilon")

;;A single state in the state machine
(defclass node ()
  ((name
    ;;The name of the node
    :initarg :name
    :initform (error "node constructed with no name")
    :accessor name)
   (transitions
    ;;A hash-table of sets of nodes to transition to on input symbols
    ;;<input symbol, fset:set of nodes>
    :initform (make-hash-table :test 'equal)
    :accessor transitions)
   (delta
    ;;Transitions, except using the closed epsilon transformation
    ;;<input symbol, fset:set of nodes>
    :initform nil
    :accessor delta)
   (epsilon-closure
    ;;fset:set of states that can be reached from this state via epsilon moves
    :initform nil
    :accessor epsilon-closure)
   (final-state?
    :initform nil
    :accessor final-state?)))

;;A state machine
;;The name node-machine contrasts with table-machine, the original model
(defclass node-machine ()
  ((state-table
    ;;A hash-table of nodes that represent the machine's states
    :initarg :state-table
    :initform (error "node-machine constructed with no nodes")
    :accessor state-table)
   (current-states
    ;;An fset:set of nodes representing the states that the machine is currently in
    :initform (fset:empty-set)
    :accessor current-states)
   (start-states
    ;;An fset:set of nodes representing the states that the machine is in when reset
    :initform nil
    :accessor start-states)
   (description
    :initform ""
    :accessor description)))

;;Creates a set of the nodes in n-machine that have names contained in state-list
;;Returns an fset:set of nodes
(defun collect-states (state-list n-machine)
  (let ((node-set (fset:empty-set)))
    (mapc (lambda (x)
            (setf node-set (fset:union node-set (fset:set (gethash x (state-table n-machine))))))
          state-list)
    node-set))

;;Builds a node-machine following the definition in a JSON file
;;Currently does not perform the epsilon transformation
;;Returns a node-machine
(defun build-node-machine (filename)
  (let* ((t-machine (build-machine filename))
         (n-machine (make-instance 'node-machine
                                   :state-table (generate-nodes-table t-machine))))
    (setf (start-states n-machine) (collect-states (gethash "start_states" t-machine) n-machine))
    (fill-transitions (state-table n-machine) t-machine)
    (register-end-states n-machine t-machine)
    (epsilon-transform n-machine)
    (setf (description n-machine) (gethash "description" t-machine))
    n-machine))

;;Returns a node machine to it's start states
;;Probably unnecessary due to simplicity, but seems like it will make later code easier to read.
(defun restart-node-machine (n-machine)
  (setf (current-states n-machine) (start-states n-machine)))

;;Defines how to print nodes
(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
                     (current-states current-states))
        obj
      (format stream "~a" name))))

;;Defines how to print node-machines
(defmethod print-object ((obj node-machine) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((state-table state-table)
                     (current-states current-states))
        obj
      (format stream "~a~%~a"
              (loop for state being the hash-keys of (state-table obj) collect state)
              (current-states obj)))))

;;Generates a hash table of node objects from a t-machine.
;;Returns a hash table <name, node> of empty nodes
(defun generate-nodes-table (machine)
  (let ((nodes (make-hash-table :test 'equal)))
    (mapcar (lambda (x)
              (setf (gethash (name x) nodes) x))
            (mapcar (lambda (y)
                      (make-instance 'node :name y))
                    (gethash "states" machine)))
    nodes))

;;Fills in the transitions in a table of empty nodes
;;Uses the delta function from a supplied t-machine
;;Used for side effects, no meaningful return
(defun fill-transitions (nodes-table machine)
  (maphash (lambda (state transitions)
             (maphash (lambda (symbol targets)
                        (setf (gethash symbol (transitions (gethash state nodes-table)))
                              (fset:convert 'fset:set (mapcar (lambda (target)
                                                                (gethash target nodes-table))
                                                              targets))))
                      transitions))
           (gethash "delta" machine)))

;;Sets the appropriate end state values for states in n-machine built from t-machine
;;Used for side effects, no meaningful return
(defun register-end-states (n-machine t-machine)
  (let ((final-states (fset:convert 'fset:set (gethash "final_states" t-machine))))
    (loop for state being the hash-values of (state-table n-machine)
          do (setf (final-state? state) (fset:contains? final-states (name state))))))

;;Collects a set of all valid states given a current state and a symbol to consume
;;Returns an fset:set of nodes
(defun get-next-states (state symbol)
  (let ((next-states (gethash symbol (transitions state))))
    (if next-states
        next-states
        (fset:empty-set))))

;;Advances a node machine to the next set of valid states given an input symbol
;;Used for side effects
;;Returns the fset:set of nodes representing the machine's new state
(defun advance-node-machine (n-machine sym)
  (setf (current-states n-machine)
        (fset:reduce (lambda (acc i)
                       (fset:union acc (get-next-states i sym)))
                     (current-states n-machine)
                     :initial-value (fset:empty-set))))
;;TODO update DOT generation code to group transition arrows that point to the same node.
;;Compute and use a new table to track all possible nodes and symbols, then iterate over nodes.

;;DOT GENERATION CODE
;;Creates a list of nodes
;;Uses the same names as the original machine
;;Uses *current-state-color* for the color of the current states
;;Uses *default-state-color* for the color of nodes the machine is not currently in
(defmethod cl-dot:graph-object-node ((graph node-machine) (object node))
  (make-instance 'cl-dot:node
                 :attributes (list
                              :label (name object)
                              :style :filled
                              :shape (if (final-state? object)
                                         :doublecircle
                                         :circle)
                              :fillcolor (if (fset:contains? (current-states graph) object)
                                         *current-state-color*
                                         *default-state-color*))))

#||
;;Old version, generates one edge per symbol, fairly sloppy
;;Retained for comparison and posterity
;;Creates a list of edges from a node
(defmethod cl-dot:graph-object-points-to ((graph node-machine) (object node))
  (labels ((build-edge-to (symbol destination)
             (make-instance 'cl-dot:attributed
                            :object destination
                            :attributes (list :label symbol))))
    (loop for symbol being the hash-keys of (transitions object)
          using (hash-value state-set)
          append (loop for target in (fset:convert 'list state-set)
                       collect (build-edge-to symbol target)))))
||#

(defun list-to-csv (symbols)
  (let ((s (format nil "~{~A, ~}" symbols)))
    (subseq s 0 (- (length s) 2))))

;;Creates a list of edges coming from a node
(defmethod cl-dot:graph-object-points-to ((graph node-machine) (object node))
  (let ((arrows (make-hash-table :test 'equal)))
    (loop for symbol being the hash-keys of (transitions object)
          using (hash-value state-set)
          do (loop for target in (fset:convert 'list state-set)
                   do (if (gethash target arrows)
                          (setf (gethash target arrows) (append (list symbol) (gethash target arrows)))
                          (setf (gethash target arrows) (list symbol)))))
    (labels ((build-edge-to (symbols destination)
               (make-instance 'cl-dot:attributed
                              :object destination
                              :attributes (list :label (list-to-csv symbols)))))
      (loop for destination being the hash-keys of arrows
            using (hash-value symbols)
            collect (build-edge-to symbols destination)))))


;;Creates a dot file on stream
(defmethod create-dot (n-machine stream)
  (let* ((state-list (loop for state being the hash-values of (state-table n-machine)
                           collect state))
         (graph (cl-dot:generate-graph-from-roots n-machine state-list '(:rankdir "LR"))))
    (cl-dot:print-graph graph :stream stream)))

;;EPSILON TRANSFORMATION GOES HERE

;;Calculates the epsilon closure of a single state, storing it in the epsilon-closure field
(defun calc-epsilon-closure (state)
  (if (not (epsilon-closure state))
      (setf (epsilon-closure state) (fset:set state)))
  (let ((new-closure (fset:union (epsilon-closure state)
                                 (fset:reduce #'fset:union
                                              (fset:image (lambda (x)
                                                            (get-next-states x *epsilon-string*))
                                                          (epsilon-closure state))
                                              :initial-value (fset:empty-set)))))
    (if (> (fset:size new-closure) (fset:size (epsilon-closure state)))
        (progn
          (setf (epsilon-closure state) new-closure)
          (calc-epsilon-closure state))
        new-closure)))

;;Calculates and populates the epsilon closure of all states in an n-machine
(defun populate-epsilon-closure (n-machine)
  (loop for state being the hash-values of (state-table n-machine)
        do (setf (epsilon-closure state) (calc-epsilon-closure state))))

;;Calculates the delta (including epsilon transform) of a state, storing in the delta field
(defun calc-delta (state)
  (setf (delta state) (make-hash-table :test 'equal))
  (maphash (lambda (symbol destinations)
             (if (not (string= symbol *epsilon-string*))
                 (let ((new-dests (fset:reduce #'fset:union (fset:image #'epsilon-closure destinations))))
                   (setf (gethash symbol (delta state)) new-dests))))
           (transitions state)))

;;Calculates and populates the delta of all states in an n-machine
(defun populate-delta (n-machine)
  (loop for state being the hash-values of (state-table n-machine)
        do (calc-delta state)))

(defun epsilon-transform (n-machine)
  (populate-epsilon-closure n-machine)
  (populate-delta n-machine))

(defun epsilon-transform-mutate-state (state)
  (setf (transitions state) (delta state)))

(defun epsilon-transform-mutate (n-machine)
  (loop for state being the hash-values of (state-table n-machine)
        do (epsilon-transform-mutate-state state)))


;;Builds a hashtable of lists of transitions for use in t-machine/json conversion
(defun build-delta-table-state (state)
  (let ((delta-table (make-hash-table :test #'equal)))
    (loop for symbol being the hash-keys of (transitions state)
          do (setf (gethash symbol delta-table) (fset:convert 'list (fset:image #'name (gethash symbol (transitions state))))))
    delta-table))

;;Builds a hashtable of transitions for all states for use in t-machine/json conversion
(defun build-delta-table-machine (n-machine)
  (let ((delta-table (make-hash-table :test #'equal)))
    (loop for state being the hash-values of (state-table n-machine)
          do (setf (gethash (name state) delta-table) (build-delta-table-state state)))
    delta-table))

;;Builds a t-machine from an n-machine for json output
(defun build-t-machine (n-machine)
  (let* ((t-machine (make-hash-table :test #'equal))
         (state-list (loop for state being the hash-values of (state-table n-machine)
                           collect (name state)))
         (alphabet (car (remove-duplicates (append (append (loop for state being the hash-values of (state-table n-machine)
                                                                 collect (loop for symbol being the hash-keys of (transitions state)
                                                                               collect symbol))))
                                           :test #'equal)))
         (start-states (mapcar #'name (fset:convert 'list (start-states n-machine))))
         (final-states (mapcar #'name (remove-if-not #'final-state? (loop for state being the hash-values of (state-table n-machine) collect state))))
         (delta-table (build-delta-table-machine n-machine)))
    (setf (gethash "states" t-machine) state-list)
    (setf (gethash "alphabet" t-machine) alphabet)
    (setf (gethash "start_states" t-machine) start-states)
    (setf (gethash "final_states" t-machine) final-states)
    (setf (gethash "description" t-machine) (description n-machine))
    (setf (gethash "delta" t-machine) delta-table)    
    t-machine))

