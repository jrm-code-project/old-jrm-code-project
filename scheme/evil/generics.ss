#ci(module generics (lib "swindle.ss" "swindle")

  ;; These operators are obvious names for use in different phases of
  ;; the compiler on different data structures.

  (defgeneric access/environment  (object))
  (defgeneric access/name         (object))

  (defgeneric assignment/variable (object))
  (defgeneric assignment/value    (object))

  (defgeneric definition/name     (object))
  (defgeneric definition/value    (object))

  (defgeneric combination/operator (object))
  (defgeneric combination/operands (object))

  (defgeneric conditional/predicate   (object))
  (defgeneric conditional/consequent  (object))
  (defgeneric conditional/alternative (object))

  (defgeneric constant/value          (object))

  (defgeneric return/value            (object))
  (defgeneric sequence/actions        (object))

  (defgeneric variable/name           (object))

  (provide
   access/environment
   access/name

   assignment/variable
   assignment/value

   definition/name
   definition/value

   combination/operator
   combination/operands

   conditional/predicate
   conditional/consequent
   conditional/alternative

   constant/value

   return/value
   sequence/actions

   variable/name
   ))
