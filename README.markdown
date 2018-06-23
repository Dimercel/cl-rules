# cl-rules

[![Build Status](https://travis-ci.org/Dimercel/cl-rules.svg?branch=master)](https://travis-ci.org/Dimercel/cl-rules)

Simple DSL for rules that can be configured without code. If part of your program logic is set of rules, this package will help you. You can create custom rules and conditions, bind actions to rules, load and save your rules from/in yaml format.

## Usage

Consider a simple example: system of different tariffs, which defines in a declarative style. Each tariff contains a set of limits from which the cost is calculated.

### 1. Define your set of parameters

Parameters represent basic variables of your system. They may contain absolutely any information and arbitrary structure. For creating them, specify the name and initial value. Any parameter can change own value over time, for this purpose is intended `setparam`.
  
In our example, parameters - this is basic characteristics of a tariff. Define them:

```common-lisp
(in-package :cl-user)
(defpackage my-package
  (:use :cl)
  (:import-from :cl-rules
                :defparam
                :defaction
                :defcond
                :defrule
                :eval-rule
                :fire-rule)))
(in-package :my-package)


(defparam ram 0)
(defparam cpu 0)
(defparam disk 0)

(defvar *user-balance* 1000) ;; Starting value of user balance. Only for illustration.
```

### 2. Define your conditons

Condition represent predicate, which may be only a true or false. In the base case, the condition is a function of previously defined parameters. With help of `param-val` you can get a parameter value within the condition or specify it in arguments.
  
All values of the tariff characteristics are in a certain range, therefore it is sufficient for us to define only one condition:

```common-lisp
(defcond between (low-limit high-limit value)
  (and (>= value low-limit) (<= value high-limit value)))
```

### 3. Define your actions

Action - arbitrary kind of code. Any actions may be linked with a rule. The action called only if a rule is true.

Define two actions: first - withdraw money from the user account, second - print a user account balance.

```common-lisp
(defaction pay (amount)
  (decf *user-balance* amount))
  
(defaction report ()
  (print (format nil "The balance is ~d" *user-balance*)))
```

### 4. Define your rules!!!

The rules - heart of our system. They consist of several conditions and optional actions. Conditions specified with concrete values of arguments and can be specified in the arbitrary order. Only if all conditions are true, a rule is true.

Now we can define the rules for our tariff system. On this stage exists two ways:

define your rules in code

```common-lisp
(defrule mini-tariff
  :actions
  ((pay 100)
   (report))
   
  (between 0 512 ram)
  (between 1 1 cpu)
  (between 0 20 disk))
  
(defrule base-tariff
  :actions
  ((pay 200)
   (report))
   
  (between 513 1024 ram)
  (between 1 1 cpu)
  (between 21 30 disk))

(defrule super-tariff
  :actions
  ((pay 300)
   (report))
   
  (between 1025 2048 ram)
  (between 2 2 cpu)
  (between 31 50 disk))
```
or load them from file using function `(loads "/path/to/your/file")`

```yml
rules:
  mini-tariff:
    conditions:
      - [between, 0, 512, "{{ram}}"]
      - [between, 1, 1, "{{cpu}}"]
      - [between, 0, 20, "{{disk}}"]
    actions:
      - [pay, 100]
      - [report]
  base-tariff:
    conditions:
      - [between, 513, 1024, "{{ram}}"]
      - [between, 1, 1, "{{cpu}}"]
      - [between, 21, 30, "{{disk}}"]
    actions:
      - [pay, 200]
      - [report]
  super-tariff:
    conditions:
      - [between, 1025, 2048, "{{ram}}"]
      - [between, 2, 2, "{{cpu}}"]
      - [between, 31, 50, "{{disk}}"]
    actions:
      - [pay, 300]
      - [report]
```
For storage is used [yaml](http://yaml.org/) format.

### Time to run your rules

Rules running with this command: `(fire-rule 'mini-tariff 'base-tariff 'super-tariff)`, but in this case, actions not called. `fire-rule` - only return a logic value, true - if all rules are true and false - otherwise.

If you want to call actions, using this: `(eval-rule 'mini-tariff 'base-tariff 'super-tariff)`. After this command, `*user-balance*` will be less.

More examples in [tests](https://github.com/Dimercel/cl-rules/tree/master/t).

## Installation

```common-lisp
(ql:quickload :cl-rules)
```

## Author

* Ito Dimercel (xolcman@gmail.com)

## Copyright

Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)

## License

Licensed under the GPL-3.0 License.
