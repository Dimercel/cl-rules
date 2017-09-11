# cl-rules

[![Build Status](https://travis-ci.org/Dimercel/cl-rules.svg?branch=master)](https://travis-ci.org/Dimercel/cl-rules)

Simple DSL for rules that can be configured without code. If part of your programm logic is set of rules, then this package will help you. With him you can create custom rules and conditions, bind actions to rules, load and save your rules in yaml format.

## Usage

Simple example: system of different tariffs, which defines in declarative style. Each tariff contains a set of limits from which the cost is calculated.

### 1. Define your set of parameters

Parameters represent various characteristics your system. They is basic values for rules. Parameters can change over time.

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


(defparam ram (user-ram-value))
(defparam cpu (user-cpu-value))
(defparam disk (user-disk-value))

(defvar *user-balance* 1000) ;; Starting value of user balance. Only for illustration.
```
All tariffs depends on this three parameters.

### 2. Define your conditons

Condition represent predicate, which may be a true or false. Condition may contain many arguments.

```common-lisp
(defcond between (low-limit high-limit value)
  (and (>= value low-limit) (<= value high-limit value)))
```
### 3. Define your actions

Action - arbitrary kind of code. Any actions may be linked with rule. Action called only if rule is true.

```common-lisp
(defaction pay (amount)
  (decf *user-balance* amount))
  
(defaction report ()
  (print (format nil "The balance is ~d" *user-balance*)))
```

### 4. Define your rules!!!

Rules consists of several conditions and actions. Each rule may be only true or false. On this stage exists two ways:

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

### Time for run your rules

Rules running with this command: `(fire-rule 'mini-tariff 'base-tariff 'super-tariff)`, but in this case, actions not called. `fire-rule` - only return a logic value, true - if all rules is true and false - otherwise.

If you want call actions, using this: `(eval-rule 'mini-tariff 'base-tariff 'super-tariff)`. After this command `*user-balance*` will be less.

More examples in [tests](https://github.com/Dimercel/cl-rules/tree/master/t).

## Installation

## Author

* Ito Dimercel (xolcman@gmail.com)

## Copyright

Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)

## License

Licensed under the GPL-3.0 License.
