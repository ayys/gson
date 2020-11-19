# GSON

GSON is an easy-to-use JSON library for
[Guile](https://www.gnu.org/software/guile/ "GNU/Guile Homepage").

## Getting Started

Let's get started with a very simple example. This example does not
perform exception handling.
```scheme
;;; Import gson
(use-modules (gson))

;;; Define variable code which stores a JSON string
(define code
  "
{
    \"name\": \"John Doe\",
    \"age\": 43,
    \"address\": {
        \"street\": \"10 Downing Street\",
        \"city\": \"London\"
    },
    \"phones\": [
        \"+44 1234567\",
        \"+44 2345678\"
    ]
}")

;;; Print the scheme representation of above JSON
(display(json-string->scm code))
(newline)
```

#### Output

```
((name . John Doe)
 (age . 43)
 (address
   (street . 10 Downing Street)
   (city . London))
 (phones . #(+44 1234567 +44 2345678)))
```

## Installation

before installing, make sure you have Guile and make installed.
### Installing make on Debian / Ubuntu
```bash
sudo apt-get install build-essential
```

Once you've installed make, run the following command to install gson.
```bash
sudo make install
```

## Documentation

GSON module exports size functions in total. The definition and usage
examples of each of the functions is given below.

### json-string->scm

```scheme
json-string->scm (string #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
Parse a JSON string into scheme expression. It optionally takes hooks
for various JSON data-types. Hooks are described in a separate section.

#### Example

**sample.scm**
```scheme
;;; Import gson
(use-modules (gson))

;;; define code to be a string containing json
(define code "[1, 2, 3]")

;;; Convert JSON string to scheme expressions
(display (json-string->scm code))
```

##### Output
```
#(1 2 3)
```

### json-port->scm

```scheme
json-port->scm (port #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
A wrapper around [json-string->scm](#json-string-scm) which reads takes a port called
`port' for JSON string and outputs scheme expression. optional hook
arguments are explained in a sepratate secion.

#### Example

**sample.json**
```json
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}
```

**sample.scm**
```scheme
;;; Import gson
(use-modules (gson))

(call-with-input-file (cadr (command-line))
    (lambda (port) (display (json-string->scm code))))
```

##### Output
```
(("menu"
  ("id" . "file")
  ("value" . "File")
  ("popup"
   ("menuitem" . #((("value" . "New") ("onclick" . "CreateNewDoc()"))
                   (("value" . "Open") ("onclick" . "OpenDoc()"))
                   (("value" . "Close") ("onclick" . "CloseDoc()")))))))
```
### json-file->scm

```scheme
json-file->scm (filename #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
A file-wrapper around  [json-string->scm](#json-string-scm) function that takes a filename
as argument and outputs scheme expressions that correspond to the JSON
data in given file. Optional hook arguments are explain in a separate section.
#### Example

**sample.json**
```json
{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": {
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}
```

**sample.scm**
```scheme
;;; Import gson
(use-modules (gson))

;;; Convert JSON string to scheme expressions
(display (json-file->scm "sample.json"))
```

##### Output
```
(("widget"
  ("debug" . "on")
  ("window"
   ("title" . "Sample Konfabulator Widget")
   ("name" . "main_window")
   ("width" . 500)
   ("height" . 500))
  ("image"
   ("src" . "Images/Sun.png")
   ("name" . "sun1")
   ("hOffset". 250)
   ("vOffset" . 250)
   ("alignment" . "center"))
  ("text"
   ("data" . "Click Here")
   ("size" . 36)
   ("style" . "bold")
   ("name" . "text1")
   ("hOffset" . 250)
   ("vOffset". 100)
   ("alignment" . "center")
   ("onMouseUp" . "sun1.opacity = (sun1.opacity / 100) * 90;"))))
```


### scm->json-string

```scheme
scm->json-string (val)
```

### scm->json-port

```scheme
scm->json-port (port code)
```

### scm->json-file

```scheme
scm->json-file (filename code)
```

### Hooks

### Exception Handling
A GSON-JSON-INVALID exception is thrown if an error is found during
JSON parsing with a list of two elements as argument. The first
element of list contains line number and second element contains
column number wherein the syntax error occured.

Here is a simple example to demonstrate exception handling with gson.

```scheme
(use-modules
 (gson)
 (ice-9 format))

;;; Incorrect JSON - Notice the comma after closing square bracket
(define code "[1, 2],")

(define x (catch GSON-JSON-INVALID
            (lambda ()
              (json-string->scm code))
            (lambda (key . args)
              (let ((line (caar args))
                    (column (cadar args)))
                (format #t "Error: Syntax error on line ~d:~d\n" line column)
                #f))))

(if x (display x))
(newline)

```

##### Output
```
Error: Syntax error on line 0:6
```

### Default Conversion Format


## License
Copyright (C) 2020 Ayush Jha <ayushjha@pm.me>

gson is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

gson is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with gson. If not, see https://www.gnu.org/licenses/.
