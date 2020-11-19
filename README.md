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
```scheme
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
git clone https://github.com/ayys/gson.git
cd gson
sudo make install
```

## Documentation

GSON module exports size procedures in total. The definition and usage
examples of each of the procedures is given below.

### json-string->scm

```scheme
json-string->scm (string #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
Parse a JSON string into scheme expression. It optionally takes hooks
for various JSON data-types. Hooks are described in a [separate section](#hooks).

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
```scheme
#(1 2 3)
```

### json-port->scm

```scheme
json-port->scm (port #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
A wrapper around [json-string->scm](#json-string-scm) which reads takes a port called
`port' for JSON string and outputs scheme expression. optional hook
arguments are explained in a [separate section](#hooks).

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
```scheme
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
A file-wrapper around  [json-string->scm](#json-string-scm) procedure that takes a filename
as argument and outputs scheme expressions that correspond to the JSON
data in given file. Optional hook arguments are explain in a [separate section](#hooks).
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
```scheme
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
Takes scheme expression as `val`, and converts it to JSON expression. For
conversion datatypes, refer to [default conversion format](#default-conversion-format).

#### Example

**sample.scm**
```scheme
;;; Import gson
(use-modules (gson))

;;; define code to be a string containing json
(define code "#(1 #f 3)")

;;; Convert JSON string to scheme expressions
(display (scm->json-string code))
```

##### Output
```scheme
[1, false, 3]
```

### scm->json-port

```scheme
scm->json-port (port code)
```
This is a wrapper around [scm->json-string](#scm-json-string) which
takes in an input port, reads scm data from it and converts it to JSON
string. For more details on conversion, refer to [default conversion format](#default-conversion-format).

### scm->json-file

```scheme
scm->json-file (filename code)
```
This is a wrapper around [scm->json-string](#scm-json-string) which
takes in an input filename, reads scm data from it and converts it to JSON string.
For more details on conversion, refer to [default conversion format](#default-conversion-format).

### Hooks
gson hooks are optional keyword arguments sent to
[json-string->scm](#json-string-scm),
[json-port->scm](#json-port-scm), [json-file->scm](#json-file-scm)
procedures which are then applied to the scheme expressions parsed from
the input JSON. Hooks let the user transform the output of the parser.

By default, JSON data is converted into scheme expressions based on
the [default conversion format](#default-conversion-format). You can
change this via hooks.

#### Example of json-string->scm with hooks

```scheme
;;; Import gson
(use-modules (gson))

;;; define code to be a string containing json
(define code "[4, \"two\", false]")

;;; Convert JSON string to scheme expressions
(display
    (json-string->scm
        code
            #:number-hook (lambda (num-or-string) (1+ num-or-string))
            #:string-hook (lambda (string) (string-length string))
            #:boolean-hook (lambda (bool) (not bool))))
```

##### Output
```scheme
#(5 3 #t)
```

#### object-hook
object-hook is an optional procedure that will be called with the
result of any JSON objects decoded (a [List](https://www.gnu.org/software/guile/manual/html_node/Lists.html)). The return value of the
object-hook will be used instead of the
[list](https://www.gnu.org/software/guile/manual/html_node/Lists.html).

object-hook takes in a list as argument and produces any output.

##### Example
```scheme
(lambda (lst)
    (list->vector lst))
```

#### list-hook
list-hook is another optional procedure that will be called on the
result of any JSON lists decoded (a
[Vector](https://www.gnu.org/software/guile/manual/html_node/Vectors.html)). The
resulting value will be used in place of the vector.

object-hook takes in a vector as argument and produces any output.

##### Example
```scheme
(lambda (vector)
    (vector->list lst))
```

#### number-hook
number-hook is a optional procedure that is called on any
[numbers](https://www.gnu.org/software/guile/manual/html_node/Numbers.html)
or
[strings](https://www.gnu.org/software/guile/manual/html_node/Strings.html)
encountered while decoding JSON data. The resulting value
is used in place of the number.

object-hook takes in a number/string as argument and produces any output.

##### Example
```scheme
(lambda (num-or-string)
    (if (number? num-or-string)
        (1+ num-or-string)
        0))
```

#### string-hook
string-hook, as the name suggests is called everything the gson parses
a
[string](https://www.gnu.org/software/guile/manual/html_node/Strings.html).
The resulting value is used in place of the string.

string-hook takes in a string as argument and produces any output.

##### Example
```scheme
(lambda (string)
    (string-length string))
```
The example above replaces string with the length of said string.

#### boolean-hook
boolean-hook is an optional procedure that will be called with the
result of any JSON objects decoded (a [boolean](https://www.gnu.org/software/guile/manual/html_node/Booleans.html#Booleans)). The return value of the
boolean-hook will be used instead of the [boolean](https://www.gnu.org/software/guile/manual/html_node/Booleans.html#Booleans).

boolean-hook takes in a boolean as argument and produces any output.

##### Example
```scheme
(lambda (bool)
    (not string))
```

#### nil-hook
By default, nil in JSON data maps to `#nil` in scheme expression. You
can change this with the nil-hook that is a procedure which is called
on all teh nil values in JSON data. The output of that procedure is
used instead of #nil.

##### Example
```scheme
(lambda (nil-value) #f)
```
This example shows a hook that maps nil value to `#f`.

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
Notice in the above example, the input JSON string contained syntax
error after the closing square-bracket. The error was thrown by gson
and caught by the user after which a clean error message was printed
to the default output.

### Default Conversion Format
| JSON Datatype | Guile Datatype    |
|---------------|-------------------|
| String        | String            |
| Number        | *Number or String |
| false         | #f                |
| true          | #t                |
| nil           | #nil              |
| List          | Vector            |
| Object        | **List of pairs   |

\* If a number is too large to be represented in Guile, it is
represented as a string.

\*\* The first element of pair is the key, second element is the value
for each property of JSON object.

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
