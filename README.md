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

```scheme
json-string->scm (string #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
Parse a JSON string into scheme expression. It optionally takes hooks
for various JSON data-types. Hooks are described in a separate section.


```scheme
json-port->scm (port #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
A wrapper around json-string->scm which reads takes a port called
`port' for JSON string and outputs scheme expression. optional hook
arguments are explained in a sepratate secion.


```scheme
json-file->scm (filename #:optional #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
A file-wrapper around json-string->scm function that takes a filename
as argument and outputs scheme expressions that correspond to the JSON
data in given file. Optional hook arguments are explain in a separate section.

```scheme
scm->json-file (filename code)
```

```scheme
scm->json-port (port code)
```

```scheme
scm->json-string (val)
```
