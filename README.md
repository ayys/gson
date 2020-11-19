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
procedure json-string->scm (string #:key number-hook nil-hook
    list-hook object-hook string-hook boolean-hook)
```
