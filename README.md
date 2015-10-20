# prettybraces

Pretty-Braces parses, and then pretty-prints strings with balanced braces.

RUN

    Usage: prettybraces [-b|--braces ARG] [FILES...]
      Also accepts input on STDIN.

    Available options:
      -h,--help                Show this help text
      -b,--braces ARG          Space-separated, comma-separated pairs of braces,
                               e.g. '(,) {,} <,>'.

IN

    {hello world ((whatever [you like]) can go) here} ...


OUT

    {
      hello world 
      (
        (
          whatever 
          [
            you like
          ]
        )
         can go
      )
       here
    }
     ...

# Binaries

* <http://sordina.binaries.s3.amazonaws.com/prettybraces-0.1.0.0-MacOSX-10.9.5-13F1077.zip>
* <http://sordina.binaries.s3.amazonaws.com/prettybraces-0.1.0.0-MacOSX-10.9.5-13F1077.zip>

