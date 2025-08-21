
set -e

TODO REMOVE FILE

maxima <<EOT
file_search_maxima : append(["/moodle-qtype_stack/stack/maxima/###.mac"], file_search_maxima)$
load("stackmaxima.mac")$
stream: openr("expression.txt")$
simp: true$
expr_string: readline(stream)$
expression: parse_string(expr_string)$
load("/expression_to_json.mac")$
term: expr_to_json(expression)$;
stringout("result.txt", term)$
EOT
