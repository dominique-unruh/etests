
set -e

maxima <<EOT
file_search_maxima : append(["/moodle-qtype_stack/stack/maxima/###.mac"], file_search_maxima)$
load("stackmaxima.mac")$
EOT
