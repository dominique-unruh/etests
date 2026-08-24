set -e
cd /moodle-qtype_stack/api/public
export WORKDIR=/workdir
php -d disable_functions=locale_lookup parseexpression.php
