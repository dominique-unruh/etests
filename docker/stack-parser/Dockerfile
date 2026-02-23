#FROM ubuntu
FROM archlinux
LABEL authors="unruh"
RUN pacman --noconfirm -Sy maxima git base-devel php composer
RUN git clone --filter blob:none https://github.com/dominique-unruh/moodle-qtype_stack-for-parsing.git moodle-qtype_stack && \
    cd moodle-qtype_stack && git checkout 7f8c64ee77cb3143c05a0015e2b7fea35d4aa276
COPY parse.sh /
RUN cd /moodle-qtype_stack/api && composer install
RUN cd /moodle-qtype_stack/api/public && php init.php
