#FROM ubuntu
FROM archlinux
LABEL authors="unruh"
RUN pacman --noconfirm -Sy maxima git base-devel php composer
RUN git clone --filter blob:none https://github.com/dominique-unruh/moodle-qtype_stack-for-parsing.git moodle-qtype_stack && \
    cd moodle-qtype_stack && git checkout 4a656c57bd7fe83e0d88e9bbfd3b4dd9aed3578b
COPY parse.sh /
RUN cd /moodle-qtype_stack/api && composer install
#RUN cd /moodle-qtype_stack/api/public && php init.php
