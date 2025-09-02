#FROM ubuntu
FROM archlinux
LABEL authors="unruh"
RUN pacman --noconfirm -Sy maxima git base-devel php
#RUN apt update
#RUN apt install -y maxima git build-essential
RUN git clone --filter blob:none https://github.com/maths/moodle-qtype_stack.git
COPY stack.patch /
RUN cd /moodle-qtype_stack && patch -p1 </stack.patch
RUN cd /moodle-qtype_stack/api/public && php init.php
COPY parse.sh /
