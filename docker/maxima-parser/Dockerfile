FROM archlinux
LABEL authors="unruh"
RUN pacman --noconfirm -Sy maxima git base-devel
RUN git clone --filter blob:none https://github.com/maths/moodle-qtype_stack.git
#COPY parse.sh /
COPY init.sh /
COPY parse.mac /
COPY expression_to_json.mac /
RUN sh -c /init.sh
