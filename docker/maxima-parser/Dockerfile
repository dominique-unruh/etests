#FROM ubuntu
FROM archlinux
LABEL authors="unruh"
RUN pacman --noconfirm -Sy maxima git base-devel
#RUN apt update
#RUN apt install -y maxima git build-essential

### **** NOT WORKING: Use the right branch/revision
RUN git clone --filter blob:none https://github.com/maths/moodle-qtype_stack.git
COPY init.sh /
COPY parse.mac /
COPY expression_to_json.mac /
# TODO: Instead of init.sh, call "maxima -b /init.mac" for suitable init.mac 
RUN sh -c /init.sh
