FROM docker.io/library/ubuntu:noble-20260810
LABEL authors="unruh"
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        git build-essential ca-certificates curl bzip2 \
        php-cli php-mbstring php-xml php-curl composer && \
    rm -rf /var/lib/apt/lists/*
# Maxima 5.44.0 from conda-forge (ECL backend). The apt maxima uses the gcl
# Lisp image, whose fixed-address unexec image segfaults under container ASLR.
ENV MAMBA_ROOT_PREFIX=/opt/micromamba
ENV PATH=/opt/micromamba/envs/maxima/bin:$PATH
RUN curl -Ls https://micro.mamba.pm/api/micromamba/linux-64/latest | tar -xj -C /usr/local bin/micromamba && \
    micromamba create -y -n maxima -c conda-forge maxima=5.44.0 && \
    micromamba clean -a -y
RUN git clone --filter blob:none https://github.com/dominique-unruh/moodle-qtype_stack-for-parsing.git moodle-qtype_stack && \
    cd moodle-qtype_stack && git checkout 2eaf41a699b799bc569cafa269ec186f6d3d5e0a
COPY parse.sh /
RUN cd /moodle-qtype_stack/api && composer install
RUN cd /moodle-qtype_stack/api/public && php  -d disable_functions=locale_lookup init.php
