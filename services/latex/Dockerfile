FROM docker.io/aergus/latex:latest
LABEL authors="unruh"
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends python3 && \
    rm -rf /var/lib/apt/lists/*
COPY server.py /
EXPOSE 8080
CMD ["python3", "/server.py"]
