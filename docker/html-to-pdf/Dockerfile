FROM node:22-slim

RUN apt-get update && apt-get install -y chromium fonts-liberation fonts-noto fonts-noto-cjk python3-pip --no-install-recommends \
 && pip3 install pikepdf --break-system-packages \
 && rm -rf /var/lib/apt/lists/* \

WORKDIR /app

# Install Puppeteer, telling it not to download its own Chromium
# (we use the system one instead)
COPY package.json .
RUN npm install

COPY convert.mjs fix_links.py /app/

CMD ["node", "/app/convert.mjs"]
