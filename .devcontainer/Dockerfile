FROM mcr.microsoft.com/devcontainers/base:trixie
RUN apt update
RUN apt install -y nodejs npm plocate file git ripgrep
RUN npm install -g @anthropic-ai/claude-code
RUN sudo npm install -g opencode-ai

USER vscode

# Install uv
RUN curl -LsSf https://astral.sh/uv/install.sh | sh

RUN sudo updatedb

RUN ln -s ~/.claude/link-me-claude.json ~/.claude.json

COPY init.sh /home/vscode

