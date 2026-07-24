FROM debian:stable

ARG USERNAME=user
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# ca-certificates + curl are needed by the uv installer, sudo by the RUNs below and by init.sh.
# JDK 21 is the newest LTS that both sbt 1.10.1 and Play 3.0 support.
RUN apt update \
	&& apt install -y --no-install-recommends \
		ca-certificates curl sudo git less procps \
		nodejs npm plocate file ripgrep \
		openjdk-21-jdk-headless \
		python3 libpython3-dev python3-sympy \
	&& rm -rf /var/lib/apt/lists/*

# The MS devcontainer base image provided this user; plain Debian does not.
RUN groupadd --gid $USER_GID $USERNAME \
	&& useradd --uid $USER_UID --gid $USER_GID --create-home --shell /bin/bash $USERNAME \
	&& echo "$USERNAME ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/$USERNAME \
	&& chmod 0440 /etc/sudoers.d/$USERNAME

RUN npm install -g @anthropic-ai/claude-code opencode-ai

ENV JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64

# sbt version matches project/build.properties. (The launcher bootstraps whatever
# version a project asks for anyway, so this is only the default.)
ARG SBT_VERSION=1.10.1
RUN curl -fsSL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
		| tar -xz -C /opt \
	&& ln -s /opt/sbt/bin/sbt /usr/local/bin/sbt

# Coursier, for `cs install scala scalac scala-cli` etc. The Scala 3.6.4 compiler
# itself is resolved by sbt from build.sbt, so it is not installed here.
RUN curl -fsSL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" \
		| gzip -d > /usr/local/bin/cs \
	&& chmod 0755 /usr/local/bin/cs

USER user

# Install uv
RUN curl -LsSf https://astral.sh/uv/install.sh | sh

RUN sudo updatedb

RUN ln -s ~/.claude/link-me-claude.json ~/.claude.json

COPY --chown=user:user init.sh /home/user
