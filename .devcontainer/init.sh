# For things that needs the mounts in place (i.e., that can't be in the Dockerfile)
echo "Running init script"

set -ex

# Install caveman
( cd / && curl -fsSL https://raw.githubusercontent.com/JuliusBrussee/caveman/main/install.sh | bash )
