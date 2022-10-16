FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && apt-get install -y \
  clang \
  curl \
  gcc-aarch64-linux-gnu \
  make \
  qemu-user

# Install opam and its system dependencies
RUN apt-get install -y bzip2 patch unzip
RUN bash -c "echo -ne '\n' | sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

# Install opam dependencies
COPY ./myte.opam /myte/myte.opam
COPY ./scripts/install /myte/scripts/install
RUN /myte/scripts/install --disable-sandboxing

WORKDIR /myte

CMD [ "bash" ]