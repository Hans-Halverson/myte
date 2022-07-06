# Common tools shared across all images
FROM public.ecr.aws/lambda/nodejs:14 AS base

RUN yum -y groupinstall "Development Tools"
RUN yum -y install clang


# Image that builds myte from source
FROM base AS builds

# Install bubblewrap (dependency of opam)
RUN curl -o bubblewrap.rpm https://download-ib01.fedoraproject.org/pub/epel/7/x86_64/Packages/b/bubblewrap-0.3.0-1.el7.x86_64.rpm &&\
    rpm -Uvh bubblewrap.rpm &&\
    yum -y install bubblewrap

# Install opam
RUN bash -c "echo -ne '\n' | sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

# Install opam dependencies
COPY ./myte.opam /myte/myte.opam
RUN opam init --disable-sandboxing &&\
    opam switch create 4.09.1 &&\
    opam install /myte --deps-only --yes

# Build myte
COPY . /myte
RUN rm -rf /myte/_build /myte/build && /myte/scripts/build


# Image used in the lambda
FROM base AS lambda

RUN npm install

# Copy server files
COPY --from=builds myte/playground/src/server/*.js ${LAMBDA_TASK_ROOT}

# Include built myte binary and lib
COPY --from=builds myte/_build/default/src/myte.exe ${LAMBDA_TASK_ROOT}/myte
COPY --from=builds myte/build/lib ${LAMBDA_TASK_ROOT}/lib

# Include server dependencies (system assembler and linker along with their shared libraries,
# as they are not included in base amazonlinux image)
COPY --from=builds myte/playground/system-deps ${LAMBDA_TASK_ROOT}/system-deps

ENV MYTEPATH=${LAMBDA_TASK_ROOT}

CMD [ "lambda.handler" ]