# Common tools shared across all images
FROM public.ecr.aws/lambda/nodejs:14 AS base

RUN yum -y groupinstall "Development Tools"
RUN yum -y install clang


# Image that builds myte from source
FROM base AS builds

# Install opam
RUN bash -c "echo -ne '\n' | sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

# Install opam dependencies
COPY ./myte.opam /myte/myte.opam
COPY ./scripts/install /myte/scripts/install
RUN /myte/scripts/install --disable-sandboxing

# Build myte
COPY . /myte
RUN /myte/scripts/build


# Image used in the lambda
FROM base AS lambda

RUN npm install

# Copy server files
COPY --from=builds myte/playground/src/server/*.js ${LAMBDA_TASK_ROOT}

# Include built myte binary and lib
COPY --from=builds myte/_build/default/src/myte.exe ${LAMBDA_TASK_ROOT}/myte
COPY --from=builds myte/build/lib ${LAMBDA_TASK_ROOT}/lib

ENV MYTEPATH=${LAMBDA_TASK_ROOT}

CMD [ "lambda.handler" ]