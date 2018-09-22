package std::io

import std::result::Result

type ReadError =
    | ReadIOError
    | ReadEOFReached

type WriteError = WriteIOError

trait Reader {
    sig read(vec<int>): Result<int, ReadError>
}

trait Writer {
    sig write(vec<int>): Result<int, WriteError>
}
