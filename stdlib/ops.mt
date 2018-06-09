package std::ops

trait Indexed<a> {
    sig __index(int): a
}

trait IndexedAssignment<a> {
    sig __indexedAssignment(int, a): a
}
