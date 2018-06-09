package std::bool

implement bool {
    def toString(): string = match this
        | true -> "true"
        | false -> "false"    
}
