package std::result

type Result<ok, err> =
    | Ok(ok)
    | Err(err)
