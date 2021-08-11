let print_errors errors =
  let errors = List.sort (fun (loc1, _) (loc2, _) -> Loc.compare loc1 loc2) errors in
  Printf.printf
    "%s"
    (String.concat
       "\n"
       (List.map
          (fun (loc, err) ->
            if loc <> Loc.none then
              Error_pp.pp loc err
            else
              Error_pp.print_message_line err)
          errors))

let print_error_message msg = print_string (Error_pp.print_message_line msg)
