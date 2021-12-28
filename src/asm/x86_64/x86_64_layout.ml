open Mir_type

module AggregateElement = struct
  type t = {
    (* Offset of this element from the beginning of the aggregate *)
    offset: int;
    (* Total size of the element *)
    size: int;
  }
end

module AggregateLayout = struct
  type t = {
    agg: Aggregate.t;
    size: int;
    alignment: int;
    elements: AggregateElement.t array;
  }

  let get_element agg_layout i = agg_layout.elements.(i)
end
