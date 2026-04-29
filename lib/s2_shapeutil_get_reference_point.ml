let get_reference_point (shape : S2_shape.t) =
  S2_shape.get_reference_point
    ~num_edges:shape.#num_edges
    ~num_chains:shape.#num_chains
    ~edge:shape.#edge
    ~chain:shape.#chain
;;
