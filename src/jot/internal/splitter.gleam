pub type Splitter

// TODO: make it gracefully handle empty list
// TODO: make it gracefully handle empty string

@external(erlang, "splitter_ffi", "new")
@external(javascript, "./splitter_ffi.mjs", "make")
pub fn new(patterns: List(String)) -> Splitter

@external(erlang, "splitter_ffi", "split")
@external(javascript, "./splitter_ffi.mjs", "split")
pub fn split(splitter: Splitter, string: String) -> #(String, String, String)
