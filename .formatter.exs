# Used by "mix format"
[
  # don't add parens around assert_value arguments
  import_deps: [:assert_value],
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"]
]
