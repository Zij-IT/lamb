(line_comment) @comment

(function_call
  (identifier) @function.builtin
  "(")

(identifier) @variable

[
  "&&"
  "!"
  "||"
  "<$"
  "$>"
  ":="
  "&"
  "~"
  "|"
  "<<"
  ">>"
  "^"
  "<."
  ".>"
  "+"
  "-"
  "/"
  "%"
  "*"
  "="
  ">="
  ">"
  "<="
  "<"
  "!="
] @operator

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  "case"
  "elif"
  "else"
  "fn"
  "if"
  "rec"
  "return"
] @keyword

[
  (char_literal)
  (string_literal)
] @string

[
  ":"
  ","
  ";"
  "->"
] @punctuation.delimeter

[
  (bool_literal)
  (number_literal)
] @constant.builtin
