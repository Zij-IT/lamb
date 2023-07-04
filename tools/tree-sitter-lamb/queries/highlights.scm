(line_comment) @comment.line

(string_literal
  (escape_sequence) @constant.character.escape
  )

(char_literal 
  (escape_sequence) @constant.character.escape
  )

(function_call
  (identifier) @function
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
  "return"
] @keyword.control.return

[
  "case"
  "elif"
  "else"
  "if"
] @keyword.control.conditional

[
  "fn"
] @keyword.function

[
  "rec"
] @keyword.storage.modifier

[
  (string_literal)
] @string

[
  (char_literal)
] @constant.character

[
  (bool_literal)
] @constant.builtin.boolean

[
  (number_literal)
] @constant.numeric

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ","
  ";"
  "->"
] @punctuation.delimeter

