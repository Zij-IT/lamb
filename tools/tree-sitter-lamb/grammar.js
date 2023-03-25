const PREC = [
  "func_def",
  "apply",
  "compose",
  "logical_and",
  "logical_or",
  "comparisons",
  "bin_or",
  "bin_xor",
  "bin_and",
  "bin_shifts",
  "add",
  "mul",
  "unary",
  "call",
].reduce((acc, key, idx) => { acc[key] = idx; return acc; }, {});

module.exports = grammar({
  name: 'lamb',
  extras: $ => [
    $.line_comment,
    /[\s\f\uFEFF\u2060\u200B]|\\\r?\n/
  ],
  rules: {
    // Rules    
    file: $ => choice(
      repeat($._statement),
    ),
    
    _statement: $ => choice(
      $.statement_assign,
      $.statement_expr,
      $.statement_return,
    ),
    
    statement_assign: $ => seq($.identifier, ":=", $._expression, ';'),
    statement_expr:   $ => seq($._expression, ';'),
    statement_return: $ => seq("return", optional($._expression), ';'),
    
    _expression: $ => choice(
      $.identifier,
      $._literal,
      $.unary_expression,
      $.binary_expression,
      $.function_call,
      $.index,
      $.if_expr,
      $.case_expr,
      $.block,
      $.grouped,
      $.array,
      $.function_def,
    ),
    
    _literal: $ => choice(
      $.number_literal,
      $.string_literal,
      $.char_literal,
      $.bool_literal,
      $.nil_literal,
    ),
    
    unary_expression: $ => prec.right(PREC.unary, seq(
      choice(
        '-',
        '~',
        '!',
      ),
      $._expression,
    )),
    
    binary_expression: $ => {
      const binary_operators = [
        ["<$", PREC.apply],
        ["$>", PREC.apply],
        ["<.", PREC.compose],
        [".>", PREC.compose],
        ["&&", PREC.logical_and],
        ["||"     , PREC.logical_or],
        ["=", PREC.comparisons],
        [">=", PREC.comparisons],
        [">", PREC.comparisons],
        ["<=", PREC.comparisons],
        ["<", PREC.comparisons],
        ["!=", PREC.comparisons],
        ["|", PREC.bin_or],
        ["^", PREC.bin_xor],
        ["&", PREC.bin_and],
        ["<<", PREC.bin_shifts],
        [">>", PREC.bin_shifts],
        ["+", PREC.add],
        ["-", PREC.add],
        ["*", PREC.mul],
        ["%", PREC.mul],
        ["/", PREC.mul],
      ];
      
      return choice(
        ...binary_operators.map(([op, prec_strength]) => prec.left(prec_strength, seq($._expression, op, $._expression)))
      );
    },
    
    function_def: $ => prec.left(PREC.func_def, seq(
      optional("rec"),
      "fn",
      '(',
      optional(seq(comma_sep_req_one($._expression), optional(','))),
      ')',
      '->',
      $._expression,
    )),
    
    function_call: $ => prec.right(PREC.call, seq(
      $._expression,
      $._args_list,
    )),
    
    _args_list: $ => seq(
      '(',
      optional(comma_sep_req_one($._expression)),
      optional(','),
      ')',
    ),
    
    array: $ => seq(
      '[',
      optional(comma_sep_req_one($._expression)),
      optional(','),
      ']',
    ),
    
    index: $ => prec.right(PREC.call, seq(
      $._expression,
      '[',
      $._expression,
      ']',
    )),
    
    if_expr: $ => seq(
      "if",
      $._expression,
      $.block,
      optional($.elif),
    ),
    
    elif: $ => choice(
      seq(
        "elif",
        $._expression,
        $.block,
        optional($.elif),
      ), 
      $.els
    ),
    
    els: $ => seq(
      "else",
      $.block,
    ),
    
    block: $ => seq(
      '{',
      repeat($._statement),
      optional($._expression),
      '}',
    ),
    
    case_expr: $ => seq(
      "case",
      $._expression,
      '{',
      repeat($.case_arm),
      '}',
    ),
    
    case_arm: $ => seq(
      choice($._literal, $.identifier),
      '->',
      choice(
        seq($.block),
        seq($._expression, ',')
      ),
    ),
    
    grouped: $ => seq(
      '(',
      $._expression,
      ')',
    ),
    
    _string_literal: $ => seq(
      '"',
      repeat(
        choice(
          token.immediate(prec(1, /[^:"\n]+/)),
          seq(':', choice('n', '"', ':',)),
        )
      ),
      '"',
    ),
    
    _char_literal: $ => seq(
      "'",
      choice(
        token.immediate(prec(1, /[^:'\n]+/)),
        seq(':', choice('n', "'", ':',)),
      ),
      "'",
    ),
    
    // Tokens
    number_literal:    $ => choice(seq(choice('0b'), repeat1(/[01]+/)), seq(choice('0o'), repeat1(/[0-7]+/)), seq(choice('0h'), repeat1(/[0-9abcdefABCDEF]+/)), seq(choice('0d'), repeat1(/[0-9]+/)), /[1-9][0-9]*|0/),
    string_literal:    $ => $._string_literal,
    bool_literal:      $ => choice("true", "false"),
    char_literal:      $ => $._char_literal,
    nil_literal:       $ => "nil",
    identifier:        $ => /[_a-zA-Z][_a-zA-Z0-9]*/,

    // Extras
    line_comment: $ => token(seq("--", /.*/)),
  }
});

function comma_sep_req_one(rule) {
  return sep_req_one(rule, ',');
}

function sep_req_one(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
