/**
 * @file CX grammar for tree-sitter
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const KEYWORDS = [
  'break',
  'case',
  'class',
  'const',
  'continue',
  'default',
  'defer',
  'do',
  'else',
  'enum',
  'extern',
  'for',
  'if',
  'import',
  'inline',
  'match',
  'post',
  'pre',
  'private',
  'public',
  'register',
  'restrict',
  'return',
  'safe',
  'sizeof',
  'static',
  'strong',
  'struct',
  'switch',
  'template',
  'type',
  'typedef',
  'union',
  'volatile',
  'weak',
  'where',
  'while',
];

const BUILTIN_TYPES = [
  'auto',
  'bool',
  'char',
  'double',
  'float',
  'int',
  'long',
  'short',
  'signed',
  'unsigned',
  'void',
];

module.exports = grammar({
  name: 'cx',

  extras: $ => [
    /[\s\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]+/,
    $.comment,
  ],

  word: $ => $.identifier,

  rules: {
    source_file: $ => repeat($._element),

    _element: $ => choice(
      $.block,
      $.parenthesized_group,
      $.bracket_group,
      $.preprocessor_directive,
      $.string_literal,
      $.char_literal,
      $.number_literal,
      $.compiler_identifier,
      alias(choice(...KEYWORDS), $.keyword),
      alias(choice(...BUILTIN_TYPES), $.builtin_type),
      $.identifier,
      $.operator,
      $.punctuation,
    ),

    block: $ => seq('{', repeat($._element), '}'),

    parenthesized_group: $ => seq('(', repeat($._element), ')'),

    bracket_group: $ => seq('[', repeat($._element), ']'),

    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*/,

    compiler_identifier: _ => /@[A-Za-z_][A-Za-z0-9_]*/,

    string_literal: _ => token(seq(
      '"',
      repeat(choice(
        /[^"\\\n]+/,
        /\\./,
      )),
      '"',
    )),

    char_literal: _ => token(seq(
      '\'',
      choice(
        /[^'\\\n]/,
        /\\./,
      ),
      '\'',
    )),

    number_literal: _ => token(choice(
      /0[xX][0-9a-fA-F]+/,
      /\d+\.\d+([eE][+-]?\d+)?/,
      /\d+[eE][+-]?\d+/,
      /\d+/,
    )),

    preprocessor_directive: _ => token(seq('#', /[^\n]*/)),

    operator: _ => token(choice(
      '::',
      '=>',
      '->',
      '==',
      '!=',
      '<=',
      '>=',
      '&&',
      '||',
      '<<',
      '>>',
      '++',
      '--',
      '..',
      '+',
      '-',
      '*',
      '/',
      '%',
      '=',
      '<',
      '>',
      '!',
      '&',
      '|',
      '^',
      '~',
    )),

    punctuation: _ => token(choice(
      ';',
      ',',
      ':',
      '.',
      '?',
    )),

    comment: _ => token(choice(
      seq('//', /[^\n]*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/',
      ),
    )),
  },
});
