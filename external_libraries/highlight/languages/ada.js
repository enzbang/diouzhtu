/*

Ada definition (c) Olivier Ramonat <enzbang@ramonat.fr>

*/

LANGUAGES.ada = {
  case_insensitive: true,
  defaultMode: {
    lexems: [UNDERSCORE_IDENT_RE],
    contains: ['tag', 'comment', 'string', 'attribute'],
    keywords: {
          'abort' : 1, 'else' : 1, 'new' : 1, 'return' : 1,
          'abs' : 1, 'elsif' : 1, 'not' : 1, 'reverse' : 1,
          'abstract' : 1, 'end' : 1, 'null' : 1,
          'accept' : 1, 'entry' : 1, 'select' : 1,
          'access' : 1, 'exception' : 1, 'of' : 1, 'separate' : 1,
          'aliased' : 1, 'exit' : 1, 'or' : 1, 'subtype' : 1,
          'all' : 1, 'others' : 1, 'synchronized' : 1,
          'and' : 1, 'for' : 1, 'out' : 1,
          'array' : 1, 'function' : 1, 'overriding' : 1, 'tagged' : 1,
          'at' : 1, 'task' : 1, 'generic' : 1,  'terminate' : 1,
          'begin' : 1, 'goto' : 1, 'pragma' : 1, 'then' : 1,
          'body' : 1, 'private' : 1, 'type' : 1,
          'if' : 1, 'procedure' : 1, 'package': 1,
          'case' : 1, 'in' : 1, 'protected' : 1, 'until' : 1,
          'constant' : 1, 'interface' : 1, 'use' : 1,
          'is' : 1, 'raise' : 1,
          'declare' : 1, 'range' : 1, 'when' : 1,
          'delay' : 1, 'limited' : 1, 'record' : 1, 'while' : 1,
          'delta' : 1, 'loop' : 1, 'rem' : 1, 'with' : 1,
          'digits' : 1, 'renames' : 1,
          'do' : 1, 'mod' : 1, 'requeue' : 1, 'xor' : 1}
  },
  modes: [
    {
      className: 'comment',
      begin: '--', end: '$'
    },
    {
       className: 'attribute',
       begin: '\'[a-zA-Z]+', end: '^',
       illegal: ' '
    },
    APOS_STRING_MODE,
    QUOTE_STRING_MODE,
    {
      className: 'title',
      illegal: '[^\\(]',
      begin: UNDERSCORE_IDENT_RE, end: '^'
    }
  ]
};
