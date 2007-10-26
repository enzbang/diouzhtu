/*

Lisp definition (c) Olivier Ramonat <enzbang@ramonat.fr>

*/

LANGUAGES.elisp = {
  case_insensitive: false,
  defaultMode: {
    lexems: ['[a-zA-Z][a-zA-Z0-9_-]*'],
    contains: ['comment', 'string'],
    keywords: { 'not' : 1, 'defun' : 1, 'princ' : 1,
       'eval' : 1,'apply' : 1,'funcall' : 1,'quote' : 1,'identity' : 1,
       'function' : 1, 'complement' : 1,'backquote' : 1,'lambda' : 1,
       'set' : 1,'setq' : 1,'setf' : 1, 'defun' : 1,'defmacro' : 1,
       'gensym' : 1,'make' : 1,'symbol' : 1,'intern' : 1,
       'symbol' : 1,'name' : 1,'symbol' : 1,'value' : 1,'symbol' : 1,
       'plist' : 1,'get' : 1, 'getf' : 1,'putprop' : 1,'remprop' : 1,
       'hash' : 1,'make' : 1,'array' : 1,'aref' : 1, 'car' : 1,'cdr' : 1,
       'caar' : 1,'cadr' : 1,'cdar' : 1,'cddr' : 1,'caaar' : 1,
       'caadr' : 1,'cadar' : 1, 'caddr' : 1,'cdaar' : 1,'cdadr' : 1,
       'cddar' : 1,'cdddr' : 1,'caaaar' : 1,'caaadr' : 1,
       'caadar' : 1,'caaddr' : 1,'cadaar' : 1,'cadadr' : 1,
       'caddar' : 1,'cadddr' : 1, 'cdaaar' : 1,'cdaadr' : 1,
       'cdadar' : 1,'cdaddr' : 1,'cddaar' : 1,'cddadr' : 1,
       'cdddar' : 1,'cddddr' : 1,'cons' : 1,'list' : 1,'append' : 1,
       'reverse' : 1,'last' : 1,'nth' : 1, 'nthcdr' : 1,'member' : 1,
       'assoc' : 1,'subst' : 1,'sublis' : 1,'nsubst' : 1,
       'nsublis' : 1,'remove' : 1,'length' : 1,'list' : 1,'length' : 1,
       'mapc' : 1,'mapcar' : 1,'mapl' : 1,'maplist' : 1,'mapcan' : 1,
       'mapcon' : 1,'rplaca' : 1, 'rplacd' : 1,'nconc' : 1,'delete' : 1,
       'atom' : 1,'symbolp' : 1,'numberp' : 1, 'boundp' : 1,'null' : 1,
       'listp' : 1,'consp' : 1,'minusp' : 1,'zerop' : 1,'plusp' : 1,
       'evenp' : 1,'oddp' : 1,'eq' : 1,'eql' : 1,'equal' : 1,'cond' : 1,
       'case' : 1,'and' : 1,'or' : 1, 'let' : 1,'l' : 1,'if' : 1,
       'prog' : 1,'prog1' : 1,'prog2' : 1,'progn' : 1,'go' : 1,'return' : 1,
       'do' : 1,'dolist' : 1,'dotimes' : 1,'catch' : 1,'throw' : 1,
       'error' : 1,'cerror' : 1,'break' : 1, 'continue' : 1,
       'errset' : 1,'baktrace' : 1,'evalhook' : 1,'truncate' : 1,
       'float' : 1, 'rem' : 1,'min' : 1,'max' : 1,'abs' : 1,'sin' : 1,
       'cos' : 1,'tan' : 1,'expt' : 1,'exp' : 1,'sqrt' : 1,
       'random' : 1,'logand' : 1,'logior' : 1,'logxor' : 1,
       'lognot' : 1,'bignums' : 1,'logeqv' : 1, 'lognand' : 1,
       'lognor' : 1,'logorc2' : 1,'logtest' : 1,'logbitp' : 1,'logcount' : 1,
       'integer' : 1,'length' : 1,'nil' : 1}
  },
  modes: [
    {
      className: 'comment',
      begin: ';', end: '$'
    },
    {
      className: 'string-inside',
      relevance: 0,
      begin: '\\\\"', end: '\\\\"'
    },
    {
      className: 'string',
      contains: ['string-inside'],
      relevance: 0,
      begin: '"', end: '"'
    }

  ]
};
