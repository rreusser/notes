
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlahefRk = require( './zlahef_rk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlahefRk, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlahefRk;
