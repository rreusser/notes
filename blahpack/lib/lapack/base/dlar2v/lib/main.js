'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlar2v = require( './dlar2v.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlar2v, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlar2v;
