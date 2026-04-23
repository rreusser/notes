'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dhseqr = require( './dhseqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dhseqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dhseqr;
