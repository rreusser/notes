'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhseqr = require( './zhseqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhseqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhseqr;
