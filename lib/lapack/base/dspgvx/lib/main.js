
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dspgvx = require( './dspgvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dspgvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dspgvx;
