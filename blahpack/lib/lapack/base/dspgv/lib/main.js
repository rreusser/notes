
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dspgv = require( './dspgv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dspgv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dspgv;
