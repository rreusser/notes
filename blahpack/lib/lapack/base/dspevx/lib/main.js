
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dspevx = require( './dspevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dspevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dspevx;
