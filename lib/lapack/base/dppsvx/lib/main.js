
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dppsvx = require( './dppsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dppsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dppsvx;
