
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dptsvx = require( './dptsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dptsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dptsvx;
