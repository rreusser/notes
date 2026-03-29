'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgesvx = require( './dgesvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgesvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgesvx;
