'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsysvx = require( './dsysvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsysvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsysvx;
