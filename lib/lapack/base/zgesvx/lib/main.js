'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgesvx = require( './zgesvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgesvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgesvx;
