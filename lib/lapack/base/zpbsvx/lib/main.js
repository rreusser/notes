
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpbsvx = require( './zpbsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpbsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpbsvx;
