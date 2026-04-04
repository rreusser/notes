
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgbsvx = require( './zgbsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbsvx;
