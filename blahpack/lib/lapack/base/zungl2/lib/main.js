

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zungl2 = require( './zungl2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zungl2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zungl2;
