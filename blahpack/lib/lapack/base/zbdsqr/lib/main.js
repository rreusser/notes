

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zbdsqr = require( './zbdsqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zbdsqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zbdsqr;
