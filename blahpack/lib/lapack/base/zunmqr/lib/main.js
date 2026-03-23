

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zunmqr = require( './zunmqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmqr;
