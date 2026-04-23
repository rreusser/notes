'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunmr2 = require( './zunmr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmr2;
