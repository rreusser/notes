'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunmr3 = require( './zunmr3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmr3, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmr3;
