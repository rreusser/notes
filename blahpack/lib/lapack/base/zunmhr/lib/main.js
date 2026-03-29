'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunmhr = require( './zunmhr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmhr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmhr;
