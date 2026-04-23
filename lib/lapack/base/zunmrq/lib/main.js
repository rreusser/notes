'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunmrq = require( './zunmrq.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmrq, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmrq;
