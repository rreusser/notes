'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgttrf = require( './zgttrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgttrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgttrf;
