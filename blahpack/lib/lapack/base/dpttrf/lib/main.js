'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpttrf = require( './dpttrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpttrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpttrf;
