'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpttrf = require( './zpttrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpttrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpttrf;
