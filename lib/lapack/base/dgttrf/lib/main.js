'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgttrf = require( './dgttrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgttrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgttrf;
