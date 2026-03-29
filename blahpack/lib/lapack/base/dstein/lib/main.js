'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dstein = require( './dstein.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dstein, 'ndarray', ndarray );


// EXPORTS //

module.exports = dstein;
