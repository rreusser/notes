'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlanv2 = require( './dlanv2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlanv2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlanv2;
