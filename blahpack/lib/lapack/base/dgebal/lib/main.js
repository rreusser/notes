'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgebal = require( './dgebal.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgebal, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgebal;
