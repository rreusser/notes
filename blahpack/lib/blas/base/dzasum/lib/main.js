'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dzasum = require( './dzasum.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dzasum, 'ndarray', ndarray );


// EXPORTS //

module.exports = dzasum;
