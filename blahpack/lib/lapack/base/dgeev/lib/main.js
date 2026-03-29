'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgeev = require( './dgeev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeev, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeev;
