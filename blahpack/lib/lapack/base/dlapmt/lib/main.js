'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlapmt = require( './dlapmt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlapmt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlapmt;
