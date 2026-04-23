'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlapmt = require( './zlapmt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlapmt, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlapmt;
