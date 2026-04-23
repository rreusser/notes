'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetri3 = require( './zhetri_3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetri3, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetri3;
