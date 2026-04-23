'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrttp = require( './ztrttp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrttp, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrttp;
