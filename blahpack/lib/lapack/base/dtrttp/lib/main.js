'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrttp = require( './dtrttp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrttp, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrttp;
