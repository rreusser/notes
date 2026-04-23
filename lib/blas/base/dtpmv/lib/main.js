
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtpmv = require( './dtpmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtpmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtpmv;
