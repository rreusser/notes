'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarscl2 = require( './dlarscl2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarscl2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarscl2;
