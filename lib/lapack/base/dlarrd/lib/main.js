

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarrd = require( './dlarrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarrd;
