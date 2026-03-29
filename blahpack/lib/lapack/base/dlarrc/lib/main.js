'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarrc = require( './dlarrc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarrc, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarrc;
