
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpteqr = require( './zpteqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpteqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpteqr;
