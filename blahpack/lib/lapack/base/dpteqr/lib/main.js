
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpteqr = require( './dpteqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpteqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpteqr;
