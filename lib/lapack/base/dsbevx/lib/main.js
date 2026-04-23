
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsbevx = require( './dsbevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsbevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsbevx;
