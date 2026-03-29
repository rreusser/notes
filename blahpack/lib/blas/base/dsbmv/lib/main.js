
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsbmv = require( './dsbmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsbmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsbmv;
