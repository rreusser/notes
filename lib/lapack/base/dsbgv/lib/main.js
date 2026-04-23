
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsbgv = require( './dsbgv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsbgv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsbgv;
