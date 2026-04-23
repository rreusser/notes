
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsbev = require( './dsbev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsbev, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsbev;
