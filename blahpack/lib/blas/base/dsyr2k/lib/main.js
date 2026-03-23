

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsyr2k = require( './dsyr2k.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyr2k, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyr2k;
