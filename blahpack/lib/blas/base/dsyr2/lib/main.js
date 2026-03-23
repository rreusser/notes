

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsyr2 = require( './dsyr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyr2;
