
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlagv2 = require( './dlagv2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlagv2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlagv2;
