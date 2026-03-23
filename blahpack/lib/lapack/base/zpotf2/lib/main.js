

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zpotf2 = require( './zpotf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpotf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpotf2;
