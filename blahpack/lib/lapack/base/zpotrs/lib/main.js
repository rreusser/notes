

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zpotrs = require( './zpotrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpotrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpotrs;
