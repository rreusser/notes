

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zpotri = require( './zpotri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpotri, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpotri;
