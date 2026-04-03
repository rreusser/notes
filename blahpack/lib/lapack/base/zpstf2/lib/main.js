
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpstf2 = require( './zpstf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpstf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpstf2;
