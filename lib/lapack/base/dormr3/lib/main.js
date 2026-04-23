
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dormr3 = require( './dormr3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormr3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormr3;
