'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgerq2 = require( './dgerq2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgerq2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgerq2;
