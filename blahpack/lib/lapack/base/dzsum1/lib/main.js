'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dzsum1 = require( './dzsum1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dzsum1, 'ndarray', ndarray );


// EXPORTS //

module.exports = dzsum1;
