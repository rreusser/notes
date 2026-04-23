'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlacn2 = require( './zlacn2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlacn2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlacn2;
