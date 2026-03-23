

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlacpy = require( './zlacpy.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlacpy, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlacpy;
