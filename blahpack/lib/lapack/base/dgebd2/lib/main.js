

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgebd2 = require( './dgebd2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgebd2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgebd2;
