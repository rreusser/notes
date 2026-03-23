

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgetrf2 = require( './dgetrf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgetrf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgetrf2;
