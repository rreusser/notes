

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpotrf2 = require( './dpotrf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpotrf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpotrf2;
