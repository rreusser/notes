

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlapy2 = require( './dlapy2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlapy2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlapy2;
