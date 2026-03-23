

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgesvd = require( './dgesvd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgesvd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgesvd;
