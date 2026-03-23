

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgesvd = require( './zgesvd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgesvd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgesvd;
