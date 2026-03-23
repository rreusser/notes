

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpotrs = require( './dpotrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpotrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpotrs;
