

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpbtrs = require( './dpbtrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbtrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbtrs;
