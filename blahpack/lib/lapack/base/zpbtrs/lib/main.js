

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zpbtrs = require( './zpbtrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpbtrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpbtrs;
