

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpotri = require( './dpotri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpotri, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpotri;
