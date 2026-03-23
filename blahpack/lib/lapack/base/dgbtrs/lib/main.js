

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgbtrs = require( './dgbtrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbtrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbtrs;
