

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpotrf = require( './dpotrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpotrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpotrf;
