

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpbtrf = require( './dpbtrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbtrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbtrf;
