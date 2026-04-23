
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtgex2 = require( './dtgex2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtgex2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtgex2;
